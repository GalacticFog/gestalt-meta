package controllers

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceIds
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{PatchOp, PatchDocument, PatchHandler}
import com.galacticfog.gestalt.meta.api.GestaltResourceInput
import com.galacticfog.gestalt.meta.api.output._

import com.galacticfog.gestalt.meta.api.rte
import com.galacticfog.gestalt.meta.api.ResourceNotFoundException
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util.MetaController
import controllers.util.Security
import controllers.util.toError
import play.api.{ Logger => log }
import play.api.libs.json.JsError
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.ResourceStates

import controllers.util.stringmap

/**
 * Code for integrating Security and Meta. Handles resource synchronization between
 * the two systems (Orgs and Accounts), and implements the REST endpoints for CRUD
 * operations on Orgs and Users in Meta (ensuring they are created in Security as well).
 *
 * TODO: Security doesn't have PUT/PATCH endpoints - need that.
 */
object Meta extends GestaltFrameworkSecuredController[DummyAuthenticator] with MetaController with NonLoggingTaskEvents {

  import play.api.libs.json._

  implicit def str2js(s: String) = JsString(s)

  //
  // TODO: Tighten up error-handling - too many 500s
  // TODO: ResourceFactory::create - failed property validation assertion is being swallowed!
  //

  def sync() = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>
    trace("sync")

    log.debug("Getting Orgs from Security.")
    /* Get Security Org/User tree */
    val sd = Security.getOrgSyncTree(None, request.identity) match {
      case Success(data) => data
      case Failure(err)  => throw err
    }

    /* Get parent of given org - get root Org if None */
    def parentId(o: GestaltOrg) = {
      if (o.parent.isDefined) o.parent.get.id
      else Security.getRootOrg(request.identity) match {
        case Success(org) => org.id
        case Failure(err) => throw err
      }
    }

    val metaorgs = ResourceFactory.findAll(ResourceIds.Org)
    val metausers = ResourceFactory.findAll(ResourceIds.User)

    val secOrgIds = sd.orgs map { _.id }
    val secAccIds = sd.accounts map { _.id }
    val metaOrgIds = metaorgs map { _.id }
    val metaUserIds = metausers map { _.id }

    val secOrgMap = (sd.orgs map { o => (o.id, o) }).toMap
    val secAccMap = (sd.accounts map { a => (a.id, a) }).toMap

    val orgsCreate = secOrgIds.diff(metaOrgIds) map { u => secOrgMap(u) }
    val usersCreate = secAccIds.diff(metaUserIds) map { u => secAccMap(u) }

    log.debug(s"Orgs Create [${orgsCreate.size} orgs]:")
    for (o <- orgsCreate) log.debug("%s, %s".format(o.id, o.name))

    log.debug(s"Users Create [${usersCreate.size} users]:")
    for (a <- usersCreate) log.debug("%s, %-10s : org: %s".format(a.id, a.name, a.directory.orgId))

    log.debug("Creating Resources in Meta...")

    
    /*
     * TODO: Refactor this - as it is any errors in the create methods are swallowed. 
     */
    
    Try {
      
      for (o <- orgsCreate) {
      
        createNewMetaOrg(parentId(o), o, properties = None)
      
      }
      
      for (a <- usersCreate) {
        val uprops = Some(Map(
            "email"       -> a.email,
            "firstName"   -> a.firstName,
            "lastName"    -> a.lastName,
            "phoneNumber" -> a.phoneNumber))
        createNewMetaUser(a.directory.orgId, a, properties = uprops)
      }

    } match {
      case Success(_) => NoContent
      case Failure(e) => InternalServerError
    }
  }

  //
  // TODO: Add creator to auth at create!
  //  

  /**
   * TODO: Return async GestaltTask
   * Create an Org in Security, then in Meta
   * API implements => POST /orgs/:uuid
   */
  def createOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createOrg($org)")
    CreateSynchronizedResult2(org, ResourceIds.Org, request.body)(
        Security.createOrg, createNewMetaOrg[JsValue])
  }

  def createOrgFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createOrgFqon($fqon)")
    orgFqon(fqon) match {
      case None      => Future { OrgNotFound(fqon) }
      case Some(org) => {
        CreateSynchronizedResult2(org.id, ResourceIds.Org, request.body)(
          Security.createOrg, createNewMetaOrg[JsValue])
      }
    }
  }

  def createResource(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createResource($fqon)")
    Future {
      orgFqon(fqon) match {
        case Some(org) => CreateResourceResult(org.id, request.body, request.identity)
        case None      => OrgNotFound(fqon)
      }
    }
  }

  def patchResource(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"patchResource($fqon, $id)")
    
    // Parse PATCH document from payload
    Future {
      
      safeGetPatchDocument(request.body) match {
        case Failure(e) => BadRequest(toError(400, e.getMessage))
        case Success(patch) => {
          // TODO: Don't currently use typeId, but will in future.
          val handler = PatchHandler(UUID.randomUUID(), id, patch)
          handler.applyPatch match {
            case Success(r) => Ok(Output.renderInstance(r))
            case Failure(e) => InternalServerError(toError(500, e.getMessage))
          }
        }
      }

    }
  }
  
  import com.galacticfog.gestalt.data._
  import com.galacticfog.gestalt.meta.api.output._
  
  def typeExists(typeId: UUID) = {
    !TypeFactory.findById(typeId).isEmpty
  }
  
  
  private def ownerFromAccount(account: AuthAccountWithCreds) = toOwnerLink(ResourceIds.User,
    account.account.id, name = Some(account.account.name), orgId = account.account.directory.orgId)
  
  
  private def CreateResourceResult(org: UUID, resourceJson: JsValue, user: AuthAccountWithCreds) = {
    trace(s"CreateResourceResult($org, [json], [account])")
    
    safeGetInputJson(resourceJson) match {
      case Failure(e)     => BadRequest(toError(400, e.getMessage))
      case Success(input) => {

        if (input.resource_type.isEmpty) {
          BadRequest(toError(400, s"resource_type must be specified."))
        }
        else if (!typeExists(input.resource_type.get)) {
          BadRequest(toError(400, s"resource_type ${input.resource_type.get} does not exist."))
        }
        else {
          val owner = if (input.owner.isDefined) input.owner else Some(ownerFromAccount(user))
          val resid = if (input.id.isDefined)    input.id else Some(UUID.randomUUID())
          val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
          val domain = fromResourceInput(org,
            input.copy(id = resid, owner = owner, resource_state = state))

          ResourceFactory.create(domain) match {
            case Success(res) => Ok(Output.renderInstance(res))
            case Failure(err) => InternalServerError(toError(500, err.getMessage))
          }
        }
      }  
    }
  }
  
  
  /*
  id: UUID,
  typeId: UUID,
  state: UUID = ResourceState.id(ResourceStates.Active),
  orgId: UUID,
  owner: ResourceOwnerLink,
  name: String,
  description: Option[String] = None,
  created: Option[Hstore] = None,
  modified: Option[Hstore] = None,  
  properties: Option[Hstore] = None,
  variables: Option[Hstore] = None,
  tags: Option[List[String]] = None,
  auth: Option[Hstore] = None)  
 */



  //
  // TODO: Refactor to use CreateOrgResult
  //
  def createTopLevelOrg() = GestaltFrameworkAuthAction(nullOptString(None)).async(parse.json) { implicit request =>
    Future {
      val input = request.body.as[GestaltResourceInput]
      val root = Security.getRootOrg(request.identity).get.id

      createSynchronized(ResourceIds.Org, root, input) match {
        case Success(resource) => Ok(Output.renderInstance(resource))
        case Failure(e)        => InternalServerError(e.getMessage)
      }
    }
  }

  /**
   * Create a User Account in Security, then in Meta
   * API implements => POST /orgs/:uuid/users
   */
//  def createUser(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
//    trace(s"createUser(org = $org)")
//    CreateSynchronizedResult(org, ResourceIds.User, request.body)
//  }
//
//  def createUserFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
//    trace(s"createUserFqon(fqon = $fqon)")
//    orgFqon(fqon) match {
//      case Some(org) => CreateSynchronizedResult(org.id, ResourceIds.User, request.body)
//      case None      => Future { OrgNotFound(fqon) }
//    }
//  }

type SecurityResourceFunction = (UUID, AuthAccountWithCreds, GestaltResourceInput) => Try[SecurityResource]
type MetaResourceFunction     = (UUID, SecurityResource, Option[Hstore]) => Try[GestaltResourceInstance]
    
  def createUser(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createUser(org = $org)")
    CreateSynchronizedResult2(org, ResourceIds.User, request.body)(
        Security.createAccount, createNewMetaUser[JsValue])
  }

  def createUserFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createUserFqon(fqon = $fqon)")
    orgFqon(fqon) match {
      case None      => Future { OrgNotFound(fqon) }  
      case Some(org) => 
        CreateSynchronizedResult2(org.id, ResourceIds.User, request.body)(
            Security.createAccount, createNewMetaUser[JsValue])
    }
  }

  private def CreateSynchronizedResult2[T](org: UUID, typeId: UUID, json: JsValue)
      (sc: SecurityResourceFunction, mc: MetaResourceFunction)(implicit request: SecuredRequest[T]) = {
    trace(s"CreateSynchronizedResult($org, $typeId, ${json.toString})")
    Future {
      safeGetInputJson(typeId, json) match {
        case Failure(e)     => BadRequest(toError(400, e.getMessage))
        case Success(input) => {
          
          createSynchronized2(org, typeId, input)(sc, mc) match {
            case Success(resource) => Ok(Output.renderInstance(resource))
            case Failure(e)        => InternalServerError(e.getMessage)
          }
          
        }
      }
    }
  }

  /*
   * TODO: This method is shared between the Org and User resources. With minimal specification
   * the two input types are identical - validate properties to ensure things to to the right place.
   */
  private def CreateSynchronizedResult[T](org: UUID, typeId: UUID, json: JsValue)(implicit request: SecuredRequest[T]) = {
    trace(s"CreateSynchronizedResult($org, $typeId, ${json.toString})")
    Future {
      safeGetInputJson(typeId, json) match {
        case Failure(e)     => BadRequest(toError(400, e.getMessage))
        case Success(input) => {
          
          createSynchronized(typeId, org, input.copy(resource_type = Some(typeId))) match {          
            case Success(resource) => Ok(Output.renderInstance(resource))
            case Failure(e)        => InternalServerError(e.getMessage)
          }
          
        }
      }
    }
  }


private def createSynchronized2[T](org: UUID, typeId: UUID, input: GestaltResourceInput)
  (sc: SecurityResourceFunction, mc: MetaResourceFunction)(implicit request: SecuredRequest[T]) = {
  
    trace("createSynchronized(...)")
    //log.debug(s"Creating ${ResourceType.name(typeId)}")

    val stringprops = stringmap(input.properties)
    
    PropertyValidator.validate(typeId, stringprops) match {
      case  (false, message) => Failure(illegal(message.get))
      case _ => for {
        sr  <- sc(org, request.identity, input)
        mr  <- mc(org, sr, stringprops)
      } yield mr
    }    
//    PropertyValidator.validate(typeId, input.properties) match {
//      case  (false, message) => Failure(illegal(message.get))
//      case _ => for {
//        sr  <- sc(org, request.identity, input)
//        mr  <- mc(org, sr, input.properties)
//      } yield mr
//    }

  }
  

  private def safeGetPatchDocument(json: JsValue): Try[PatchDocument] = Try {
//    json.validate[PatchDocument].map {
//      case patch: PatchDocument => {
//        log.debug("PATCH : " + patch)
//        patch
//      }
//    }.recoverTotal { e =>
//      log.error("Error parsing request JSON: " + JsError.toFlatJson(e).toString)
//      illegal(toError(400, JsError.toFlatJson(e).toString))
//    }
      PatchDocument.fromJsValue(json)
  }
  
  private def safeGetInputJson(json: JsValue): Try[GestaltResourceInput] = Try {

    implicit def jsarray2str(arr: JsArray) = arr.toString

    json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => {
        log.debug("RESOURCE : " + resource)
        resource
      }
    }.recoverTotal { e =>
      log.error("Error parsing request JSON: " + JsError.toFlatJson(e).toString)
      illegal(toError(400, JsError.toFlatJson(e).toString))
    }
  }

  
  private def safeGetInputJson(typeId: UUID, json: JsValue): Try[GestaltResourceInput] = Try {
    val res = json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal {
      e => illegal(toError(400, JsError.toFlatJson(e).toString))
    }

    log.debug(s"Validating instance properties for type ${ResourceType.name(typeId)}")
    val validation = PropertyValidator.validate(typeId, stringmap(res.properties))

    if (validation._1) res else illegal(validation._2.get)
  }

  /**
   * Create a resource in both Security and Meta. For new Orgs and Accounts
   * created via the Meta API. Currently handles new Orgs and Users.
   */
  private def createSynchronized[T](typeId: UUID, org: UUID, input: GestaltResourceInput)(implicit request: SecuredRequest[T]) = Try {
    trace("createSynchronized(...)")
    log.debug(s"Creating ${ResourceType.name(typeId)}")

    //
    // TODO: Convert function resolvers to PartialFunction to avoid redundant
    // checks on typeId while avoiding MatchError warnings.
    //

    // Resolve Security function to create resources.
    val securityCreate: (UUID, AuthAccountWithCreds, GestaltResourceInput) => Try[SecurityResource] = typeId match {
      case ResourceIds.Org  => Security.createOrg _
      case ResourceIds.User => Security.createAccount _
      case _                => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
    }

    // Resolve Meta function to create resources.
    val metaCreate: (UUID, SecurityResource, Option[Hstore]) => Try[GestaltResourceInstance] = typeId match {
      case ResourceIds.Org  => createNewMetaOrg[T] _
      case ResourceIds.User => createNewMetaUser[T] _
      case _                => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
    }

    //
    // TODO: Need to validate properties against Meta before creating in Security
    // otherwise we're out of sync if meta-create fails.
    //
    
    val st = securityCreate(org, request.identity, input.copy(resource_type = Some(typeId)))
    st match {
      case Success(resource) => metaCreate(org, resource, stringmap(input.properties)) match {
        case Success(resource) => resource
        case Failure(ex)       => rte(s"Failed to create ${ResourceType.name(typeId)} in Meta: " + ex.getMessage)
      }
      case Failure(ex) => rte(s"Failed to create ${ResourceType.name(typeId)} in Security: " + ex.getMessage)
    }
    
    val metaResource = for {
      sr  <- securityCreate(org, request.identity, input.copy(resource_type = Some(typeId)))
      mr  <- metaCreate(org, sr, stringmap(input.properties))
    } yield mr
    
    metaResource match {
      case Success(res) => ???
      case Failure(err) => ???
    }
    
  }

  
  private def createNewMetaOrg[T](owningOrg: UUID, org: SecurityResource, properties: Option[Hstore])(implicit securedRequest: SecuredRequest[T]) = {
    trace(s"createNewMetaOrg($owningOrg, [org])")
    Try {
      val o = org.asInstanceOf[GestaltOrg]
      val parent = o.parent map { _.id }
      val props = Some(Map("fqon" -> o.fqon) ++ properties.getOrElse(Map()))

      ResourceFactory.create(
          fromSecurityResource(org, ResourceIds.Org, owningOrg, props), parent).get
    }
  }

  private def createNewMetaUser[T](owningOrg: UUID, account: SecurityResource, properties: Option[Hstore])(implicit securedRequest: SecuredRequest[T]) = {
    trace(s"createNewMetaUser($owningOrg, [account])")
    Try {
      val a = account.asInstanceOf[GestaltAccount]
      ResourceFactory.create(
          fromSecurityResource(a, ResourceIds.User, owningOrg, properties)).get
    }
  }

  /**
   * Convert Security::GestaltOrg or Security::GestaltAccount to GestaltResourceInstance
   * (could be used for other object types as well)
   */
  private def fromSecurityResource[T](sr: SecurityResource, typeId: UUID, org: UUID, properties: Option[Hstore] = None)(implicit request: SecuredRequest[T]) = {
    GestaltResourceInstance(
      id = sr.id,
      typeId = typeId,
      orgId = org, // this needs to be org from URI
      owner = ResourceOwnerLink(ResourceIds.User, request.identity.account.id),
      name = sr.name,
      properties = properties)
  }

  /**
   * Convert GestaltResourceInput to GestaltResourceInstance
   */
  private def fromResourceInput(org: UUID, in: GestaltResourceInput) = {
    GestaltResourceInstance(
      id = in.id.get,
      typeId = in.resource_type.get,
      state = ResourceState.id(in.resource_state.get),
      orgId = org,
      owner = in.owner.get,
      name  = in.name,
      description = in.description,
      /* TODO: Here is where we transform from map(any) to map(string) */
      properties = stringmap(in.properties),
      tags = in.tags,
      auth = in.auth)
  }  

  /**
   * Unwrap the given UUID or get the root org_id if None.
   */
  private def orgOrElseRoot[T](org: Option[UUID])(implicit request: SecuredRequest[T]) = org getOrElse {
    Security.getRootOrg(request.identity) match {
      case Success(org) => org.id
      case Failure(ex)  => throw ex
    }
  }

  /**
   * TODO: This will be replaced with an appropriate type-renderer.
   */
  private def pretty(r: GestaltResourceInstance) = Json.prettyPrint(Json.toJson(r))

}