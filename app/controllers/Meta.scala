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
import com.galacticfog.gestalt.meta.api.GestaltResourceInput
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.gestaltResourceInputFormat
import com.galacticfog.gestalt.meta.api.output.gestaltResourceInstanceFormat
import com.galacticfog.gestalt.meta.api.rte
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util.MetaController
import controllers.util.Security
import controllers.util.toError
import play.api.{Logger => log}
import play.api.libs.json.JsError
import play.api.libs.json.JsValue
import play.api.libs.json.Json


/**
 * Code for integrating Security and Meta. Handles resource synchronization between
 * the two systems (Orgs and Accounts), and implements the REST endpoints for creating
 * Orgs and Users in Meta (ensuring they are created in Security as well).
 */
object Meta extends GestaltFrameworkSecuredController[DummyAuthenticator] with MetaController with NonLoggingTaskEvents {
  
  //
  // TODO: Tighten up error-handling - too many 500s
  // TODO: ResourceFactory::create - failed property validation assertion is being swallowed!
  //

  def sync() = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>

    /* Get Security Org/User tree */
    val sd = Security.getOrgSyncTree(None, request.identity) match {
      case Success(data) => data
      case Failure(err) => throw err
    }

    /* Get parent of given org - get root Org if None */
    def parentId(o: GestaltOrg) = {
      if (o.parent.isDefined) o.parent.get.id 
      else Security.getRootOrg(request.identity) match {
        case Success(org) => org.id
        case Failure(err) => throw err
      }
    }

    val metaorgs  = ResourceFactory.findAll(ResourceIds.Org)
    val metausers = ResourceFactory.findAll(ResourceIds.User)    
    
    val secOrgIds = sd.orgs     map { _.id }
    val secAccIds = sd.accounts map { _.id }
    val metaOrgIds  = metaorgs  map { _.id }
    val metaUserIds = metausers map { _.id }

    val secOrgMap = (sd.orgs map     { o => (o.id, o) }).toMap
    val secAccMap = (sd.accounts map { a => (a.id, a) }).toMap
    
    val orgsCreate = secOrgIds.diff(metaOrgIds)   map { u => secOrgMap(u) }
    val usersCreate = secAccIds.diff(metaUserIds) map { u => secAccMap(u) }
    
    
    log.debug(s"Orgs Create [${orgsCreate.size} orgs]:")
    for (o <- orgsCreate) log.debug("%s, %s".format(o.id, o.name))
    
    log.debug(s"Users Create [${usersCreate.size} users]:")
    for (a <- usersCreate) log.debug("%s, %-10s : org: %s".format(a.id, a.name, a.directory.orgId))

    
    Try {  
      for (o <- orgsCreate) createNewMetaOrg(parentId(o), o)
      for (a <- usersCreate) createNewMetaUser(a.directory.orgId, a)
      
    } match {
      case Success(_) => NoContent
      case Failure(e) => InternalServerError
    }

  }

  
  //
  // TODO: Add creator to auth at create!
  //  
  
  
  /** TODO: Return async GestaltTask
   * Create an Org in Security, then in Meta
   * API implements => POST /orgs/:uuid
   */
  def createOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createOrg($org)")
    CreateSynchronizedResult(org, ResourceIds.Org, request.body)
  }
  
  def createOrgFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createOrgFqon($fqon)")
    orgFqon(fqon) match {
      case Some(org) => CreateSynchronizedResult(org.id, ResourceIds.Org, request.body)
      case None      => Future { OrgNotFound(fqon) }
    }
  }
  
  //
  // TODO: Refactor to use CrateOrgResult
  //
  def createTopLevelOrg() = GestaltFrameworkAuthAction(nullOptString(None)).async(parse.json) { implicit request =>
    Future {      
      val input = request.body.as[GestaltResourceInput]
      val root = Security.getRootOrg(request.identity).get.id
      
      createSynchronized(ResourceIds.Org, root, input) match {
        case Success(resource) => Ok(Output.renderInstance(resource))
        case Failure(e) => InternalServerError(e.getMessage)
      }
    }
  }  


  /**
   * Create a User Account in Security, then in Meta
   * API implements => POST /orgs/:uuid/users
   */
  def createUser(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createUser(org = $org)")
    CreateSynchronizedResult(org, ResourceIds.User, request.body)
  }
  
  def createUserFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createUserFqon(fqon = $fqon)")
    orgFqon(fqon) match {
      case Some(org) => CreateSynchronizedResult(org.id, ResourceIds.User, request.body)
      case None      => Future { OrgNotFound(fqon) }
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
        case Failure(e) => BadRequest(toError(400, e.getMessage))
        case Success(input) => createSynchronized(typeId, org, input) match {
          case Success(resource) => Ok( Output.renderInstance(resource) )
          case Failure(e) => InternalServerError(e.getMessage)
        }
      }
    }
  }

  private def safeGetInputJson(typeId: UUID, json: JsValue): Try[GestaltResourceInput] = Try {
    val res = json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal {
      e => illegal(toError(400, JsError.toFlatJson(e).toString))
    }
    
    log.debug(s"Validating instance properties for type ${ResourceType.name(typeId)}")
    val validation = PropertyValidator.validate(typeId, res.properties)
    
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
      case _ => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
    }

    // Resolve Meta function to create resources.
    val metaCreate: (UUID, SecurityResource) => Try[GestaltResourceInstance] = typeId match {
      case ResourceIds.Org  => createNewMetaOrg[T] _
      case ResourceIds.User => createNewMetaUser[T] _
      case _ => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
    }

    val st = securityCreate(org, request.identity, input.copy(resource_type = Some(typeId)))
    st match {
      case Success(resource) => metaCreate(org, resource) match {
        case Success(resource) => resource
        case Failure(ex) => rte(s"Failed to create ${ResourceType.name(typeId)} in Meta: " + ex.getMessage)
      }
      case Failure(ex) => rte(s"Failed to create ${ResourceType.name(typeId)} in Security: " + ex.getMessage)
    }
  }
    
// TODO: OLD SECURITY SYNC FUNCTION - VERIFY NEW FUNCTION AND REMOVE
//  /**
//   * Ensure Security and Meta contain the same resources of the given type.
//   * 
//   * @param typeId the Meta resource_type_id you want to synchronize
//   * @param owningOrg the Org used as the value for org_id on the new resource
//   */
//  private def syncSecurityResources[T](typeId: UUID, owningOrg: Option[UUID])(implicit request: SecuredRequest[T]) = Try {
//    
//    trace(s"syncSecurityResources($typeId, $owningOrg)")
//    
//    // TODO: This defaults to the root Org if None given (is that correct?).
//    val owner = orgOrElseRoot(owningOrg)
//    
//    // Resolve function to pull resources from Security
//    val securityReader: (Option[UUID], AuthAccountWithCreds) => Try[Seq[SecurityResource]] = typeId match {
//      case ResourceIds.Org  => Security.getAllOrgs _
//      case ResourceIds.User => Security.getAllAccounts _
//      case _ => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
//    }
//    
//    // Get the resources from Security
//    val securityResources = securityReader(Some(owner), request.identity) match {
//      case Success(rs) => rs.map(r => (r.id, r)).toMap
//      case Failure(ex) => throw ex
//    }
//    
//    // Get resource of the same type from Meta
//    val metaResources = ResourceFactory.findAll(typeId, owner).map(r => (r.id, r)).toMap
//    
//    /*
//     * Compare keys from both maps to determine which resources to 
//     * create/delete in Meta (Security is authoritative).
//     */
//    val securityIds = securityResources.keys.toList
//    val metaIds = metaResources.keys.toList
//    val resourcesCreate = securityIds.diff(metaIds) map { id => securityResources(id) }
//    val resourcesDelete = metaIds.diff(securityIds) map { metaResources(_) }
//    
//    
//    // Resolve function to create new resources in Meta.
//    val metaWriter: (UUID, SecurityResource*) => Try[Unit] = typeId match {
//      case ResourceIds.Org  => createNewMetaOrgs[T] _
//      case ResourceIds.User => createNewMetaUsers[T] _
//      case _ => illegal(Errors.INVALID_RESOURCE_TYPE_ID(typeId))
//    }
//
//    // TODO: Implement DELETE.
//    if (!resourcesCreate.isEmpty) metaWriter(owner, resourcesCreate : _ *) // else Success
//  }
  
  private def createNewMetaOrg[T](owningOrg: UUID, org: SecurityResource)(implicit securedRequest: SecuredRequest[T]) = {
    trace(s"createNewMetaOrgs($owningOrg, [org])")
    Try {
      val o = org.asInstanceOf[GestaltOrg]
      val parent = if (o.parent.isDefined) Some(o.parent.get.id) else None
      val props = Some(Map("fqon" -> o.fqon))
      ResourceFactory.create(toMeta(org, ResourceIds.Org, owningOrg, props), parent)
    }
  }  
  
  private def createNewMetaUser[T](owningOrg: UUID, account: SecurityResource)(implicit securedRequest: SecuredRequest[T]) = {
    trace(s"createNewMetaUser($owningOrg, [account])")
    Try {
      val a = account.asInstanceOf[GestaltAccount]
      log.debug(s"Creating User : ${a.id}, ${a.name}")
      ResourceFactory.create(toMeta(a, ResourceIds.User, owningOrg, Some(Map(
        "email"    -> a.email,    "firstName"   -> a.firstName, 
        "lastName" -> a.lastName, "phoneNumber" -> a.phoneNumber ))))//, "password" -> a.password))))
    }
  }
  
  /**
   * Create one or more Orgs in Meta from Security::GestaltOrg
   */
  private def createNewMetaOrgs[T](owningOrg: UUID, orgs: SecurityResource*)(implicit securedRequest: SecuredRequest[T]) = {
    trace(s"createNewMetaOrgs($owningOrg, [${orgs.size} orgs])")
    Try {
      for (org <- orgs.asInstanceOf[Seq[GestaltOrg]]) {
        val parent = if (org.parent.isDefined) Some(org.parent.get.id) else None
        val props = Some(Map("fqon" -> org.fqon))
        ResourceFactory.create(toMeta(org, ResourceIds.Org, owningOrg, props), parent)
      }
    }
  }  
  
  /**
   * Create one or more Users in Meta from Security::GestaltAccount
   */
  private def createNewMetaUsers[T](owningOrg: UUID, accounts: SecurityResource*)(implicit securedRequest: SecuredRequest[T]) = {
    trace(s"createNewMetaUsers($owningOrg, [${accounts.size} users])")
    Try {
      for (acc <- accounts.asInstanceOf[Seq[GestaltAccount]]) {
        trace(s"Creating User : ${acc.id}, ${acc.name}")
        val props = Some(Map(
          "email" -> acc.email,
          "firstName" -> acc.firstName,
          "lastName" -> acc.lastName,
          "phoneNumber" -> acc.phoneNumber))
        ResourceFactory.create(toMeta(acc, ResourceIds.User, owningOrg, props))
      }
    }
  }
  
  /**
   * Convert a Security Org or Account to a Meta representation. 
   * (could be used for other object types as well)
   */
  private def toMeta[T](sr: SecurityResource, typeId: UUID, org: UUID, properties: Option[Hstore] = None)(implicit request: SecuredRequest[T]) = {
    GestaltResourceInstance(
      id = sr.id,
      typeId = typeId,
      orgId = org, // this needs to be org from URI
      owner = ResourceOwnerLink(ResourceIds.User, request.identity.account.id),
      name = sr.name,
      properties = properties)
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