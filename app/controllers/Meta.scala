package controllers

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceIds
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{ PatchOp, PatchDocument, PatchHandler }
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

import controllers.util.trace

/**
 * Code for integrating Security and Meta. Handles resource synchronization between
 * the two systems (Orgs and Accounts), and implements the REST endpoints for CRUD
 * operations on Orgs and Users in Meta (ensuring they are created in Security as well).
 *
 * TODO: Security doesn't have PUT/PATCH endpoints - need that.
 */
object Meta extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with NonLoggingTaskEvents with SecurityResources {

  import play.api.libs.json._

  implicit def str2js(s: String) = JsString(s)

  //
  // TODO: Tighten up error-handling - too many 500s
  // TODO: ResourceFactory::create - failed property validation assertion is being swallowed!
  // TODO: Add creator to auth at create!
  //  

  
  //
  // TODO: Refactor to use CreateOrgResult
  //
  def createTopLevelOrg() = GestaltFrameworkAuthAction(nullOptString(None)).async(parse.json) { implicit request =>
//    Future {
      //val input = request.body.as[GestaltResourceInput]
      
      //val creator = request.identity.account.id

//      CreateSynchronizedResult2(org, ResourceIds.Org, request.body)(
//        Security.createOrg, createNewMetaOrg[JsValue])
    
      val root = Security.getRootOrg(request.identity).get.id
      
      Security.getRootOrg(request.identity) match {
        case Success(root) => 
          CreateSynchronizedResult(root.id, ResourceIds.Org, request.body)(
            Security.createOrg, createNewMetaOrg[JsValue])
        case Failure(err)  => Future { handleSecurityApiException(err) }
      }

      
//      createSynchronized2(root, ResourceIds.Org, input)(
//        Security.createOrg, createNewMetaOrg[JsValue]) match {
//          case Success(resource) => Ok(Output.renderInstance(resource))
//          case Failure(e)        => InternalServerError(e.getMessage)
//        }
//    }
  }  
  
  /**
   * TODO: Return async GestaltTask
   * Create an Org in Security, then in Meta
   * API implements => POST /orgs/:uuid
   */
  def createOrg(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createOrg($org)")
    CreateSynchronizedResult(org, ResourceIds.Org, request.body)(
      Security.createOrg, createNewMetaOrg[JsValue])
  }

  def createOrgFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createOrgFqon($fqon)")
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }
      case Some(org) => {
        CreateSynchronizedResult(org.id, ResourceIds.Org, request.body)(
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

  
  private object Err {
    
    def RESOURCE_TYPE_NOT_FOUND(m: String) = ???
    
    val RESOURCE_TYPE_NOT_GIVEN = "resource_type must be specified."
    
  }
  
//  private def validateInputResource(input: GestaltResourceInput, typeRequired: Boolean = true) = Try {
//    def typeExists(typeId: UUID) = !TypeFactory.findById(typeId).isEmpty
//    
//    if (input.resource_type.isEmpty) 
//      rte(toError(400, Err.RESOURCE_TYPE_NOT_GIVEN))
//    else if (!typeExists(input.resource_type.get))
//      rte(toError(404, Err.RESOURCE_TYPE_NOT_FOUND(input.resource_type.get)))
//  }
  
//  Need to formalize the toError structure
//  Need a function that takes an error and returns the appropriate Result based on error code.
  
  private def CreateResourceResult(org: UUID, resourceJson: JsValue, user: AuthAccountWithCreds) = {
    trace(s"CreateResourceResult($org, [json], [account])")
    
    def typeExists(typeId: UUID) = !TypeFactory.findById(typeId).isEmpty
    
    safeGetInputJson(resourceJson) match {
      case Failure(e) => BadRequest(toError(400, e.getMessage))
      case Success(input) => {
  
        if (input.resource_type.isEmpty) {
          log.error(s"No resource_type specified.")
          BadRequest(toError(400, Err.RESOURCE_TYPE_NOT_GIVEN))
        } 
        else if (!typeExists(input.resource_type.get)) {
          log.error(Err.RESOURCE_TYPE_NOT_FOUND(input.resource_type.get))
          NotFound(toError(404, Err.RESOURCE_TYPE_NOT_FOUND(input.resource_type.get)))
        } 
        else {
          val owner = if (input.owner.isDefined) input.owner else Some(ownerFromAccount(user))
          val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
          val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
          val domain = fromResourceInput(org,
            input.copy(id = resid, owner = owner, resource_state = state))

          ResourceFactory.create(domain) match {
            case Success(res) => Ok(Output.renderInstance(res))
            case Failure(err) => {
              log.error(s"Internal Server Error: ${err.getMessage}")
              InternalServerError(toError(500, err.getMessage))
            }
          }
        }
      }
    }
  }



  /**
   * Create a User Account in Security, then in Meta
   * API implements => POST /orgs/:uuid/users
   */
  def createUser(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    trace(s"createUser(org = $org)")
    CreateSynchronizedResult(org, ResourceIds.User, request.body)(
      Security.createAccount, createNewMetaUser[JsValue])
  }

  def createUserFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    trace(s"createUserFqon(fqon = $fqon)")
    orgFqon(fqon) match {
      case None => Future { OrgNotFound(fqon) }
      case Some(org) =>
        CreateSynchronizedResult(org.id, ResourceIds.User, request.body)(
          Security.createAccount, createNewMetaUser[JsValue])
    }
  }

  private def CreateSynchronizedResult[T](org: UUID, typeId: UUID, json: JsValue)(sc: SecurityResourceFunction, mc: MetaResourceFunction)(implicit request: SecuredRequest[T]) = {
    trace(s"CreateSynchronizedResult($org, $typeId, ${json.toString})")
    Future {
      safeGetInputJson(typeId, json) match {
        case Failure(e) => BadRequest(toError(400, e.getMessage))
        case Success(input) => {

          createSynchronized(org, typeId, input)(sc, mc) match {
            case Success(resource) => Ok(Output.renderInstance(resource))
            case Failure(e)        => InternalServerError(e.getMessage)
          }

        }
      }
    }
  }

  private def createSynchronized[T](org: UUID, typeId: UUID, input: GestaltResourceInput)(sc: SecurityResourceFunction, mc: MetaResourceFunction)(implicit request: SecuredRequest[T]) = {

    trace("createSynchronized(...)")
    //log.debug(s"Creating ${ResourceType.name(typeId)}")

    val stringprops = stringmap(input.properties)
    val creator = request.identity.account.id

    PropertyValidator.validate(typeId, stringprops) match {
      case (false, message) => Failure(illegal(message.get))
      case _ => for {
        sr <- sc(org, request.identity, input)
        mr <- mc(org, sr, creator, stringprops)
      } yield mr
    }
  }

  
  private def safeGetPatchDocument(json: JsValue): Try[PatchDocument] = Try {
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
      illegal(JsError.toFlatJson(e).toString)
    }
  }

  
  private def safeGetInputJson(typeId: UUID, json: JsValue): Try[GestaltResourceInput] = Try {
    val res = json.validate[GestaltResourceInput].map {
      case resource: GestaltResourceInput => resource
    }.recoverTotal {
      e => illegal(JsError.toFlatJson(e).toString)
    }

    log.debug(s"Validating instance properties for type ${ResourceType.name(typeId)}")
    val validation = PropertyValidator.validate(typeId, stringmap(res.properties))

    if (validation._1) res else illegal(validation._2.get)
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
      name = in.name,
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