package controllers

import java.util.UUID

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try


import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.gestaltResourceInstanceFormat
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.meta.api.sdk.resourceLinkFormat
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util.HandleExceptions
import controllers.util.JsonUtil
import controllers.util.NotFoundResult
import controllers.util.getExpandParam
import controllers.util.booleanParam

import play.api.{Logger => log}
import play.api.libs.json.JsError
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json

import com.galacticfog.gestalt.meta.auth._

  
//
// TODO: Rename to 'EntitlementController'
//

object AuthorizationController extends Authorization {
  
  // --------------------------------------------------------------------------
  // POST
  // --------------------------------------------------------------------------
  
  /**
   * POST /{fqon}/entitlements
   */
  def postEntitlementOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    postEntitlementCommon(org, ResourceIds.Org, org)
  }
  
  /**
   * POST /{fqon}/{resource-type}/entitlements
   */
  def postEntitlementFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    postEntitlementCommon(fqid(fqon), typeId, resourceId)
  }
  
  
  // --------------------------------------------------------------------------
  // GET
  // --------------------------------------------------------------------------
  
//  def getEntitlementsOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
//    val org = fqid(fqon)
//    getEntitlementsCommon(org, ResourceIds.Org, org)
//  }
//  
//  def getEntitlementsFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon) { implicit request =>
//    getEntitlementsCommon(fqid(fqon), typeId, resourceId)
//  }
//  
//  def getEntitlementByIdOrgFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
//    val org = fqid(fqon)
//    getEntitlementByIdCommon(org, ResourceIds.Org, org, id)
//  }
//  
//  def getEntitlementByIdFqon(fqon: String, typeId: String, resourceId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>  
//    getEntitlementByIdCommon(fqid(fqon), UUID.fromString(typeId), resourceId, id)
//  }
//  

//  /**
//   * DELETE /{fqon}/entitlements/{id}
//   */
//  def deleteEntitlementOrgFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
//    val org = fqid(fqon)
//    deleteEntitlementCommon(org, org, id)
//  }
//  
//  /**
//   * DELETE /{fqon}/{resource-type}/entitlements/{id}
//   */
//  def deleteEntitlementFqon(fqon: String, typeId: String, resourceId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
//    deleteEntitlementCommon(fqid(fqon), resourceId, id)
//  }
//  
//  private [controllers] def deleteEntitlementCommon(org: UUID, parent: UUID, id: UUID) = {
//    ResourceFactory.findChildOfType(ResourceIds.Entitlement, parent, id) match {
//      case None => NotFoundResult(s"Entitlement with ID '$id' not found.")
//      case Some(res) => {
//        ResourceFactory.hardDeleteResource(ResourceIds.Entitlement, id) match {
//          case Success(_) => NoContent
//          case Failure(e) => HandleExceptions(e)
//        }
//      }
//    }
//  }
  
  def patchEntitlementFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  
  def handleEntitlementOptions(org: UUID, qs: Map[String,Seq[String]], baseUrl: Option[String] = None) = {
    val merge = booleanParam("effective", qs)
    val filter = qs.get("action")
  }
  
  
//  def handleEntitlementExpansion(org: UUID)(entitlements: Seq[GestaltResourceInstance])(implicit request: SecuredRequest[_]) = {
//    if (getExpandParam(request.queryString)) {
//      Ok(Json.toJson(transformEntitlements(org)(entitlements)))
//    } else Ok(Output.renderLinks(entitlements, META_URL))
//  }
  
  
  def validateEntitlementUpdate(old: GestaltResourceInstance, newent: GestaltResourceInstance) = {
    val oldaction = old.properties.get("action")
    val newaction = newent.properties.get("action")
    
    for {
      r1 <- Try { if (old.id == newent.id) newent else throw new ConflictException("You may not modify the resource ID.") }
      r2 <- Try { if (oldaction == newaction) newent else throw new ConflictException(s"You may not modify the entitlement action. Found: '$newaction', Expected: '$oldaction'") }
    } yield r2
  }
  
  def copyEntitlementForUpdate(old: GestaltResourceInstance, newent: GestaltResourceInstance) = Try {
     newent.copy(created = old.created)
  }  
  
  def putEntitlementOrgFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    putEntitlementCommon(org, org, id)
  }
  
  def putEntitlementFqon(fqon: String, parentTypeId: String, parentId: UUID, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    val parentType = UUID.fromString(parentTypeId)
    ResourceFactory.findById(parentTypeId, parentId) match {
      case None => Future(NotFoundResult(s"${ResourceLabel(parentId)} with ID '$id' not found."))
      case Some(parent) => {
        putEntitlementCommon(fqid(fqon), parent.id, id)
      }
    }
  }
  
  private[controllers] def putEntitlementCommon(org: UUID, parent: UUID, id: UUID)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    val user = request.identity
    val json = request.body
    
    /*
     * TODO: Authorize 'entitlement.update'
     */
    
    ResourceFactory.findById(ResourceIds.Entitlement, id) map { ent =>
      for {
        r1 <- validateEntitlementPayload(org, parent, user, json, "update")
        r2 <- validateEntitlementUpdate(ent, r1)
        r3 <- copyEntitlementForUpdate(ent, r2)
        r4 <- ResourceFactory.update(r3, user.account.id)
      } yield r4
      
    } getOrElse {
      
     throw new ResourceNotFoundException(s"Entitlement with ID '$id' not found.")
     
    } match {
      case Failure(e) => HandleExceptions(e)
      case Success(r) => Ok(transformEntitlement(r, org, META_URL))
    }    
  }
  

  
  private[controllers] def postEntitlementCommon(org: UUID, typeId: UUID, resourceId: UUID)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    // This is the resource we're creating the Entitlement for.
    val parentResource = ResourceFactory.findById(typeId, resourceId)
    
    /*
     * TODO: Authorize 'entitlement.create'
     */
    
    parentResource match {
      case None => NotFoundResult(s"${ResourceLabel(typeId)} with ID '$resourceId' not found.")
      case Some(parent) => {

        validateEntitlementPayload(org, parent.id, request.identity, request.body, "create") match {
          case Failure(e) => HandleExceptions(e)
          case Success(r) => {
            createResourceInstance(
              org, request.body, 
              Some(ResourceIds.Entitlement), 
              Some(resourceId)) match {
                case Failure(err) => HandleExceptions(err)
                case Success(res) => Created(transformEntitlement(res, org, META_URL)) 
              }

          }
        }   
      }
    }
  }
  
  /**
   * This currently only gets entitlements that are set DIRECTLY on the target Resource (resourceId)
   */
  private[controllers] def getEntitlementsCommon(org: UUID, typeId: UUID, resourceId: UUID)(implicit request: SecuredRequest[_]) = {
    AuthorizeList("entitlement.view") {
      ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, resourceId)
    }
  }
  
  private[controllers] def getEntitlementByIdCommon(org: UUID, typeId: UUID, resourceId: UUID, id: UUID)(
      implicit request: SecuredRequest[_]) = {

    ResourceFactory.findChildOfType(ResourceIds.Entitlement, resourceId, id) match {
      case Some(res) => Ok(transformEntitlement(res, org, META_URL))
      case None      => NotFoundResult(s"Entitlement with ID '$id' not found.")
    }
  }
  
  private[controllers] def validateEntitlementPayload(
      org: UUID, 
      parent: UUID, 
      creator: AuthAccountWithCreds, 
      payload: JsValue,
      accessType: String): Try[GestaltResourceInstance] = {
    
    log.debug("Entered: validateEntitlementPayload(...)")

    /*
     * Validations:
     * 1.) Caller has SetEntitlements permission
     * 2.) Action is valid
     * 3.) Entitlement for given action DOES NOT already exist
     * 4.) Given identities ALL exist
     */

    toResource(org, creator, payload) map { resource =>
      
      EntitlementProps.make(resource).validate match {
        case Left(err)    => throw new BadRequestException(err)
        case Right(props) => {

          accessType match {
            case "update" => resource
            case "create" => {
              
              // If we're trying to create, existing must be empty.        
              val existing = entitlementsByAction(parent, props.action)  
              if (existing.isEmpty) resource
              else throw new ConflictException(Errors.ActionExists(props.action))
            }
            case e => throw new RuntimeException(s"Unhandled accessType: '$e'")
          }
        }
      }
    }
    
  }
  
  private[this] object Errors {
    def ActionExists(action: String) = s"Found existing entitlement for action '${action}'. There can be only one."
  }
  
  /**
   * 
   */
  def toResource(org: UUID, creator: AuthAccountWithCreds, json: JsValue) = Try {
    
    safeGetInputJson(json) match {
      case Failure(error) => throw error
      case Success(input) => {
        withInputDefaults(org, input, creator, Option(ResourceIds.Entitlement))
      }
    }
  }
  
  /*
   * HACK: This is temporary. the 'transformEntitlement' methods are used to transform 'entitlement.properties.identities' 
   * from a Seq[UUID] to a Seq[ResourceLink]. This is necessary because 'identity' is a polymorphic reference
   * (either a User or a Group), and the type system has no way to indicate this fact for reference properties.
   */
  private[this] def transformEntitlements(org: UUID)(entitlements: => Seq[GestaltResourceInstance])(implicit request: SecuredRequest[_]) = {
    entitlements map { transformEntitlement(_, org, META_URL) }    
  }
  
  private[this] def transformEntitlement(ent: GestaltResourceInstance, org: UUID, baseUrl: Option[String]): JsValue = {
    
    val props = EntitlementProps.make(ent)
    
    val output = props.identities match {
      case None => None
      case Some(ids) => {
        val identities = ResourceFactory.findAllIn(ids)
        
        // Transform identity Seq[UUID] to Seq[ResourceLink] JSON.
        val idJson = Output.renderLinks(identities, baseUrl)
        
        JsonUtil.upsertProperty(Json.toJson(ent).as[JsObject], "identities", idJson) match {
          case Failure(e) => throw e
          case Success(newres) => {
            
            val orgLink = {
              Json.toJson {
                com.galacticfog.gestalt.meta.api.output.toLink(
                  ResourceFactory.findById(org).get, baseUrl)
              }
            }
            
            val out =
              JsonUtil.replaceKey(
                JsonUtil.replaceKey(
                  JsonUtil.replaceKey(newres, "typeId", "resource_type"),
                    "state", "resource_state"),
                      "orgId", "org", Some(orgLink))

            Option(out.as[JsValue])
          }
        }
      }
    }  
    if (output.isDefined) output.get else Json.toJson(ent) 
  }

}

