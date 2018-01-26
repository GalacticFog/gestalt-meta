package controllers

import java.util.UUID

import scala.annotation.tailrec
//import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.concurrent.Execution.Implicits.defaultContext
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
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import controllers.util._
import play.api.{Logger => log}
import play.api.libs.json.JsError
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import com.galacticfog.gestalt.meta.auth._

import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

  
//
// TODO: Rename to 'EntitlementController'
//
import javax.inject.Singleton

@Singleton
class AuthorizationController @Inject()(
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
    db: play.api.db.Database)
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  // --------------------------------------------------------------------------
  // POST
  // --------------------------------------------------------------------------
  
  /**
   * POST /{fqon}/entitlements
   */
  def postEntitlementOrgFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    val org = fqid(fqon)
    postEntitlementCommon(org, ResourceIds.Org, org)
  }
  
  /**
   * POST /{fqon}/{resource-type}/entitlements
   */
  def postEntitlementFqon(fqon: String, typeId: String, resourceId: UUID) = AsyncAudited(fqon) { implicit request =>
    postEntitlementCommon(fqid(fqon), typeId, resourceId)
  }

  /**
   * Ensure that neither the Entitlement ID or Action have been modified.
   */
  private[controllers] def validateEntitlementUpdate(old: GestaltResourceInstance, newent: GestaltResourceInstance) = {
    log.debug("validateEntitlementUpdate(...)")
    val oldaction = old.properties.get("action")
    val newaction = newent.properties.get("action")
    
    for {
      r1 <- Try { if (old.id == newent.id) newent else throw new ConflictException("You may not modify the resource ID.") }
      r2 <- Try { if (oldaction == newaction) newent else throw new ConflictException(s"You may not modify the entitlement action. Found: '$newaction', Expected: '$oldaction'") }
    } yield r2
  }
  
  /**
   * Simply copies the created object from old to new resource.
   */
  private[controllers] def copyEntitlementForUpdate(old: GestaltResourceInstance, newent: GestaltResourceInstance) = Try {
    log.debug("copyEntitlementForUpdate()")
     newent.copy(created = old.created)
  }
  
  def putEntitlementOrgFqon(fqon: String, id: UUID) = AsyncAudited(fqon) { implicit request =>
    val org = fqid(fqon)
    putEntitlementCommon(org, org, id)
  }
  
  def putEntitlementFqon(fqon: String, parentTypeId: String, parentId: UUID, id: UUID) = AsyncAudited(fqon) { implicit request =>
    val parentType = UUID.fromString(parentTypeId)
    ResourceFactory.findById(parentTypeId, parentId) match {
      case None => Future(NotFoundResult(s"${ResourceLabel(parentId)} with ID '$id' not found."))
      case Some(parent) => {
        putEntitlementCommon(fqid(fqon), parent.id, id)
      }
    }
  }

  private[controllers] def reconcile[A](base: Seq[A], adds: Seq[A], deletes: Seq[A]): Seq[A] = {
    (base filter { !deletes.contains(_) }) ++ adds
  }  
  
  private[controllers] def cascadeEntitlementIdentities(
      parent: UUID, 
      oldent: GestaltResourceInstance,
      newent: GestaltResourceInstance): Seq[GestaltResourceInstance] = {

    val delta = IdentityChange(oldent, newent)
    val action = EntitlementProps.make(oldent).action
    val targets = ResourceFactory.findDescendantEntitlements(parent, action)

    targets map { ent =>
      val existing = IdentityChange.getIdentities(ent)
      val newids = reconcile(existing, delta.added, delta.deleted)
      Entitlement.toGestalt(ent.owner.id,
        Entitlement.make(ent).withIdentities(newids))
    }
  }

  
  private[controllers] def putEntitlementCommon(org: UUID, parent: UUID, id: UUID)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    val user = request.identity
    val json = request.body

    val targetEntitlement = ResourceFactory.findById(ResourceIds.Entitlement, id) getOrElse {
        throw ResourceNotFoundException(s"Entitlement with ID '$id' not found")
    }

    val target = ResourceFactory.findById(parent) getOrElse {
      throw ResourceNotFoundException(s"Resource with ID '$parent' not found")
    }

    ResourceFactory.findParent(targetEntitlement.id).foreach {
      p => log.debug(s"entitlement ${id} has parent /${ResourceLabel(p.typeId)}/${p.id}")
    }

    val options = this.standardRequestOptions(user, id, targetEntitlement)
    val operations = this.standardRequestOperations("entitlement.update")
    
    SafeRequest(operations, options) Protect { maybeState =>
      
      val modified = for {
        r1   <- validateEntitlementPayload(org, parent, user, json, "update")
        r2   <- validateEntitlementUpdate(targetEntitlement, r1)
        r3   <- copyEntitlementForUpdate(targetEntitlement, r2)
        updates = cascadeEntitlementIdentities(parent, targetEntitlement, r3)
      } yield updates    
    
      modified match {
        case Failure(error) => HandleExceptions(error)
        case Success(updates)  => {
          
          val persisted = updates map { entitlement => 
            ResourceFactory.update(entitlement, user.account.id).get
          }
          log.debug(s"${persisted.size} entitlements modified in cascade.")
          val root = getUpdatedRoot(id, persisted).get
          Ok(transformEntitlement(root, org, Some(META_URL)))
        }
      }  
    }
  }
  
  
  /**
   * Filter the root entitlement from a list of descendant entitlements
   */
  private[controllers] def getUpdatedRoot(id: UUID, updates: Seq[GestaltResourceInstance]
      ): Try[GestaltResourceInstance] = Try {
    val updated = updates filter { _.id == id }
    updated.size match {
      case 1 => updated(0)
      case 0 => throw new RuntimeException("Could not find updated entitlement in cascade list. This is a bug.")
      case _ => throw new RuntimeException("Multiple matching root entitlements found. This is a bug.")
    }    
  }

  //private[controllers] def findChildTargets(targetType: UUID

  private[controllers] def findOrFail(typeId: UUID, id: UUID): Try[GestaltResourceInstance] = Try {
    ResourceFactory.findById(typeId, id) getOrElse {
      throw new ResourceNotFoundException(s"${ResourceLabel(typeId)} with ID '$id' not found.")
    }
  }

  def validateRequest(org: UUID, typeId: UUID, resourceId: UUID, user: AuthAccountWithCreds, json: JsValue) = {
    for {
      target <- findOrFail(typeId, resourceId)
      input  <- validateEntitlementPayload(org, target.id, user, json, "create")
    } yield input
  }  

  def gatherTargets(typeId: UUID, parent: UUID) = {
    ResourceFactory.findChildrenOfType(typeId, parent)
  }

  private[controllers] def postEntitlementCommon(
      org: UUID, 
      typeId: UUID, 
      resourceId: UUID)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    // This is the resource we're creating the Entitlement for.
  
    /*
     * TODO: Authorize 'entitlement.create'
     */
    val user = request.identity
    val json = request.body
    
    val entitlement = for {
      parent <- findOrFail(typeId, resourceId)
      input  <- validateEntitlementPayload(org, parent.id, user, json, "create")
      output <- CreateWithEntitlements(org, user, json, ResourceIds.Entitlement, Some(resourceId))
    } yield output

    entitlement match {
      case Failure(e) => HandleExceptions(e)
      case Success(resource) => {
        val transformed = transformEntitlement(resource, org, Some(META_URL))
        Created(transformed)
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
      case Some(res) => Ok(transformEntitlement(res, org, Some(META_URL)))
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
    toInput(json) match {
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
    entitlements map { transformEntitlement(_, org, Some(META_URL)) }
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

  private[this] def standardRequestOptions(
    user: AuthAccountWithCreds,
    target: UUID,
    resource: GestaltResourceInstance,
    data: Option[Map[String, String]] = None) = {

    RequestOptions(user,
      authTarget = Option(target),
      policyOwner = Option(target),
      policyTarget = Option(resource),
      data)
  }

  private[this] def standardRequestOperations(action: String) = {
    List(
      controllers.util.Authorize(action),
      controllers.util.EventsPre(action),
      controllers.util.PolicyCheck(action),
      controllers.util.EventsPost(action))
  }    
  
}

