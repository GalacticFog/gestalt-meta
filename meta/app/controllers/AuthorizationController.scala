package controllers

import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.security.play.silhouette.{GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment}
import com.mohiva.play.silhouette.api.actions.SecuredRequest
//import com.galacticfog.gestalt.io.util.patch
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.output.Output
// import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import controllers.util._
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import com.galacticfog.gestalt.meta.auth._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import com.google.inject.Inject
import play.api.i18n.MessagesApi

  
//
// TODO: Rename to 'EntitlementController'
//
import javax.inject.Singleton

@Singleton
class AuthorizationController @Inject()(
    messagesApi: MessagesApi,
    gatewayMethods: GatewayMethods,
    sec: GestaltFrameworkSecurity,
    db: play.api.db.Database)
  extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
  
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
  
  def putEntitlement(fqon: String, id: UUID) = AsyncAudited(fqon) { implicit request =>
    putEntitlementCommon(fqid(fqon), id)
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
      Entitlement.toGestalt(
        creator = ent.owner.id,
        ent = Entitlement.make(ent).withIdentities(newids)
      )
    }
  }

  
  private[controllers] def putEntitlementCommon(org: UUID, id: UUID)(
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]) = Future {

    val user = request.identity
    val json = request.body

    val targetEntitlement = ResourceFactory.findById(ResourceIds.Entitlement, id) getOrElse {
      throw ResourceNotFoundException(s"Entitlement with ID '$id' not found")
    }

    val parent = ResourceFactory.findParent(targetEntitlement.id) match {
      case Some(p) =>
        log.debug(s"entitlement ${id} has parent /${ResourceLabel(p.typeId)}/${p.id}")
        p
      case None =>
        throw ResourceNotFoundException(s"Could not find parent of Entitlement '$id'")
    }

    val options = this.standardRequestOptions(user, id, targetEntitlement)
    val operations = this.standardRequestOperations("entitlement.update")

    SafeRequest(operations, options) Protect { _ =>

      val modified = for {
        r1   <- validateEntitlementPayload(org, parent.id, user, json, "update")
        r2   <- validateEntitlementUpdate(targetEntitlement, r1)
        r3   <- copyEntitlementForUpdate(targetEntitlement, r2)
        updates = cascadeEntitlementIdentities(parent.id, targetEntitlement, r3)
      } yield updates    
    
      modified match {
        case Failure(error) => HandleExceptions(error)
        case Success(updates)  => {

          val persisted = updates map { entitlement =>
            // for apiendpoint.invoke only; do we care if this doesn't work?
            updateApiEndpointByEntitlement(entitlement) recover {
              case e =>
                log.error(s"Error updating ApiEndpoint associated with Entitlement ${entitlement.id}", e)
            }
            ResourceFactory.update(entitlement, user.account.id).get
          }
          log.debug(s"${persisted.size} entitlements modified in cascade.")
          val root = getUpdatedRoot(id, persisted).get
          Ok(transformEntitlement(root, org, Some(META_URL)))
        }
      }
    }
  }

  private[controllers] def updateApiEndpointByEntitlement(entitlement: GestaltResourceInstance)
                                                         (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]): Future[Unit] = {
    val pair = for {
      ent <- Option(Entitlement.make(entitlement))
      if ent.properties.action == "apiendpoint.invoke"
      endpoint <- ResourceFactory.findParent(ResourceIds.ApiEndpoint, entitlement.id)
    } yield (ent,endpoint)
    pair match {
      case Some((ent,endpoint)) =>
        log.trace(s"updating 'apiendpoint.invoke' entitlement ${ent.id} for endpoint ${endpoint.id}")
        val (users,groups) = ent.properties.identities.getOrElse(Seq.empty).partition (
          id => ResourceFactory.findById(ResourceIds.User, id).isDefined
        )
        val patch = PatchDocument(
          PatchOp.Replace("/properties/plugins/gestaltSecurity/users", Json.toJson(users)),
          PatchOp.Replace("/properties/plugins/gestaltSecurity/groups", Json.toJson(groups))
        )
        gatewayMethods.updateEndpoint(endpoint, patch) map (_ => ())
      case _ => Future.successful(())
    }
  }


  /**
   * Filter the root entitlement from a list of descendant entitlements
   */
  private[controllers] def getUpdatedRoot( id: UUID,
                                           updates: Seq[GestaltResourceInstance] ): Try[GestaltResourceInstance] = Try {
    updates filter { _.id == id } match {
      case Seq(root) => root
      case Nil       => throw new RuntimeException("Could not find updated entitlement in cascade list. This is a bug.")
      case _         => throw new RuntimeException("Multiple matching root entitlements found. This is a bug.")
    }    
  }

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
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]) = Future {
    
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
  private[controllers] def getEntitlementsCommon(org: UUID, typeId: UUID, resourceId: UUID)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]) = {
    AuthorizeList("entitlement.view") {
      ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, resourceId)
    }
  }
  
  private[controllers] def getEntitlementByIdCommon(org: UUID, typeId: UUID, resourceId: UUID, id: UUID)(
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]) = {

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
        resourceWithDefaults(org, input, creator, Option(ResourceIds.Entitlement))
      }
    }
  }
  
  /*
   * HACK: This is temporary. the 'transformEntitlement' methods are used to transform 'entitlement.properties.identities' 
   * from a Seq[UUID] to a Seq[ResourceLink]. This is necessary because 'identity' is a polymorphic reference
   * (either a User or a Group), and the type system has no way to indicate this fact for reference properties.
   */
  private[this] def transformEntitlements(org: UUID)(entitlements: => Seq[GestaltResourceInstance])(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]) = {
    entitlements map { transformEntitlement(_, org, Some(META_URL)) }
  }
  
  private[controllers] def transformEntitlement(ent: GestaltResourceInstance, org: UUID, baseUrl: Option[String]): JsValue = {
    
    val identities = ResourceFactory.findAllIn(
      EntitlementProps.make(ent).identities.getOrElse(Seq.empty)
    )

    // Transform identity Seq[UUID] to Seq[ResourceLink] JSON.
    val idJson = Output.renderLinks(identities, baseUrl)

    JsonUtil.upsertProperty(
      Output.renderInstance(ent,baseUrl).as[JsObject],
      "identities",
      idJson
    ).get
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

