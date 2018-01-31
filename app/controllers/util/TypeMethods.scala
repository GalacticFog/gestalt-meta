package controllers.util

import play.api.Logger
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import controllers.util._
import java.util.UUID

import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import scala.language.postfixOps
import com.galacticfog.gestalt.json.Js
import javax.inject.Singleton
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.data.ResourceSelector
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods

object TypeMethods extends AuthorizationMethods {

  private[this] val log = Logger(this.getClass)
  
  /**
   * Render a ResourceType to JSON.
   * Uses the provided querystring to determine whether or not to expand property definitions inline.
   * 
   * @param p the type to render
   * @param qs the querystring sent with the original request
   * @param metaUrl address of the hosting meta server (used in creating resource link hrefs)
   */
  def renderType(p: GestaltResourceType, qs: Map[String, Seq[String]], metaUrl: String): Try[JsValue] = Try {
    val typeJson = Output.renderResourceTypeOutput(p, Some(metaUrl))
    if (QueryString.singleBoolean(qs, "withprops")) {
      log.debug("Found 'withprops' query param - looking up type-properties for expansion...")
      TypeMethods.withPropertiesExpanded(p.id, typeJson, metaUrl)
    } else typeJson
  }  
  
  
  /**
   * Render a type with property_defs expanded inline. This method a JSON rendering of a GestaltResourceType
   * and injects fully rendered property_defs (as opposed to just property links)
   * 
   * @param pid UUID of the type being rendered
   * @param pjson JSON representing the type being rendered
   * @param metaUrl address of the hosting meta server
   */
  def withPropertiesExpanded(pid: UUID, pjson: JsValue, metaUrl: String) = {
    /*
     * This maps over all properties declared directly on the target type
     * (not inherited properties) and fully renders them to JSON (complete resource).
     */
    val jsonProperties = PropertyFactory.findAll(pid).map { p =>
      Output.renderTypePropertyOutput(p, Some(metaUrl))
    }
  
    /*
     * Replace property_defs with fully expanded properties.
     */
    pjson.as[JsObject] ++ Json.obj("property_defs" -> jsonProperties)
  }  
 
  
  import com.galacticfog.gestalt.data.bootstrap._  
  
  def getParentTypes(tpe: GestaltResourceType): Seq[UUID] = {  
    val target = SystemType.fromResourceType(tpe)
    target.lineage.fold(Seq.empty[UUID]) { lin =>
      lin.parent_types.map(UUID.fromString(_))
    }
  }
  
  def updateInstanceEntitlements(
      targetType: UUID,
      forType: UUID,
      rootAccount: AuthAccountWithCreds,
      callerAccount: AuthAccountWithCreds,
      startingOrg: Option[UUID] = None): Seq[Seq[Try[GestaltResourceInstance]]] = {
    
    val instances = startingOrg.fold {
      ResourceFactory.findAll(targetType)
    }{ 
      org => ResourceFactory.findAll(targetType, startingOrg.get) 
    }
    
    /*
     * Set each entitlement for root
     * If the caller is the owner of a resource, add caller identity to entitlement
     */

    val rootId = rootAccount.account.id
    val userId = callerAccount.account.id
    
    val actions = getSelfActions(forType)
    val actionList = actions.mkString(",")
    
    instances.map { instance =>
      log.debug(s"Creating new entitlements on : ${instance.id} [$actionList]")
      val owner = if (instance.owner.id == userId) Some(userId) else None
      val ents = entitlements2(rootId, instance.orgId, instance.id, Seq.empty, actions, owner)
      setEntitlements(instance.orgId, rootAccount, instance, ents)
    }
  }
  
  
  import com.galacticfog.gestalt.security.api.{GestaltAPICredentials, GestaltAccount, GestaltDirectory}
  import com.galacticfog.gestalt.security.api.GestaltOrg
  
  
  
  
  def makeAccount(directoryOrg: GestaltOrg, account: GestaltAccount) = {
    //val (org, user) = security.getRootInfo(request.identity)
    
    val directory = GestaltDirectory(uuid(), directoryOrg.name, None, orgId = directoryOrg.id)
    
    newAuthAccountWithCreds(Map(
        "id" -> account.id,
        "username" -> account.name, 
        "firstName" -> account.firstName,
        "lastName" -> account.lastName,
        "description" -> account.description.getOrElse(""),
        "email" -> account.email.getOrElse(""),
        "phoneNumber" -> account.phoneNumber.getOrElse("")),
        None,
        directory)
  }  
  
  def newAuthAccountWithCreds(
      userInfo: Map[String,String] = Map(), 
      authHeader: Option[String] = None,
      directory: GestaltDirectory): AuthAccountWithCreds = {
    
    val defaultStr  = "foo"
    val header      = authHeader getOrElse s"Bearer ${uuid()}"
    val credentials = GestaltAPICredentials.getCredentials(header).get    
    //val directory   = GestaltDirectory(uuid(), defaultStr, None, uuid())

    val account = GestaltAccount(
        userInfo.get("id") map (UUID.fromString(_)) getOrElse uuid(),
        userInfo.getOrElse("username",  defaultStr), 
        userInfo.getOrElse("firstName", defaultStr),
        userInfo.getOrElse("lastName",  defaultStr),
        userInfo.get("description") orElse Option(defaultStr),
        userInfo.get("email")       orElse Option(defaultStr),
        userInfo.get("phoneNumber") orElse Option(defaultStr),
        directory)
        
    AuthAccountWithCreds(account, Seq(), Seq(), credentials, uuid())
  }    
  
  /**
   * Update a resource-type by adding the given list of UUIDs to its
   * `properties.lineage.child_types` list.
   * 
   * NOTE: This method actually saves the updated resource-type back to meta-repository.
   * 
   * @param caller UUID of the user calling this function
   * @param parent the resource type to update
   * @param children Seq of UUIDs to add to the child_types list
   */
  def updateChildTypes(caller: UUID, parent: GestaltResourceType, children: Seq[UUID]): Try[GestaltResourceType] = {
    
    if (children.isEmpty) Success(parent)
    else {
      val p = SystemType.fromResourceType(parent)
      val lineage = p.lineage getOrElse LineageInfo(Seq.empty, None)
      val oldlist = lineage.child_types getOrElse Seq.empty
      val newlist = (oldlist ++ children)
      
      val newlineage = lineage.copy(child_types = Some(newlist))
      val newprops = (parent.properties.get + ("lineage" -> Json.stringify(Json.toJson(newlineage))))
      val updated = parent.copy(properties = Some(newprops))
      
      TypeFactory.update(updated, caller)
    }
  }
  
  /**
   * Make the given type ID a child of each resource-type named in the parents sequence. Adds child
   * to `properties.lineage.child_types` of each parent resource.
   * 
   * @param caller UUID of the user calling this function
   * @param child UUID of the child-type
   * @param parents Seq of UUIDs to modify with the new child-type 
   */
  def makeNewParents(caller: UUID, child: UUID, parents: Seq[UUID]) = {

    def loop(pids: Seq[UUID], acc: Seq[Try[GestaltResourceType]]): Seq[Try[GestaltResourceType]] = {
      pids match {
        case Nil => acc
        case id :: tail => {
          TypeFactory.findById(id).fold {
            throw new UnprocessableEntityException(s"Cannot find given parent-type '$id'.")
          }{ parent =>
            log.debug(s"Adding $child as a child-of ${parent.name}")
            loop(tail, TypeMethods.updateChildTypes(caller, parent, Seq(child)) +: acc)
          }
        }
      }
    }
    loop(parents, Seq.empty)
  }
  
  /**
   * Transform type JSON into input-format.
   */
  private def safeGetTypeJson(json: JsValue): Try[GestaltResourceTypeInput] = Try {
    json.validate[GestaltResourceTypeInput].map{
      case resource: GestaltResourceTypeInput => resource
    }.recoverTotal{
      e => throw new BadRequestException(Js.errorString(e))
    }
  }
  
  /** 
   * Transform input to resource-type 
   */
  private def typeFromInput(org: UUID, owner: UUID, r: GestaltResourceTypeInput) = Try {
    val ownerLink = ResourceOwnerLink(ResourceIds.User, owner)
    GestaltResourceType(
        id = r.id.getOrElse(UUID.randomUUID),
        typeId = ResourceIds.ResourceType,
        extend = r.extend,
        state = r.resource_state.getOrElse(ResourceState.id(ResourceStates.Active)),
        orgId = org,
        owner = ownerLink,
        name = r.name,
        description = r.description,
        created = None, modified = None,
        properties = stringmap(r.properties), 
        variables = r.variables, 
        tags = r.tags, 
        auth = r.auth)
  }
  
  
  def payloadToResource(org: UUID, owner: UUID, json: JsValue): Try[GestaltResourceType] = {
    for {
      in  <- safeGetTypeJson(json)
      out <- typeFromInput(org, owner, in)
    } yield out
  }
  
  
  def typeId(name: String): Option[UUID] = {
    TypeFactory.findByName(name) map { _.id }
  }  
}