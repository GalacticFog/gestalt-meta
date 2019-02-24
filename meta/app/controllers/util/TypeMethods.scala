package controllers.util

import play.api.Logger
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import java.util.UUID

import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.security.api.DIRECTORY_TYPE_INTERNAL
import com.galacticfog.gestalt.data.bootstrap._  
import scala.util.{Either,Left,Right}

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
 
  def getParentTypes(tpe: GestaltResourceType): Seq[UUID] = {  
    val target = SystemType.fromResourceType(tpe)
    target.lineage.fold(Seq.empty[UUID]) { lin =>
      lin.parent_types.map(UUID.fromString(_))
    }
  }
  
  /*
   * This function ADDS entitlements for new resource-types
   */
  def updateInstanceEntitlements(
      targetType: UUID,
      forType: UUID,
      rootAccount: AuthAccountWithCreds,
      callerAccount: AuthAccountWithCreds,
      startingOrg: Option[UUID] = None): Either[List[String], Unit]/*: Seq[Seq[Try[GestaltResourceInstance]]]*/ = {
    
    val instances = startingOrg.fold {
      ResourceFactory.findAll(targetType)
    }{ 
      org => ResourceFactory.findAll(targetType, org) 
    }
    
    /*
     * Set each entitlement for root
     * If the caller is the owner of a resource, add caller identity to entitlement
     */

    val rootId = rootAccount.account.id
    val userId = callerAccount.account.id
    
    val actions = getSelfActions(forType)
    val actionList = actions.mkString(",")
    
    val results = instances.map { instance =>
      log.debug(s"Creating new entitlements on : ${instance.id} [$actionList]")
      /*
       * If the caller is the owner of the target instance set the 'owner' var which
       * will cause them to be added to the entitlement along with 'root'
       */
      val owner = if (instance.owner.id == userId) Some(userId) else None
      
      /*
       * This is the 'owning org' (Org where the entitlement will be created). If the target
       * instance *is* an Org, we use that ID. If it is any other type, we use its parent org ID
       */
      val orgId = if (instance.typeId == ResourceIds.Org) instance.id else instance.orgId
      
      val ents = entitlements2(rootId, orgId, instance.id, Seq.empty, actions, owner)

      setEntitlements(orgId, rootAccount, instance, ents).collect { case Failure(e) => e.getMessage }
    }
    
    /*
     * If any errors occurred setting entitlements the error messages will be in results. We just
     * flatten the messages and return the error state.
     */
    val test = results.flatten
    if (test.nonEmpty) Left(test) else Right(())
  }
  
  
  def updateInstanceEntitlementsUserId(
      targetType: UUID,
      forType: UUID,
      rootUser: GestaltResourceInstance,
      userId: UUID,
      startingOrg: Option[UUID] = None): Either[List[String], Unit] = {
    
    val instances = startingOrg.fold {
      ResourceFactory.findAll(targetType)
    }{ 
      org => ResourceFactory.findAll(targetType, org) 
    }
    
    /*
     * Set each entitlement for root
     * If the caller is the owner of a resource, add caller identity to entitlement
     */
    val rootId = rootUser.id
    val actions = getSelfActions(forType)
    val actionList = actions.mkString(",")
    
    val results = instances.map { instance =>
      log.debug(s"Creating new entitlements on : ${instance.id} [$actionList]")
      /*
       * If the caller is the owner of the target instance set the 'owner' var which
       * will cause them to be added to the entitlement along with 'root'
       */
      val owner = if (instance.owner.id == userId) Some(userId) else None
      
      /*
       * This is the 'owning org' (Org where the entitlement will be created). If the target
       * instance *is* an Org, we use that ID. If it is any other type, we use its parent org ID
       */
      val orgId = if (instance.typeId == ResourceIds.Org) instance.id else instance.orgId
      
      val ents = entitlements2(rootId, orgId, instance.id, Seq.empty, actions, owner)
      setEntitlements(orgId, rootUser, instance, ents).collect { case Failure(e) => e.getMessage }
    }
    
    /*
     * If any errors occurred setting entitlements the error messages will be in results. We just
     * flatten the messages and return the error state.
     */
    val test = results.flatten
    if (test.nonEmpty) Left(test) else Right(())
  }  
  
  import com.galacticfog.gestalt.security.api.{GestaltAPICredentials, GestaltAccount, GestaltDirectory}
  import com.galacticfog.gestalt.security.api.GestaltOrg
  
  def makeAccount(directoryOrg: GestaltOrg, account: GestaltAccount) = {  
    val directory = GestaltDirectory(uuid(), directoryOrg.name, None, orgId = directoryOrg.id, directoryType = DIRECTORY_TYPE_INTERNAL.label)
    
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
  def addChildTypes(caller: UUID, parent: GestaltResourceType, children: Seq[UUID]): Try[GestaltResourceType] = {
    
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
   * Update a resource-type by removing the given list of UUIDs from its
   * `properties.lineage.child_types` list.
   * 
   * NOTE: This method actually saves the updated resource-type back to meta-repository.
   * 
   * @param caller UUID of the user calling this function
   * @param parent the resource type to update
   * @param children Seq of UUIDs to remove from the child_types list
   */
  def removeChildTypes(caller: UUID, parent: GestaltResourceType, children: Seq[UUID]): Try[GestaltResourceType] = {
    
    if (children.isEmpty) Success(parent)
    else {
      val p = SystemType.fromResourceType(parent)
      val lineage = p.lineage getOrElse LineageInfo(Seq.empty, None)
      val oldlist = lineage.child_types getOrElse Seq.empty
      val newlist = oldlist.filter(!children.contains(_))
      
      val newlineage = lineage.copy(child_types = Some(newlist))
      val newprops = (parent.properties.get + ("lineage" -> Json.stringify(Json.toJson(newlineage))))
      val updated = parent.copy(properties = Some(newprops))
      
      TypeFactory.update(updated, caller)
    }
  }  

  private[controllers] type LineageFunction = (UUID, GestaltResourceType, Seq[UUID]) => Try[GestaltResourceType]
  
  /**
   * Make the given type ID a child of each resource-type named in the parents sequence. Adds child
   * to `properties.lineage.child_types` of each parent resource.
   * 
   * @param caller UUID of the user calling this function
   * @param child UUID of the child-type
   * @param parents Seq of UUIDs to modify with the new child-type 
   */
  def makeNewParents(caller: UUID, child: UUID, parents: Seq[UUID]) = {
    log.debug(s"Adding $child to new parents.")
    modifyLineage(caller, child, parents)(TypeMethods.addChildTypes)
  }

  /**
   * Remove the given type ID as a child of each resource-type named in the parents sequence. Removes
   * the child from `properties.lineage.child_types` of each parent resource.
   * 
   * @param caller UUID of the user calling this function
   * @param child UUID of the child-type to remove
   * @param parents Seq of type UUIDs from which to remove child-type 
   */  
  def divorceParents(caller: UUID, child: UUID, parents: Seq[UUID]) = {
    log.debug(s"Removing $child from parents.")
    modifyLineage(caller, child, parents)(TypeMethods.removeChildTypes)
  }

  def destroyTypeEntitlements(tpe: GestaltResourceType): Try[Unit] = Try {
    val prefix = tpe.properties.get.get("actions").fold {
      throw new RuntimeException("Could not find `actions` value in resource-type data.")
    }{ api =>
      Js.find(Json.parse(api).as[JsObject], "/prefix").getOrElse {
        throw new RuntimeException("Could not parse `prefix` value from `properties/actions/prefix`")
      }
    }.as[String]

    def isPolymorphic(prefix: String): Boolean = {
      prefix == "provider" || prefix == "rule"
    }
    
    if (!isPolymorphic(prefix)) {
      
      /*
       * TODO: Deleting entitlements by 'action-pattern' removes all entitlements from the system
       * where action matches '{prefix}.*'. This is a problem for polymorphic resources that use the
       * same prefix such as 'provider' and 'rule'. 
       */
      val actionPattern = "%s.%%".format(prefix)
      val deleteIds = ResourceFactory.findEntitlementsByAction(actionPattern).map(_.id)
      
      log.debug(s"Destroying ALL entitlements with action-pattern : ${actionPattern}")
      ResourceFactory.destroyAllResourcesIn(deleteIds) match {
        case Failure(e) =>
          throw new RuntimeException("There were errors destroying instance entitlements: " + e.getMessage)
        case Success(_) => ;
      }
    }    
  }  
  
  def modifyLineage(caller: UUID, child: UUID, parents: Seq[UUID])(f: LineageFunction) = {
    def loop(pids: Seq[UUID], acc: Seq[Try[GestaltResourceType]]): Seq[Try[GestaltResourceType]] = {
      pids match {
        case Nil => acc
        case id :: tail => {
          TypeFactory.findById(id).fold {
            throw new UnprocessableEntityException(s"Cannot find given parent-type '$id'.")
          }{ parent =>
            loop(tail, f(caller, parent, Seq(child)) +: acc)
          }
        }
      }
    }
    loop(parents, Seq.empty)
  }  
  
  def typeIsProvider(json: JsValue, prefix: Option[String], restName: Option[String]): Boolean = {
    Js.find(json.as[JsObject], "/extend").fold(false) { ext =>
      val issubtype = ResourceFactory.isSubTypeOf(UUID.fromString(ext.as[String]), ResourceIds.Provider)
      if (issubtype) {
        val goodprefix = prefix.fold(true)(_.trim.toLowerCase == "provider")
        val goodrestname = restName.fold(true)(_.trim.toLowerCase == "providers")
        goodprefix && goodrestname
      } else false
    }
  }
  
  def validateCreatePayload(payload: JsValue): Try[JsValue] = Try {
    
    val json = payload.as[JsObject]
    val info = TypeFactory.findTypeMetadata()
    val prefixes: Seq[String]  = info.filter { case (_,_,_,p) => p.nonEmpty }.map { _._4.get }
    val restNames: Seq[String] = info.filter { case (_,_,r,_) => r.nonEmpty }.map { case (_,_,r,_) => r.get }
    val typeNames: Seq[String] = info.map { case (_,n,_,_) => n.toLowerCase }
    
    val prefix = Js.find(json, "/properties/actions/prefix") getOrElse {
      throw new BadRequestException("You must supply a value for `/properties/actions/prefix`")
    }
    
    if (prefix == null || prefix.as[String].trim.size == 0) {
      throw new BadRequestException("You must supply a value for `/properties/actions/prefix`")
    }
    
    val isProvider = prefix.as[String].trim.toLowerCase == "provider"
    
    /*
     * Providers (currently) allow duplicates - the prefix is always 'provider'
     */
    if (!isProvider && prefixes.contains(prefix.as[String])) {
      throw new ConflictException(s"`/properties/actions/prefix` must be globally unique. Value '$prefix' is taken.")
    }
    
    /*
     * We only test for rest_name if current resource is NOT a provider. 
     */
    if (!isProvider) {
      val restName = Js.find(json, "/properties/api/rest_name") getOrElse {
        throw new BadRequestException("You must supply a value for `/properties/api/rest_name`")
      }
      
      if (restNames.contains(restName.as[String])) {
        throw new ConflictException(s"`/properties/api/rest_name` must be globally unique. Value '$restName' is taken.")
      }
    }
    
    // MUST have at least one parent-type
    val parentTypes = Js.find(json, "/properties/lineage/parent_types") getOrElse {
      throw new BadRequestException("You must supply at least one parent type in `/properties/lineage/parent_types`")
    }
    
    Js.parse[Seq[String]](parentTypes) match {
      case Failure(e) => throw new BadRequestException("Failed parsing `/properties/lineage/parent_types`")
      case Success(ps) => if (ps.isEmpty)
        throw new BadRequestException("You must supply at least one parent type in `/properties/lineage/parent_types`")
    }
    
    val name = Js.find(json, "/name") getOrElse {
      throw new BadRequestException("You must supply a 'name' for your type.")
    }
  
    val nm = name.as[String]
    if (typeNames.contains(nm.toLowerCase)) {
      throw new BadRequestException(s"A type with name '$nm' already exists. Type names must be globally unique.")
    }

    /*
     * Ensure JSON contains 'properties.actions.verbs'.
     */
    import com.galacticfog.gestalt.patch._
    val verbs = Js.find(json, "/properties/actions/verbs")
    if (verbs.nonEmpty) payload 
    else {
      PatchDocument(PatchOp.Add("/properties/actions/verbs", Json.toJson(Seq.empty[String])))
          .applyPatch(json).get
    }
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

  def typeName(typeId: UUID): Option[String] = {
    TypeFactory.findById(typeId) map { _.name }
  }  
}