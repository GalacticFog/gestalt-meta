package controllers


import java.net.URL
import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data.{CoVariant, EnvironmentType, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.keymgr.{GestaltFeature, GestaltLicense}
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.toLink
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import com.galacticfog.gestalt.meta.api.sdk.HostConfig
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.resourceLinkFormat
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil.replaceJsonPropValue
import controllers.util.JsonUtil.replaceJsonProps
import controllers.util.JsonUtil.str2js
import controllers.util.JsonUtil.upsertProperty
import controllers.util.db.EnvConfig
import play.api.i18n.MessagesApi
import play.api.libs.json._

import scala.language.implicitConversions
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.providers.ProviderManager
import javax.inject.Singleton

import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.providers._

@Singleton
class Meta @Inject()( messagesApi: MessagesApi,
                      env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                      security: Security,
                      providerManager: ProviderManager )

  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  // --------------------------------------------------------------------------
  // ORGS
  // --------------------------------------------------------------------------
  def postTopLevelOrg() = Authenticate().async(parse.json) { implicit request =>
    security.getRootOrg(request.identity) match {
      case Failure(err)  => { 
        log.error(s"Failed to create top-level Org.")
        Future(HandleExceptions(err)) 
      }
      case Success(root) => createOrgCommon(root.id)
    }  
  }  

  def postOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    createOrgCommon(fqid(fqon))
  }
  
  def createOrgCommon(parentOrg: UUID)(implicit request: SecuredRequest[JsValue]) = Future {
    
    val json = request.body
    val user = request.identity
    
    Authorize(parentOrg, "org.create", request.identity) {
      
      CreateSynchronized(parentOrg, ResourceIds.Org, json)(security.createOrg, createNewMetaOrg[JsValue]) match {
        case Failure(err) => HandleExceptions(err)
        case Success(res) => {

          setNewEntitlements(res.id, res.id, user, parent = Option(parentOrg))
          
          Created(Output.renderInstance(res, META_URL))
        }
      }

    }    
  }
  
  // --------------------------------------------------------------------------
  // GROUPS
  // --------------------------------------------------------------------------   
  def postGroupFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      createGroupCommon(fqid(fqon), request.body)  
    }
  }
  
  def createGroupCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {
    
    Authorize(org, "group.create", request.identity) {
      
      CreateSynchronized(org, ResourceIds.Group, request.body)(
          security.createGroup, createNewMetaGroup[JsValue]) match {
        
        case Failure(err) => HandleExceptions(err)
        case Success(res) => {
          val addusers: Seq[UUID] = Js.find(request.body.as[JsObject], "/properties/users").fold {
            Seq().asInstanceOf[Seq[UUID]]
            }{
            users => JsonUtil.safeParse[Seq[UUID]](users)
          }
          
          val newjson = { 
            if (addusers.isEmpty) Output.renderInstance(res, META_URL)
            else {
              log.debug(s"Adding ${addusers.size} users to new group.")
              security.addAccountsToGroup(res.id, addusers) match {
                case Success(users) => {
                  val jsonusers = Json.toJson(users)
                  JsonUtil.upsertProperty(RenderSingle(res).as[JsObject], "users", jsonusers).get
                }
                case Failure(error) => {
                  log.error("Failed adding users to new group: " + error.getMessage)
                  throw error
                }
              }
            }
          }
          setNewEntitlements(org, res.id, request.identity, parent = Option(org))
          Created( newjson )
        }
      }
    }
  }
  
  
  /**
   * 
   * Add one or more users to a group.
   * 
   */
  def patchGroupUsers(fqon: String, group: UUID) = Authenticate(fqon) { implicit request =>
    val qs = request.queryString
    
    val uids = Try {
      if (!qs.contains("id"))
        throw new BadRequestException("Must provide 'id' in querystring.")
      else {
        // -------------------------------------------------------------------------        
        // TODO: Need to manually validate the UUIDs since java.util.UUID.fromString
        // *fixes* broken strings if it thinks it can.
        // -------------------------------------------------------------------------
        val ids = qs("id") map { UUID.fromString(_) }
        val users = ResourceFactory.findAllIn(ResourceIds.User, ids)
        val found = users map { _.id }
        
        // Ensure we got all the users we asked for.
        val delta = ids.diff(found)
        if (!delta.isEmpty) throw new BadRequestException("The following user(s) not found: " + delta.mkString(","))
        else found
      }
    }
    
    log.debug("Attempting to add following users to group: " + uids)
    
    uids match {
      case Failure(err) => HandleExceptions(err)
      case Success(ids) => {
        security.addAccountsToGroup(group, ids) match {
          case Success(users) => Ok(Json.toJson(users))
          case Failure(errors) => HandleExceptions(errors)
        }
      }
    }
    
  }
  
  
  /* 
   * Remove a user from a group - note if a user that is not in the group (or user that doesn't exist)
   * is specified, no error is thrown.
   */
  def deleteGroupUsers(fqon: String, group: UUID) = Authenticate(fqon) { implicit request =>
    
    val qs = request.queryString
    
    val uids = Try {
      if (!qs.contains("id"))
        throw new BadRequestException("Must provide 'id' parameter in querystring.")
      else {
        // Make sure we actually got a value for 'id'
        val ids = {
          if (qs("id").isEmpty) throw new BadRequestException("Must provide at least one user-id")
          else qs("id") map { UUID.fromString(_) }
        }
        ResourceFactory.findAllIn(ResourceIds.User, ids) map { _.id }
      }
    }

    uids match {
      case Success(ids) => {
        log.debug(s"Attempting to REMOVE ${ids.size} user(s) from group ($group): " + ids)
        security.removeAccountsFromGroup(group, ids) match {
          case Success(members) => Ok(Json.toJson(members))
          case Failure(errors)  => HandleExceptions(errors)
        }
      }
      case Failure(err) => HandleExceptions(err)
    }

  }
  
  // --------------------------------------------------------------------------
  // USERS
  // --------------------------------------------------------------------------  
  
  /**
   * Create a User Account in Security, then in Meta
   */

  def postUserFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    createUserCommon(fqid(fqon), request.body)
  }
  

  def createUserCommon(org: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = Future {
    log.debug(s"createUserCommon($org, ...) => JSON: ${json.toString}")
    
    Authorize(org, "user.create", request.identity) {
      
      val root = security.getRootOrg(request.identity).get.fqon
      val home = Js.find(request.body.as[JsObject], "/properties/gestalt_home") getOrElse JsString(root)
      
      log.debug(s"Setting 'gestalt_home' to $home")
      
      val userJson = upsertProperty(request.body.as[JsObject], "gestalt_home", /*JsString(root)*/ home)
      
      CreateSynchronized(org, ResourceIds.User, userJson.get)(
        security.createAccount, createNewMetaUser[JsValue]) match {
          case Failure(err) => {
            log.error("Response from security.createAccount() : " + err.getMessage)
            HandleExceptions(err)
          }
          case Success(res) => {
            setNewEntitlements(org, res.id, request.identity, parent = Option(org))        
            Created(Output.renderInstance(res, META_URL))
          }
        }
    }
  }        

  
  implicit def featureToString(feature: GestaltFeature) = feature.getLabel
  
  
  def postResourceToOrg(fqon: String, typ: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newDefaultResourceResult(org, typeid, parent = org, payload = request.body)
  }
  
  def postResource(fqon: String, typ: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newDefaultResourceResult(org, typeid, parent, request.body)
  }
  
  def postResource2(fqon: String, typ: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newResourceResult2(org, typeid, parent, request.body) { resource =>
      CreateWithEntitlements(org, resource, Some(parent)) match {
        case Failure(err) => HandleExceptions(err)
        case Success(res) => Created(RenderSingle(res))
      }    
    }
  }

  
  
  def resolveProviderType(json: JsValue): UUID = {
    json \ "resource_type" match {
      case u: JsUndefined => 
        throw new BadRequestException("You must supply 'resource_type', i.e., resource_type = Provider::Type::Name")
      case v => {
        
        val validProviderTypes: Map[String,UUID] = 
          ResourceFactory.findTypesWithVariance(CoVariant(ResourceIds.Provider)).map { p =>
            (p.name -> p.id) 
          }.toMap

        log.debug("Parsed provider-type as : " + v.as[String])
        validProviderTypes.get(v.as[String]).fold {
          throw new BadRequestException(s"Unknown provider type : '${v.as[String]}'")
        }{ id => id }
        
      }
    }    
  }
  
  def postProviderConfigOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val orgId = fqid(fqon)
    postProviderCommon(orgId, ResourceIds.Org.toString, orgId, request.body)
  }
  
  def postProviderConfigFqon(fqon: String, parentType: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    postProviderCommon(fqid(fqon), parentType, parent, request.body)
  }

  def load(rootProvider: ProviderMap, identity: UUID) = {
    log.info("Beginning provider initialization...")
    providerManager.loadProviders(Some(rootProvider)) map { seq =>
      
      seq.flatMap { case (provider, services) => 
        
        log.debug(s"Provider provisioning complete. Processing: ${provider.name}[${provider.id}]")
        log.debug("Checking for dynamic updates to provider environment variables.")
        
        if (services.isEmpty) {
          log.debug("No variable updates necessary.")
          Option(Try(provider.root))
        }
        else {
          provider.env map { vs =>
            log.debug(s"Updating configuration for provider: ${provider.name}(${provider.id})")
            saveProvider(provider, vs, identity)
          }
        }
      }
    }    
  }
  
  /**
   * Update a provider resource in Meta DB, replacing its properties.config.env object
   * with the given ProviderEnv. 
   */
  private[controllers] def saveProvider(provider: ProviderMap, env: ProviderEnv, identity: UUID): Try[GestaltResourceInstance] = {
    val tryResource = ProviderMap.replaceEnvironmentConfig(provider.root, env)
    tryResource match {
      case Failure(e) => {
        log.error(s"Failed updating environment configuration for resource: ${provider.name}(${provider.id})")
        tryResource
      }
      case Success(resource) => {
        log.debug(s"Successfully updated env configuration for: ${resource.name}(${resource.id})")
        ResourceFactory.update(resource, identity)
        log.debug(s"Updated: ${resource.name}(${resource.id})")
        tryResource
      }
    }
  }
  
  /**
   * Filters the target provider from the result-set obtained when creating a new provider.
   */
  private[controllers] def getCurrentProvider(targetId: UUID, results: Future[Seq[Try[GestaltResourceInstance]]]) = {
    log.debug("entered getCurrentProvider(_, _)")
    for {
      seq <- results
      out = {
        log.debug(s"Finding provider in result-set. Seeking: ${targetId}")
        seq filter { t => t.isSuccess && t.get.id == targetId } map { _.get }
      }
    } yield {
      if (out.size > 1) {
        log.error(s"Could not find Provider ${targetId} in results-list.")
        throw new RuntimeException("An error occurred launching the Provider")
      } else out.head
    }    
  }
  
  /**
   * Currently just injects the given UUID and resource_type UUID
   * into the POST JSON paylod for a new Provider.
   */
  private[controllers] def normalizeProviderPayload(
      payload: JsValue, 
      newId: UUID, 
      providerType: UUID,
      parent: GestaltResourceInstance, 
      baseUrl: Option[String]): Try[JsObject] = {

    log.debug("Adding default kube networks...")
    lazy val addDefaultNetwork = (__ \ 'properties \ 'config).json.update(
      __.read[JsObject].map{ c => c ++
        Json.obj(
          "networks" -> (c \ "networks").asOpt[Seq[JsObject]].getOrElse[Seq[JsObject]](Seq(Json.obj("name" -> "default")))
        )
      }
    )

    for {
      withSpecifics <- providerType match {
        case ResourceIds.KubeProvider | ResourceIds.DockerProvider => Try {
          val result = payload.transform(addDefaultNetwork)
          log.debug("RESULT:\n" + result.toString())
          result.get
        }
        case _ => Success(payload.as[JsObject])
      }
      
      withParentProp <- {
        log.debug("adding parent prop...")
        JsonUtil.upsertProperties(withSpecifics,
        "parent" -> Json.toJson(toLink(parent, baseUrl))
      )}
      withIdAndType = withParentProp ++ Json.obj(
        "id" -> newId.toString,
        "resource_type" -> providerType.toString
      )
      output <- updateLinkedProviders(withIdAndType)
    } yield output
    
  }
  
  private[controllers] def providerRequestOptions(org: UUID, user: AuthAccountWithCreds, payload: JsValue, baseUrl: String) = {
    RequestOptions(user, 
      authTarget = Option(org), 
      policyOwner = Option(org), 
      policyTarget = Option(j2r(org, user, payload, Option(ResourceIds.Provider))),
      data = Option(Map("host" -> baseUrl)))    
  }
  
  def updateLinkedProviders(json: JsObject): Try[JsObject] = {

    val lps = Js.find(json, "/properties/linked_providers") map { lp =>
      LinkedProvider.fromJson(lp)
    }
    
    val updatedLinks = lps.fold(Seq[LinkedProvider]()) { links =>
      val linkids = links map { _.id }
      
      // Lookup all given linked_providers by their ID - load into a map keyed by ID
      val providers = ResourceFactory.findAllIn(linkids).map { 
        res => (res.id, res) 
      }.toMap 
      
			// If we found fewer providers than we asked for the request is invalid.
      if (providers.size < linkids.size) {
        val missing = linkids.diff( (providers.keys.toSeq) )
        throw new UnprocessableEntityException(
            s"Invalid linked_providers. The following were not found: [${missing.mkString(",")}]")
      }
      
      // Inject the type-id of each provider into the given links.
      links map { k =>
        k.copy(
          typeId = Some(providers(k.id).typeId),
          `type` = Some(sdk.ResourceName(providers(k.id).typeId))
        )
      }
    }
    
    if (updatedLinks.isEmpty) Success(json) 
    else {
      val jsonLinks = Json.toJson(updatedLinks)
      import com.galacticfog.gestalt.patch._
      val patch = PatchDocument(PatchOp.Replace("/properties/linked_providers", jsonLinks))
      patch.applyPatch(json)
    }
  }
  
  
  def postProviderCommon(org: UUID, parentType: String, parentId: UUID, json: JsValue)(
      implicit request: SecuredRequest[JsValue]) = {
    
    val providerType = resolveProviderType(json)
    val parentTypeId = UUID.fromString(parentType)
    
    ResourceFactory.findById(parentTypeId, parentId).fold {
      Future(ResourceNotFound(parentTypeId, parentId))
    }{ parent =>
      
      // This is the ID we create the new provider with.
      val targetid = Js.find(json.as[JsObject], "/id").fold(UUID.randomUUID) {
        uid => UUID.fromString(uid.as[String])
      }
      
      val user    = request.identity
      val payload = normalizeProviderPayload(json, targetid, providerType, parent, META_URL).get
      
      
      log.debug("about to call newResourceResult...")
     newResourceResultAsync(org, providerType, parent.id, payload) { _ =>
       
       log.debug("about to call CreateResource...")
        val identity = user.account.id
        CreateResource(org, user, payload, providerType, Some(parentId)) match {
          case Failure(e) => {
            /*
             * TODO: Update Provider as 'FAILED' in Meta
             */
            log.error("CreateResource Failed!")
            Future.successful(HandleExceptions(e))
          }
          case Success(newMetaProvider) => {
            log.debug("CreateResource Success!")
            
            val results = load(ProviderMap(newMetaProvider), identity)
            getCurrentProvider(targetid, results) map { t => 
              Created(RenderSingle(t)) 
            } recover { 
              case e => {
                /*
                 * TODO: Update Provider as 'FAILED' in Meta
                 */                
                HandleExceptions(e)
              }
            }
          }
        }
      }
      
    }
  }
  
  // --------------------------------------------------------------------------
  //
  // UTILITY FUNCTIONS
  //
  // --------------------------------------------------------------------------

  private def CreateSynchronized[T](org: UUID, typeId: UUID, json: JsValue)
    (sc: SecurityResourceFunction, mc: MetaResourceFunction)
    (implicit request: SecuredRequest[T]): Try[GestaltResourceInstance] = {
    
    log.debug(s"CreateSynchronized(org = $org, typeId = $typeId)")

    for {
      input    <- safeGetInputJson(json, Option(typeId))
      resource <- syncWithSecurity(org, typeId, input)(sc, mc)
    } yield resource
  }
  
  private def syncWithSecurity[T](org: UUID, typeId: UUID, input: GestaltResourceInput)
    (sc: SecurityResourceFunction, mc: MetaResourceFunction)
    (implicit request: SecuredRequest[T]): Try[GestaltResourceInstance] = {

    val stringprops = stringmap(input.properties)
    val creator = request.identity//.account.id
    
    for {
      sr <- sc(org, request.identity, input)
      mr <- mc(creator, org,  sr, stringprops, input.description)
    } yield mr
  }
  
  def normalizeUserJson(json: JsObject, account: AuthAccountWithCreds) = {
    // Check if gestalt_home property is set - if not
    // create property and set to 'root' Org.
    
    json \ "properties" \ "gestalt_home" match {
      case u: JsUndefined => {
        val root = security.getRootOrg(account)
        val props = replaceJsonPropValue(json, "gestalt_home", root.get.fqon)
        replaceJsonProps(json, props)
      }
      case _ => json
    }
  }
  
  /**
   * Unwrap the given UUID or get the root org_id if None.
   */
  private def orgOrElseRoot[T](org: Option[UUID])(implicit request: SecuredRequest[T]) = org getOrElse {
    security.getRootOrg(request.identity) match {
      case Success(org) => org.id
      case Failure(ex)  => throw ex
    }
  }

}
