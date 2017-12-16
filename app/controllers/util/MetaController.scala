package controllers.util


import play.api.http.HeaderNames
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import controllers.util.db._
import play.api.Logger

import scala.util.{Failure, Success}
import scalikejdbc._
import com.galacticfog.gestalt.data._

import scala.util.Try
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import java.util.UUID

import com.galacticfog.gestalt.data.models._
import play.api.mvc.{Request, RequestHeader, Result}
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.output._
import controllers.SecurityResources
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.patch._
import controllers.util.JsonUtil._
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods

trait MetaControllerUtils extends AuthorizationMethods {

  case class MetaRequest( caller: AuthAccountWithCreds,
                          resource: GestaltResourceInstance,
                          resourceParent: UUID,
                          action: String,
                          baseUri: Option[String] = None)

  private[this] val log = Logger(this.getClass)

  /**
   * Get an Org by FQON
   */
  protected[controllers] def orgFqon(fqon: String): Option[GestaltResourceInstance] = {
    Resource.findFqon(fqon)
  }

  /**
    * Convert a string to a resource-state UUID. If state is empty, state = Active.
    */
  protected[controllers] def resolveResourceState(state: Option[String]) = {
    ResourceState.id(state getOrElse ResourceStates.Active)
  }
  
  protected[controllers] def throwBadRequest(message: String) =
    throw new BadRequestException(message)

  private[controllers] def CreateWithEntitlements(
                                                   owningOrgId: UUID,
                                                   creator: AuthAccountWithCreds,
                                                   resource: ResourceLike,
                                                   parentId: Option[UUID]): Try[GestaltResourceInstance] = {

    ResourceFactory.create(ResourceIds.User, creator.account.id)(
      resource.asInstanceOf[GestaltResourceInstance], parentId) map { r =>
      val es = setNewEntitlements(owningOrgId, r.id, creator, parentId)
      r
    }
  }


  /**
    *
    * @param baseUri if given this will be used to construct a URL pointing to resource in the EventMessage.
    */
  def newResourceRequestOptions(
                                 caller: AuthAccountWithCreds,
                                 resource: GestaltResourceInstance,
                                 parentId: UUID,
                                 baseUri: Option[String] = None) = {

    RequestOptions(caller,
      authTarget   = Option(parentId),
      policyOwner  = Option(parentId),
      policyTarget = Option(resource),
      data = Option(Map(
        "host"     -> baseUri.get,
        "parentId" -> parentId.toString,
        "typeId"   -> resource.typeId.toString)))
  }

  def newResourceRequestSetup(
                               caller: AuthAccountWithCreds,
                               resource: GestaltResourceInstance,
                               resourceParent: UUID,
                               action: String, baseUri: Option[String] = None):
  Tuple2[List[Operation[Seq[String]]], RequestOptions] = {
    (
      newResourceRequestOperations(resource.typeId, action),
      newResourceRequestOptions(caller, resource, resourceParent, baseUri)
    )
  }

  def newResourceRequestArgs(meta: MetaRequest):
  Tuple2[List[Operation[Seq[String]]], RequestOptions] = {
    val (operations, options) = newResourceRequestSetup(
      meta.caller,
      meta.resource,
      meta.resourceParent,
      meta.action, meta.baseUri)
    (operations, options)
  }

  /**
    * Inspect a GestaltResourceInput, supplying default values where appropriate.
    */
  def inputWithDefaults(org: UUID, input: GestaltResourceInput, creator: AuthAccountWithCreds): Instance = {
    val owner = if (input.owner.isDefined) input.owner else Some(SecurityResources.ownerFromAccount(creator))
    val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
    val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
    fromResourceInput(org, input.copy(id = resid, owner = owner, resource_state = state))
  }
  
  /**
    * Convert GestaltResourceInput to GestaltResourceInstance
    */
  def fromResourceInput(org: UUID, in: GestaltResourceInput) = {
    GestaltResourceInstance(
      id = in.id getOrElse UUID.randomUUID,
      typeId = in.resource_type.get,
      state = resolveResourceState(in.resource_state),
      orgId = org,
      owner = in.owner.get,
      name = in.name,
      description = in.description,
      properties = stringmap(in.properties),
      variables = in.variables,
      tags = in.tags,
      auth = in.auth)
  }

  def newResourceRequestOperations(typeId: UUID, action: String): List[Operation[Seq[String]]] = {
    List(
      controllers.util.Authorize(action),
      controllers.util.Validate(action),
      controllers.util.PolicyCheck(action),
      controllers.util.EventsPre(action),
      controllers.util.EventsPost(action))
  }

  /**
    * Get the base URL for this Meta instance
    */
  def META_URL(implicit requestHeader: RequestHeader): String = {
    val protocol = (requestHeader.headers.get(HeaderNames.X_FORWARDED_PROTO) getOrElse {
      if (requestHeader.secure) "https" else "http"
    }).toLowerCase
    val host = requestHeader.headers.get(HeaderNames.X_FORWARDED_HOST) getOrElse requestHeader.host
    "%s://%s".format(protocol, host)
  }

  def normalizeResourceType(obj: JsValue, expectedType: UUID) = Try {
    (obj \ "resource_type" match {
      case u : JsUndefined => obj.as[JsObject] + ("resource_type" -> expectedType.toString)
      case t => {
        if (!(t.as[UUID] == expectedType))
          throw new BadRequestException(
            s"Unexpected resource_type. found: ${t.toString}, required: ${expectedType.toString}")
        else obj
      }
    }).as[JsObject]
  }

}

trait MetaController extends SecurityResources with MetaControllerUtils with JsonInput { this: SecureController =>
  
  private[this] val log = Logger(this.getClass)
  
  private type RenderFunction = (GestaltResourceInstance, Option[String]) => JsValue
  
  /**
   * Render a single GestaltResourceInstance to JSON
   */
  private[controllers] def RenderSingle(res: GestaltResourceInstance)(
      implicit rh: RequestHeader): JsValue = {
    Output.renderInstance(res, Some(META_URL))
  }

  /**
   * Render a Seq of GestaltResourceInstances to Result[Ok(JsValue)]
   */
  protected[controllers] def RenderList(rss: Seq[GestaltResourceInstance])(implicit request: Request[_]): Result = {
    val qs = request.queryString
    
    
    if (QueryString.singleBoolean(qs, "compact")) {
      val compacted = rss.map(r => Output.renderCompact(r, Some(META_URL)))
      Ok(Json.toJson(compacted))
    } else Ok(handleExpandResource(rss, qs, Some(META_URL)))
  }

  
  /**
   * Check querystring for 'expand' param and render expanded resources/types if set to true.
   * This is used to control output for 'list' type GETs in the API. If expand is set to 'false'
   * the targets are rendered as JSON links.
   * 
   * @tparam A a subtype of ResourceLike (i.e. GestaltResourceInstance, GestaltResourceType)
   * @param rs a sequence of resources to render
   */
  def handleExpand[A <: ResourceLike](rs: Seq[A], qs: Map[String, Seq[String]], baseUri: Option[String] = None)
      (f: (A, Option[String]) => JsValue): JsValue = {
    if (getExpandParam(qs)) {
      Json.toJson(rs.map(f(_, baseUri)))
    } else Output.renderLinks(rs, baseUri)
  }
  
  /**
   * Render a list of GestaltResourceTypes to JSON based on the given querystring. If the 'expand'
   * querystring param is true, render full resource. If set to false, render a Link.
   */
  def handleExpandType(rs: Seq[GestaltResourceType], qs: Map[String, Seq[String]], metaUrl: Option[String] = None) = {
    handleExpand(rs, qs, metaUrl)(Output.renderResourceTypeOutput)
  }
  
  /**
   * Render a list of GestaltResourceInstances to JSON based on the given querystring. If the 'expand'
   * querystring param is true, render full resource. If set to false, render a Link.
   */
  def handleExpandResource(rs: Seq[GestaltResourceInstance], qs: Map[String, Seq[String]], metaUrl: Option[String] = None) = {
    handleExpand(rs, qs, metaUrl)(Output.renderInstance)
  }

  /**
   * Render a list of GestaltResourceInstances to JSON based on the given querystring. If the 'expand' 
   * querystring param is true, render full resource. If set to false, render a Link.  The resulting JSON
   * is wrapped in a 200 OK HTTP response. 
   */  
  def handleExpandResourceResult(rs: Seq[GestaltResourceInstance], qs: Map[String, Seq[String]], metaUrl: Option[String] = None) = {
    Ok(handleExpandResource(rs, qs, metaUrl))
  }


  
//  def handleExpansion(rs: Seq[ResourceLike], qs: Map[String, Seq[String]], baseUri: Option[String] = None) = {
//    if (getExpandParam(qs)) {
//      Ok(Json.toJson(rs map { r => 
//        Output.renderInstance(r.asInstanceOf[GestaltResourceInstance], baseUri) 
//      }))
//    }
//    else Ok(Output.renderLinks(rs, baseUri))
//  }


  protected[controllers] def ResourceNotFound(typeId: UUID, id: UUID) = 
    NotFoundResult(s"${ResourceLabel(typeId)} with ID '$id' not found.")  
    
  protected[controllers] def CreateResource(
    org: UUID,
    caller: AuthAccountWithCreds,
    resourceJson: JsValue,
    typeId: UUID,
    parentId: Option[UUID]): Try[GestaltResourceInstance] = {
    
    safeGetInputJson(resourceJson) flatMap { input =>
      val tid = assertValidTypeId(input, Option(typeId))
      ResourceFactory.create(ResourceIds.User, caller.account.id)(
        withInputDefaults(org, input, caller, Option(tid)), parentId) map { res =>
          setNewEntitlements(org, res.id, caller, parentId)
          res
        }
    }
  }
  
  private[controllers] def CreateWithEntitlements(
      owningOrgId: UUID,
      resource: ResourceLike,
      parentId: Option[UUID])(
          implicit request: SecuredRequest[_]): Try[GestaltResourceInstance] = {
    CreateWithEntitlements(owningOrgId, request.identity, resource, parentId)
  }

  /**
   * Convert a string FQON to corresponding Org UUID.
   */
  def fqid(fqon: String): UUID = orgFqon(fqon) map { _.id } getOrElse {
    throw new ResourceNotFoundException(s"Org with FQON '$fqon' not found.")
  }

  def inputToResource(org: UUID, creator: AuthAccountWithCreds, json: JsValue) = {
    inputWithDefaults(
      org = org, 
      input = safeGetInputJson(json).get, 
      creator = creator)
  }  
  
  def inputWithDefaults(
      org: UUID,
      input: GestaltResourceInput,
      typeId: Option[UUID],
      creator: AuthAccountWithCreds) = {

    val resid = if (input.id.isDefined) input.id else Some(UUID.randomUUID())
    val owner = if (input.owner.isDefined) input.owner else Some(SecurityResources.ownerFromAccount(creator))
    val state = if (input.resource_state.isDefined) input.resource_state else Some(ResourceStates.Active)
    val tpeid = Option(assertValidTypeId(input, typeId))

    val newInput = input.copy(
        id = resid,
        owner = owner,
        resource_type = tpeid,
        resource_state = state)

    fromResourceInput(org, newInput)
  }

  def updateFailedBackendCreate(caller: AuthAccountWithCreds, metaResource: GestaltResourceInstance, ex: Throwable) = {
    log.error(s"Setting state of resource '${metaResource.id}' to FAILED")
    val failstate = ResourceState.id(ResourceStates.Failed)
    ResourceFactory.update(metaResource.copy(state = failstate), caller.account.id)
    HandleExceptions(ex)
  }

  import com.galacticfog.gestalt.meta.auth.ActionMethods
  import play.api.mvc.Result

  def metaRequest(org: UUID, json: JsValue, resourceType: UUID, resourceParent: UUID, action: String)(
      implicit request: SecuredRequest[_]): MetaRequest = {

    object actions extends ActionMethods
    val resource = j2r(org, request.identity, json, Some(resourceType))
    actions.prefixFromResource(resource).fold {
      throw new RuntimeException(s"Could not find action prefix for type '${resourceType}'")
    }{ prefix =>
      val fqaction = "%s.%s".format(prefix, action)
      MetaRequest(request.identity, resource, resourceParent, fqaction, Some(META_URL))
    }
  }

  def newResourceRequest(org: UUID, resourceType: UUID, resourceParent: UUID, payload: Option[JsValue] = None)(
      implicit request: SecuredRequest[JsValue]): MetaRequest = {
    val json = payload getOrElse request.body
    metaRequest(org, json, resourceType, resourceParent, "create")
  }

  def MetaCreate(org: UUID, tpe: UUID, parent: UUID, payload: Option[JsValue] = None)(
      implicit request: SecuredRequest[JsValue]):(GestaltResourceInstance => Result) => Result = {
    val (operations, options) = newResourceRequestArgs {
      newResourceRequest(org, tpe, resourceParent = parent, payload = payload)
    }
    SafeRequest(operations, options).Execute _
  }

  def MetaCreateAsync(org: UUID, tpe: UUID, parent: UUID, payload: Option[JsValue] = None)(
      implicit request: SecuredRequest[JsValue]):(GestaltResourceInstance => Future[Result]) => Future[Result] = {
    val (operations, options) = newResourceRequestArgs {
      newResourceRequest(org, tpe, resourceParent = parent, payload = payload)
    }
    SafeRequest(operations, options).ExecuteAsync _    
  }      
      
  type JsonPayloadTransform = (JsValue, Map[String, String]) => Try[JsValue]
  
  val payloadTransforms: Map[UUID, JsonPayloadTransform] = Map(
    ResourceIds.Environment -> transformEnvironment,
    ResourceIds.Policy      -> transformPolicy,
    ResourceIds.RuleLimit   -> transformRule,
    ResourceIds.RuleEvent   -> transformRule
  )
  
  def transformEnvironment(json: JsValue, data: Map[String, String]): Try[JsValue] = {
    for {
      a <- normalizeResourceType(json, ResourceIds.Environment)
      b <- upsertProperty(a, "workspace", JsString(data("parent")))
      c <- normalizeEnvironmentType(b)
    } yield c      
  }
  
  /*
   * TODO: clean this up - use PatchOps to transform json.
   */
  def transformRule(json3: JsValue, data: Map[String, String]): Try[JsValue] = Try {

    val parentId = data("parent")
    val typeId = data("typeId")
    
    val json = json3.as[JsObject] ++ Json.obj("resource_type" -> JsString(typeId))
    val json2 = JsonUtil.withJsonPropValue(json, "parent", JsString(parentId))
    
    val definedAt = ResourceFactory.findById(ResourceIds.Policy, UUID.fromString(parentId)).fold {
      throw new RuntimeException("Could not find parent policy.")
    }{ p =>
      Json.parse(p.properties.get("parent"))
    }
    JsonUtil.withJsonPropValue(json2, "defined_at", definedAt)
  }
  
  protected [controllers] def updateRuleJson(ruleJson: JsValue, resourceType: UUID, parent: GestaltResourceInstance) = {
    val json = ruleJson.as[JsObject] ++ Json.obj("resource_type" -> JsString(resourceType.toString))
    val json2 = JsonUtil.withJsonPropValue(json, "parent", JsString(parent.id.toString))
    val definedAt = Json.parse(parent.properties.get("parent"))
    JsonUtil.withJsonPropValue(json2, "defined_at", definedAt)
  }  
   def resolveRuleTypeFromString(json: JsValue): Try[UUID] = Try {
    Js.find(json.as[JsObject], "/resource_type").fold {
      throw new BadRequestException("You must provide a resource_type")
    } { tpe =>
      ruleTypeId(expandRuleTypeName(tpe.as[String])).get
    }
  }
  
  /**
   * Create a fully-qualified Rule type name from it's simple name, i.e.,
   * 'event' becomes 'Gestalt::Resource::Rule::Event'
   */   
   private lazy val RULE_TYPE_NAME = Resources.Rule
   protected[controllers] def expandRuleTypeName(shortName: String) = {
    if (Seq("config", "event", "limit").contains(shortName.trim.toLowerCase)) {
      "%s::%s".format(RULE_TYPE_NAME, shortName.trim.capitalize)
    } else throw new BadRequestException(s"Invalid Rule type - found: '$shortName'")
  }
  
  /**
   * Get Rule type ID from its fully-qualified name.
   */
  protected [controllers] def ruleTypeId(typeName: String) = {
    typeName match {
      case a if a == Resources.RuleEvent  => Some(ResourceIds.RuleEvent)
      case b if b == Resources.RuleConfig => Some(ResourceIds.RuleConfig)
      case c if c == Resources.RuleLimit  => Some(ResourceIds.RuleLimit)
      case _ => None
    }
  }
  
  // TODO: Need to generalize name->uuid lookups
  // Replace environment_type simple-name into type UUID in environment properties
  def normalizeEnvironmentType(env: JsObject) = Try {
    val envTypeId = env \ "properties" \ "environment_type" match {
      case u : JsUndefined => throw new BadRequestException("Missing required property : 'environment_type'")
      case n => EnvironmentType.id(n.as[String])
    }
    env ++ Json.obj(
        "properties" -> 
        replaceJsonPropValue(env, "environment_type", envTypeId.toString)) 
  }
  
  def transformPayload(tpe: UUID, org: UUID, parent: UUID, payload: JsValue): Try[JsValue] = {
    payloadTransforms.get(tpe).fold(Try(payload)) { f =>
      val data = Map("org" -> org.toString, "parent" -> parent.toString, "typeId" -> tpe.toString)
      f(payload, data)
    }
  }
  
  def transformPolicy(json: JsValue, data: Map[String, String]): Try[JsValue]  = {
    val parentId = UUID.fromString(data("parent"))
    val link = Json.toJson(parentLink(parentId, None).get)
    Try(JsonUtil.withJsonPropValue(json.as[JsObject], "parent", link))
  }
  
  protected [controllers] def updatePolicyJson(policyJson: JsValue, parent: UUID)(implicit request: SecuredRequest[_]) = {
    val link = Json.toJson(parentLink(parent, Some(META_URL)).get)
    JsonUtil.withJsonPropValue(policyJson.as[JsObject], "parent", link)
  }
  
  protected [controllers] def parentLink(pid: UUID, baseUri: Option[String]): Option[ResourceLink] = {
    ResourceFactory.findById(pid) map { toLink( _, baseUri ) }
  }
  
  private[controllers] def newDefaultResourceResult(org: UUID, tpe: UUID, parent: UUID, payload: JsValue)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    val json = transformPayload(tpe, org, parent, payload).get
    
    MetaCreate(org, tpe, parent, Some(json)).apply { resource =>
      CreateWithEntitlements(org, resource, Some(parent)) match {
        case Failure(err) => HandleExceptions(err)
        case Success(res) => Created(RenderSingle(res))
      }
    }
  }
  
  private[controllers] def newResourceResult2(
      org: UUID, tpe: UUID, parent: UUID, payload: JsValue)(f: GestaltResourceInstance => Result)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    val json = transformPayload(tpe, org, parent, payload).get
    
    MetaCreate(org, tpe, parent, Some(json)).apply { resource =>
      f(resource)
    }
  }
  
  private[controllers] def newResourceResultAsync(
      org: UUID, tpe: UUID, parent: UUID, payload: JsValue)(f: GestaltResourceInstance => Future[Result])(
      implicit request: SecuredRequest[JsValue]) = {
    
    val json = transformPayload(tpe, org, parent, payload).get
    
    MetaCreateAsync(org, tpe, parent, Some(json)).apply { resource =>
      f(resource)
    }
  }
  
  private[controllers] def resolveTypeFromPayload(json: JsValue): Option[UUID] = {
    Js.find(json.as[JsObject], "/resource_type") flatMap { typeIdentifier =>
      /*
       * TODO: Delegate finding/validating the type to the TypeController/Service so
       * it can be retrieved from cache if available.
       */
      
      val ti = typeIdentifier.as[String]
      val InvalidResourceType = 
        s"Invalid resource_type identifier. You must provide either the fully-qualified resource type-name or the resource type UUID. found: '${ti}'."
      
      val givenId = parseUUID(ti).fold {
        log.info(s"Found resource_type value, but it is not a UUID. Attempting to resolve type-name...")
        TypeFactory.findByName(ti).fold { 
          throw new UnprocessableEntityException(InvalidResourceType)
        }{ typ => typ.id }
      }{ id => UUID.fromString(id) }
      
      if (TypeFactory.typeExists(givenId)) Some(givenId) else {
        throw new UnprocessableEntityException(InvalidResourceType)
      }
    }
  }
  
}

object MetaController {
  
}

