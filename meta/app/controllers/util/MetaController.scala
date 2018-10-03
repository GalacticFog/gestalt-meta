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
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurityEnvironment}
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
import com.galacticfog.gestalt.meta.auth.ActionMethods
import play.api.mvc.Result
import com.galacticfog.gestalt.meta.auth.ActionMethods
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import play.api.mvc.Result


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

  protected[controllers] def fTry[T](t: => T): Future[T] =
    Future.fromTry(Try{t})

  protected[controllers] def findOrgOrFail(fqon: String): Future[GestaltResourceInstance] =
    fTry(orgFqon(fqon) getOrElse {
      throw new InternalErrorException("could not locate org resource after authentication")
    })


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
      creator: AccountLike,
      resource: ResourceLike,
      parentId: Option[UUID]): Try[GestaltResourceInstance] = {

    val result = ResourceFactory.create(ResourceIds.User, creator.id)(
      resource.asInstanceOf[GestaltResourceInstance], parentId
    ) map { r =>
      val _ = setNewResourceEntitlements(owningOrgId, r.id, creator, parentId)
      r
    }
    result
  }
  
  protected[controllers] def CreateWithEntitlements(
      org: UUID,
      creator: AuthAccountWithCreds,
      payload: JsValue,
      typeId: UUID,
      parentId: Option[UUID]): Try[GestaltResourceInstance] = {

    for {
      // Convert input JSON to in-memory resource
      resource <- jsonToResource(org, creator, payload, Some(typeId))
      
      // Persist resource to Meta along with appropriate Entitlements
      result   <- CreateWithEntitlements(org, creator, resource, parentId)
    } yield result
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
    lazy val protocol = (requestHeader.headers.get(HeaderNames.X_FORWARDED_PROTO) getOrElse {
      if (requestHeader.secure) "https" else "http"
    }).toLowerCase
    lazy val host = requestHeader.headers.get(HeaderNames.X_FORWARDED_HOST) getOrElse requestHeader.host
    lazy val inferredUrl = "%s://%s".format(protocol, host)
    requestHeader.headers.get("GESTALT-META-BASE-URL").getOrElse(inferredUrl)
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

trait MetaController extends SecurityResources with MetaControllerUtils with JsonInput with PolicyMethods { this: SecureController =>
  
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
  def handleExpandType(rs: Seq[GestaltResourceType], qs: Map[String, Seq[String]], metaUrl: String) = {
    val expansion = if (QueryString.singleBoolean(qs, "withprops")) {
      (r: GestaltResourceType, _: Option[String]) => {
        val typeJson = Output.renderResourceTypeOutput(r)
        TypeMethods.withPropertiesExpanded(r.id, typeJson, metaUrl)
      }
    } else Output.renderResourceTypeOutput(_,_)
    handleExpand(rs, qs, Some(metaUrl))(expansion)
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

  
  protected [controllers] def updatePolicyJson(policyJson: JsValue, parent: UUID)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]) = {
    val link = Json.toJson(parentLink(parent, Some(META_URL)).get)
    JsonUtil.withJsonPropValue(policyJson.as[JsObject], "parent", link)
  }  

  
  protected[controllers] def ResourceNotFound(typeId: UUID, id: UUID) = 
    NotFoundResult(s"${ResourceLabel(typeId)} with ID '$id' not found.")  
  
  /**
   * Convert a string FQON to corresponding Org UUID.
   */
  def fqid(fqon: String): UUID = orgFqon(fqon) map { _.id } getOrElse {
    throw new ResourceNotFoundException(s"Org with FQON '$fqon' not found.")
  }

  def updateFailedBackendCreate(caller: AuthAccountWithCreds, metaResource: GestaltResourceInstance, ex: Throwable) = {
    log.error(s"Setting state of resource '${metaResource.id}' to FAILED")
    val failstate = ResourceState.id(ResourceStates.Failed)
    ResourceFactory.update(metaResource.copy(state = failstate), caller.account.id)
    HandleExceptions(ex)
  }

  type JsonPayloadTransform = (JsValue, Map[String, String]) => Try[JsValue]
  
  val payloadTransforms: Map[UUID, JsonPayloadTransform] = Map(
    ResourceIds.Environment -> transformEnvironment,
    ResourceIds.Policy      -> transformPolicy,
    ResourceIds.RuleLimit   -> transformRule,
    ResourceIds.RuleEvent   -> transformRule
  )
  
  def transformPayload(tpe: UUID, org: UUID, parent: UUID, payload: JsValue): Try[JsValue] = {
    payloadTransforms.get(tpe).fold(Try(payload)) { f =>
      val data = Map("org" -> org.toString, "parent" -> parent.toString, "typeId" -> tpe.toString)
      f(payload, data)
    }
  }  
  
  def transformEnvironment(json: JsValue, data: Map[String, String]): Try[JsValue] = {
    for {
      a <- normalizeResourceType(json, ResourceIds.Environment)
      b <- upsertProperty(a, "workspace", JsString(data("parent")))
      c <- normalizeEnvironmentType(b)
    } yield c      
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


  private[controllers] def newDefaultResourceResult(org: UUID, tpe: UUID, parent: UUID, payload: JsValue)(
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]) = Future {

    val json = transformPayload(tpe, org, parent, payload).get

    MetaCreate(org, tpe, parent, Some(json)).apply { resource =>

      CreateWithEntitlements(org, request.identity, resource, Some(parent)) match {
        case Failure(err) => HandleExceptions(err)
        case Success(res) => Created(RenderSingle(res))
      }
    }
  }

  private[controllers] def newDefaultResource(org: UUID, tpe: UUID, parent: UUID, payload: JsValue)
                                             (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]): Future[GestaltResourceInstance] = {
    val json = transformPayload(tpe, org, parent, payload).get
    MetaCreateResource(org, tpe, parent, Some(json)).apply { resource =>
      Future.fromTry(CreateWithEntitlements(org, request.identity, resource, Some(parent)))
    }
  }


  def metaRequest(org: UUID, json: JsValue, resourceType: UUID, resourceParent: UUID, action: String)
                 (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,_]): MetaRequest = {

    object actions extends ActionMethods
    val resource = jsonToResource(org, request.identity, json, Some(resourceType)).get
    actions.prefixFromResource(resource).fold {
      throw new RuntimeException(s"Could not find action prefix for type '${resourceType}'")
    }{ prefix =>
      val fqaction = "%s.%s".format(prefix, action)
      MetaRequest(request.identity, resource, resourceParent, fqaction, Some(META_URL))
    }
  }

  def newResourceRequest(org: UUID, resourceType: UUID, resourceParent: UUID, payload: Option[JsValue] = None)(
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]): MetaRequest = {
    val json = payload getOrElse request.body
    metaRequest(org, json, resourceType, resourceParent, "create")
  }

  def MetaCreate(org: UUID, tpe: UUID, parent: UUID, payload: Option[JsValue] = None)(
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]):(GestaltResourceInstance => Result) => Result = {
    val (operations, options) = newResourceRequestArgs {
      newResourceRequest(org, tpe, resourceParent = parent, payload = payload)
    }
    SafeRequest(operations, options).Execute _
  }
  
  def MetaCreateAsync(org: UUID, tpe: UUID, parent: UUID, payload: Option[JsValue] = None)(
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]):(GestaltResourceInstance => Future[Result]) => Future[Result] = {
    val (operations, options) = newResourceRequestArgs {
      newResourceRequest(org, tpe, resourceParent = parent, payload = payload)
    }
    SafeRequest(operations, options).ExecuteAsync _    
  }

  def MetaCreateResource(org: UUID, tpe: UUID, parent: UUID, payload: Option[JsValue] = None)
                        (implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]): (Instance => Future[Instance]) => Future[Instance] = {
    val (operations, options) = newResourceRequestArgs {
      newResourceRequest(org, tpe, resourceParent = parent, payload = payload)
    }
    SafeRequest(operations, options).ExecuteAsyncT[GestaltResourceInstance] _
  }

  private[controllers] def newResourceResult2(
      org: UUID, tpe: UUID, parent: UUID, payload: JsValue)(f: GestaltResourceInstance => Result)(
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]) = Future {
    
    val json = transformPayload(tpe, org, parent, payload).get
    
    MetaCreate(org, tpe, parent, Some(json)).apply { resource =>
      f(resource)
    }
  }
  
  private[controllers] def newResourceResultAsync(
      org: UUID, tpe: UUID, parent: UUID, payload: JsValue)(f: GestaltResourceInstance => Future[Result])(
      implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]) = {
    
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

