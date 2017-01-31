package controllers

import java.net.URL
import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Either, Left, Right}
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.parseUUID
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.toLink
import com.galacticfog.gestalt.meta.api.sdk._

import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db.EnvConfig
import play.api.Logger
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import com.galacticfog.gestalt.meta.auth.Authorization

import scala.util.Either
import com.galacticfog.gestalt.keymgr.GestaltFeature

import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import com.galacticfog.gestalt.json.Js


/*
 * 
 * TODO:
 * 
 * -| More refactoring - cleanup and generalize function to translate meta gateway providers
 * -| to laser gateway providers. Need to ensure that properties.external_id is used in any
 * -| call to laser that needs a provider ID.
 * 
 */
import javax.inject.Singleton

@Singleton
class LaserController @Inject()(messagesApi: MessagesApi,
                                env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  //private val log = Logger(this.getClass)
  
  implicit lazy val lambdaProviderInfoFormat = Json.format[LambdaProviderInfo]
  
  case class LambdaProviderInfo(id: String, external_id: String, locations: Seq[JsValue])
  
  lazy val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
  lazy val lambdaConfig = HostConfig.make(new URL(EnvConfig.lambdaUrl))
  lazy val laser = new Laser(
      gatewayConfig, lambdaConfig, 
      Option(EnvConfig.securityKey), 
      Option(EnvConfig.securitySecret))
  
  
  def postApiFqon(fqon: String, parent: UUID) = Authenticate().async(parse.json) { implicit request =>
    orgFqon(fqon).fold(Future( OrgNotFound(fqon) )) { org =>
      createResourceCommon(org.id, parent, ResourceIds.Api, request.body)
    }
  }  
  
  def postApiEndpointFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>    
    Future {
      orgFqon(fqon).fold(OrgNotFound(fqon)) { org =>
        postEndpoint(org.id, parent, request.body) match {
          case Failure(error) => HandleExceptions(error)
          case Success(endpoints) => {
            Created(Json.toJson(endpoints map { ep => Output.renderInstance(ep) } ))
          }
        }
      }
    }
  }

  def postLambdaEndpointFqon(fqon: String, lambda: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      
      orgFqon(fqon).fold(OrgNotFound(fqon)) { org =>
        (for {
          e  <- createLambdaEndpoint(lambda, request.body.as[JsObject])
          es <- postEndpoint(org.id, lambda, e)
        } yield es) match {
          case Failure(error) => HandleExceptions(error)
          case Success(endpoints) => Created(Json.toJson(endpoints map (RenderSingle(_))))
        }
      }
      
      
    }
  }
  
  def createLambdaEndpoint(lambdaId: UUID, json: JsObject): Try[JsObject] = {
    log.debug(s"createLambdaEndpoint($lambdaId, <json>)")
    /*
     * To post /lambdas/:id/endpoints, caller must supply implementation.function
     */
    
    val implPath = "/properties/implementation"
    val implFunctionPath = "/properties/implementation/function"
    
    Try {
      Js.find(json, implFunctionPath).fold {
        throw new BadRequestException(Errors.LAMBDA_NO_IMPLEMENTATION)
      }{ _ =>
        Js.find(json, implPath).get.as[JsObject] ++ Json.obj(
          "type" -> "Lambda", "id" -> lambdaId.toString)
      }
    }.map { impl =>
      val newprops = Js.find(json, "/properties").get.as[JsObject] ++ Json.obj("implementation" -> impl)
      json ++ Json.obj("properties" -> newprops)
    }
  }   
  

  def getImplementationProps(json: JsValue): Try[Map[String,String]] = Try {
    log.debug("--getImplProps")
    val tpe = getJsonField(json, "type") match {
      case None => throw new BadRequestException("Must specify 'type' property.")
      case Some(tpe) => {
        val t = tpe.as[String]
        if (t.trim.toLowerCase != "lambda") {
          throw new BadRequestException(s"Only supporting implementations of type 'Lambda' at this time. found: $tpe")
        }
        t
      }
    }
    
    val id = getJsonField(json, "id") match {
      case None => throw new BadRequestException("Must specify 'id' property.")
      case Some(f) => {
        parseUUID(f.as[String]) getOrElse {
          throw new BadRequestException(s"'id' property must be a valid v4 UUID. found: $f")
        }        
      }
    }
    
    val func = getJsonField(json, "function") match {
      case None => throw new BadRequestException("Must specify 'function' property")
      case Some(f) => f.as[String]
    }
    
    Map("type" -> tpe, "id" -> id, "function" -> func)
  }
  
  /**
   * Get the lambda.properties.implementation as a Map[String,String]
   */
//  def getEndpointImplementation2(json: JsValue): Either[Throwable, Option[Map[String,String]]] = {
//    log.debug("getEndpointImplementation([json])")
//    
//    getJsonPropertyField(json, "implementation") match {
//      case None => Right(None)
//      case Some(impl) => {
//        getImplementationProps(impl) match {
//          case Success(props) => Right(Some(props))
//          case Failure(error) => Left(error)
//        }
//      }
//    }
//  }
  
  def getEndpointImplementation(json: JsValue): Try[Map[String,String]] = Try {
    getJsonPropertyField(json, "implementation").fold{
      throw new RuntimeException(LaserError.LAMBDA_IMPLEMENTATION_NOT_FOUND)
    }{ impl => getImplementationProps(impl).get }
  }
  
  def createLaserEndpoint(input: GestaltResourceInput, api: UUID, upstream: String, provider: JsValue) = {
    val json = toLaserEndpoint(input, api, upstream, provider)
    laser.createEndpoint(json, api.toString) match {
      case Failure(e) => throw e
      case Success(response) => JsonUtil.safeParse[LaserEndpoint](response.output.get)
    }
  }
  
  def findLaserProviderId(metaProviderId: String) = {
    ResourceFactory.findById(metaProviderId).fold {
      throw new RuntimeException(LaserError.GATEWAY_NOT_FOUND(metaProviderId))
    }{ provider => 
      val exid = provider.properties.get("external_id")
      log.debug(s"findLaserProviderId($metaProviderId): external_id == $exid")
      exid
    }
  }
  
  def postEndpoint(org: UUID, parent: UUID, json: JsValue)
      (implicit request: SecuredRequest[JsValue]): Try[Seq[GestaltResourceInstance]] = Try {

    /*
     * apiendpoint.properties.implementation gives us the lambda ID and 
     * the name of the function to call on the lambda.
     */
    val implementation = getEndpointImplementation(json).get
    val (lambda,lambdaFunction) = {
      val lambdaId = implementation("id")
      val lambda = ResourceFactory.findById(ResourceIds.Lambda, lambdaId).fold {
        throw new ResourceNotFoundException(LaserError.LAMBDA_NOT_FOUND(lambdaId))
      }{ resource => Option(resource) }
      (lambda,implementation("function"))       
    }
    
    // Get list of providers from the Lambda.
    val props = lambda.get.properties.get
    
    val providers = JsonUtil.safeParse[Seq[JsValue]](props("providers")) map { p =>
      val metaProviderId = (p \ "id").as[String]
      val laserProviderId = findLaserProviderId(metaProviderId)
      JsonUtil.safeParse[LambdaProviderInfo] {
        (p.as[JsObject] ++ Json.obj("external_id" -> laserProviderId))
      }
    }
    
    val t = providers(0)
    val msg = s"""
      |id          : ${t.id}
      |external_id : ${t.external_id}
      |locations   : ${t.locations}
      t.locations
    """.stripMargin
    println("**********PROVIDER-INFO:\n" + msg)
    
  
    
    /*
     * ps  - list of providers from lambda.properties
     * acc - accumulates 
     */
    def go(ps: Seq[LambdaProviderInfo], acc: Seq[GestaltResourceInstance]): Seq[GestaltResourceInstance] = {
      
      ps match {
        case Nil => acc
        case h :: t => h.locations map { loc =>
          
          val locationName  = parseLocationName(loc)
          val providerObj   = Json.obj("id" -> h.external_id, "location" -> loc)
          val lambdaBaseUrl = lambdaConfig.port.fold {
            s"${lambdaConfig.protocol}://${lambdaConfig.host}"
          }{ port => 
            s"${lambdaConfig.protocol}://${lambdaConfig.host}:${port}" 
          }
          
          val upstream = s"$lambdaBaseUrl/lambdas/${lambda.get.id.toString}/invoke"
          val api = ResourceFactory.findApiId(lambda.get.id, h.id, locationName)
          
          // Use client-supplied ID if given.
          val endpointId = request.body \ "id" match {
            case u: JsUndefined => UUID.randomUUID
            case i => UUID.fromString(i.as[String])
          }
  
          val input = safeGetInputJson(request.body, Some(ResourceIds.ApiEndpoint)).map( in =>
            in.copy(id = Some(endpointId))
          ).get

          // Create LaserEndpoint
          val laserEndpoint = createLaserEndpoint(input, api.get, upstream, providerObj)

          log.debug("Laser endpoint gateway url: " + laserEndpoint.url)
          
          // Inject 'gateway_url' and 'api' properties
          val metaJson = input.copy(
              properties = Some(input.properties.get ++ Map(
                  "gateway_url" -> JsString(laserEndpoint.url.get),
                  "api" -> JsString(api.get.toString))))
                  
          
          
          // Create the ApiEndpoint in Meta
          val metaEndpoint = 
            CreateResource(ResourceIds.User, 
              request.identity.account.id,
              org, 
              Json.toJson(metaJson), 
              request.identity,
              typeId = Some(ResourceIds.ApiEndpoint),
              parentId = Some(parent))
          
          // Write meta_x_laser map
          val out = metaEndpoint match {
            case Failure(err) => throw err
            case Success(enp) => {
              if (lambda.isDefined) {
                ResourceFactory.addEndpointToLambda(enp.id, lambda.get.id, lambdaFunction)
              }
              setNewEntitlements(org, enp.id, request.identity, Some(parent))
              enp
            }
          }
          // Write lambda_x_endpoint map
          ResourceFactory.mapLaserType(ResourceIds.ApiEndpoint, endpointId, endpointId, h.id, locationName)
          out
        }
      }
    }
    go(providers, Seq())    
  }
  
  
  def idFromImpl(json: JsValue): UUID = {
    val tpe = json \ "type" match {
      case u: JsUndefined => throw new BadRequestException("Must specify 'type'")
      case t => t.as[String]
    }
    
    if (tpe.trim.toLowerCase != "lambda") {
      throw new BadRequestException(LaserError.LAMBDA_IMPLEMENTATION_NOT_SUPPORTED)
    }
    
    val id = json \ "id" match {
      case u : JsUndefined => throw new BadRequestException("Must specify 'id'")
      case i => i.as[String]
    }
    
    if (parseUUID(id).isEmpty)
      throw new BadRequestException(s"'implementation.id' property must be a valid v4 UUID. found: $id")
    
    UUID.fromString(id)
  }

  def postLambdaFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>    
    ResourceFactory.findById(ResourceIds.Environment, parent).fold {
      Future(NotFoundResult(s"Environment ID $parent not found."))
    }{ env => 
      Future {
        val json       = request.body.as[JsObject] ++ Json.obj("resource_type" -> ResourceIds.Lambda.toString)
        val org        = fqid(fqon)
        val user       = request.identity
        val target     = jsonToInput(org, user, json)
        val options    = standardRequestOptions(user, parent, target)
        val operations = standardRequestOperations("lambda.create")
        
        SafeRequest (operations, options) Protect { maybeState => 
          createLambdaCommon(org, env)
        }
        
      }
    }
  }
  
  protected[controllers] def createApiCommon(org: UUID, parentId: UUID, json: JsValue)
      (implicit request: SecuredRequest[JsValue]) = {
    CreateResourceResult(ResourceIds.User, request.identity.account.id,
        org, json, request.identity,
        typeId   = Some(ResourceIds.Api),
        parentId = Some(parentId))  
  }
  
  
  protected[controllers] def createLambdaCommon(org: UUID, parent: GestaltResourceInstance)
      (implicit request: SecuredRequest[JsValue]) = {

    safeGetInputJson(request.body, Some(ResourceIds.Lambda)) match {
      case Failure(e)     => BadRequestResult(e.getMessage)
      case Success(input) => {
        
        val lambdaId: UUID = input.id.getOrElse(UUID.randomUUID)

        // Set ID for the Lambda.
        val newjson = injectParentLink(
            request.body.as[JsObject] ++ Json.obj("id" -> lambdaId.toString), parent)
        
        val ps = getProviderInfo(newjson)

        /*
         * TODO: This function needs a lot of help - currently lambdas will be created in laser
         * but creating the API will fail if the request is bad (say the location name is bad).
         * I can either verify location-names first, or is there any reason not to create the
         * APIs first?
         */
        
        val resource = for {
          _ <- createLaserLambdas(lambdaId, input, ps)
          _ <- createApisSynchronized(org, parent.id, lambdaId, input.name, ps)
          c <- createResourceInstance(org, newjson, Some(ResourceIds.Lambda), Some(parent.id))
        } yield c 
        
        resource match {
          case Failure(err) => HandleExceptions(err)
          case Success(res) => {
            setNewEntitlements(org, res.id, request.identity, Some(parent.id))
            Created(Output.renderInstance(res, META_URL))
          }
        }
      }
    }
  }
  
  def injectParentLink(json: JsObject, parent: GestaltResourceInstance) = {
    val parentLink = toLink(parent, None)
    json ++ Json.obj("properties" -> 
      replaceJsonPropValue(json, "parent", Json.toJson(parentLink)))
  }
  
  

  def unprocessable(message: String) = throw BadRequestException(message)
  
  def getProviderInfo(lambdaJson: JsValue): Seq[LambdaProviderInfo] = {
    val providers = lambdaJson \ "properties" \ "providers" match {
      case u: JsUndefined => None
      case j              => Some(j.validate[Seq[JsValue]].get)
    }
    
    providers.get map { p => 
      val id = (p \ "id").as[String]
//      parseUUID(id)
      
      val externalId = for {
        uid      <- Try { parseUUID(id) getOrElse unprocessable(s"Invalid provider ID. found: $id") }
        provider <- Try { ResourceFactory.findById(uid) getOrElse unprocessable(s"Provider with ID '$id' not found") }
        external <- Try { provider.properties.get.get("external_id") getOrElse unprocessable(s"Could not parse 'external_id' from JSON.") }
      } yield external
      
//      val exId = ResourceFactory.findById(id) match {
//        case Some(mp) => mp.properties.get("external_id")
//        case None => throw new RuntimeException(s"Could not find ApiGatewayProvider with ID '$id'")
//      }

      val finalJson = p.as[JsObject] ++ Json.obj("external_id" -> externalId.get)
      Js.parse[LambdaProviderInfo](finalJson).get
      
//      (p.as[JsObject] ++ Json.obj("external_id" -> exId)).validate[LambdaProviderInfo].map {
//        case lpi: LambdaProviderInfo => lpi
//      }.recoverTotal { e =>
//        log.error(Js.errorString(e))
//        throw new RuntimeException(Js.errorString(e))
//      }
      
    }
  }
  
  /*
   * Create one API for each location in each provider in the gestalt-apigateway service.
   */
  def createApisSynchronized(
      org: UUID, 
      parent: UUID, 
      metaLambdaId: UUID, 
      lambdaName: String, 
      providers: Seq[LambdaProviderInfo])(implicit request: SecuredRequest[JsValue]) = Try {

    def go(ps: Seq[LambdaProviderInfo], acc: Seq[UUID]): Seq[UUID] = {
      
      // Iterate over Providers.
      ps match {
        case Nil => acc
        case provider :: t => {
          
          /*
           * For each provider.location:
           *  - Create a new Laser API
           *  - Create a new Meta API
           */
          val newApiIds: Seq[UUID] = provider.locations map { loc =>
            
            val id = UUID.randomUUID
            val locationName = (loc \ "name").as[String]
            
            // Create API in Laser
            val laserjson = LaserApi(id = Some(id), name = lambdaName, description = None,
              provider = Some(Json.obj("id" -> provider.external_id.toString, "location" -> locationName)))

            laser.createApi(laserjson) match {
              case Success(result) => log.debug("Laser API created: " + result)
              case Failure(e) => throw e
            }
            
            // Create API in Meta
            val metaApiJson = toMetaApiInput(lambdaName, id, Json.obj("id" -> provider.id.toString, "location" -> loc))
            val res = createApiCommon(org, parent, Json.toJson(metaApiJson))

            // Record lambda -> API association
            ResourceFactory.mapLambdaApi(metaLambdaId, id, provider.id, locationName)
            
            // Record meta -> laser ID correlation
            ResourceFactory.mapLaserType(ResourceIds.Api, id, id, provider.id, locationName)
            id
          }
          go(t, (acc ++ newApiIds))
        }
      }
    }
    go(providers, Seq())
  }  
  
  
  /*
   * Create one lambda for each location in each provider in gestalt-lambda.
   */
  def createLaserLambdas(
      metaLambdaId: UUID, 
      input: GestaltResourceInput, 
      providers: Seq[LambdaProviderInfo]) = Try {
    log.debug("In createLaserLambdas(...)")
    
    for (p <- providers; l <- p.locations) {
      val laserId = Some(metaLambdaId)
      val locationName = parseLocationName(l)
      val lambda = toLaserLambda(input.copy(id = laserId), p.external_id, locationName)
      
      /*
       * Create the lambda in Laser and record the Meta ID -> Laser ID mapping.
       */
      (laser.createLambda(lambda) map { m =>
        ResourceFactory.mapLaserType(ResourceIds.Lambda, metaLambdaId, laserId.get, p.id, locationName)
      }).get
    }
  }
  
  def parseLocationName(locationJson: JsValue) = {
    (locationJson \ "name") match {
      case u: JsUndefined => throw new BadRequestException(LaserError.ERROR_PARSE_LOCATION)
      case v => v.as[String]
    }
  }
  
  def toMetaApiInput(apiName: String, apiId: UUID, provider: JsValue) = {
    GestaltResourceInput(
      id = Some(apiId),
      name = apiName,
      resource_type = Some(ResourceIds.Api),
      resource_state = Some(ResourceStates.Active),
      properties = Some(Map("provider" -> provider)))
  }

  /**
   * Temporary function - deletes all lambdas from the configured gestalt-lambda
   * and all APIs from the configured gestalt-apigateway. Not for production use
   * but very useful during development.
   */
//  def resetLaser() = Authenticate() { implicit request =>
//    val lambdastats = laser.lambdas map { m =>
//      laser.deleteLambda(m.id.get) match {
//        case Success(_) => (m.id.get, "SUCCESS")
//        case Failure(e) => (m.id.get, "FAILURE: " + e.getMessage)
//      }
//    }
//
//    val apistats = laser.apis map { a =>
//      laser.deleteApi(a.id.get) match {
//        case Success(_) => (a.id.get, "SUCCESS")
//        case Failure(e) => (a.id.get, "FAILURE: " + e.getMessage)
//      }
//    }
//
//    val providerstats = laser.providers map { p =>
//      laser.deleteProvider(p.id.get) match {
//        case Success(_) => (p.id.get, "SUCCESS")
//        case Failure(e) => (p.id.get, "FAILURE: " + e.getMessage)
//      }
//    }
//    
//    val result = Json.obj(
//      "deleted_lambdas" -> Json.toJson(lambdastats.toMap),
//      "deleted_apis" -> Json.toJson(apistats.toMap),
//      "delete_gateway_providers" -> Json.toJson(providerstats.toMap))
//
//    Ok(result)
//  }

  
  private[this] def standardRequestOptions(
    user: AuthAccountWithCreds,
    environment: UUID,
    resource: GestaltResourceInstance,
    data: Option[Map[String, String]] = None) = {

    RequestOptions(user,
      authTarget = Option(environment),
      policyOwner = Option(environment),
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
  object LaserError {
    val LAMBDA_IMPLEMENTATION_NOT_SUPPORTED = "Only supporting implementations of type 'Lambda' at this time."
    val LAMBDA_IMPLEMENTATION_NOT_FOUND = "lambda.properties.implementation not found."
    def LAMBDA_NOT_FOUND(lambdaId: UUID) = s"Lambda with ID '$lambdaId' not found."
    val ERROR_PARSE_LOCATION = "Could not parse 'name' from location. location is a JSON object of form: { name: <string>, enabled: <boolean> }"
    def GATEWAY_NOT_FOUND(providerId: String) = s"Could not find ApiGatewayProvider with ID '$providerId'"
  }  
  
}
