package controllers

import java.net.URL
import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Either,Left,Right}

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.parseUUID
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.toLink
import com.galacticfog.gestalt.meta.api.sdk._

import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db.EnvConfig

import play.api.{Logger => log}
import play.api.libs.json._
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import com.galacticfog.gestalt.meta.auth.Authorization

/*
 * 
 * TODO:
 * 
 * -| More refactoring - cleanup and generalize function to translate meta gateway providers
 * -| to laser gateway providers. Need to ensure that properties.external_id is used in any
 * -| call to laser that needs a provider ID.
 * 
 */

object LaserController extends Authorization {
  
  implicit lazy val lambdaProviderInfoFormat = Json.format[LambdaProviderInfo]
  
  case class LambdaProviderInfo(id: String, external_id: String, locations: Seq[JsValue])
  
  lazy val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
  lazy val lambdaConfig = HostConfig.make(new URL(EnvConfig.lambdaUrl))
  
  lazy val laser = new Laser(
      gatewayConfig, lambdaConfig, 
      Option(EnvConfig.securityKey), 
      Option(EnvConfig.securitySecret))
  
  
  def postApi(org: UUID, environment: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    trace(s"postApi($org, $environment)")
    createResourceCommon(org, environment, ResourceIds.Api, request.body)
  }
  
  def postApiFqon(fqon: String, parent: UUID) = Authenticate().async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => {
        
        val apijson = safeGetInputJson(ResourceIds.Api, request.body)
        //val input = toLaserApi(apijson.get)
        
        createResourceCommon(org.id, parent, ResourceIds.Api, request.body)
      }
      case None => Future { OrgNotFound(fqon) }
    }
  }  
  
  def postApiEndpoint(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    postEndpoint(org, parent, request.body)
    Future { Ok("nothing") }
  }
  
  def postApiEndpointFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      orgFqon(fqon) match {
        case Some(org) => postEndpoint(org.id, parent, request.body) match {
          case Success(endpoints) => {
            
            println("\n-----------ENDPOINTS----------")
            endpoints foreach println
            println("------------------------------\n")
            
            Created(Json.toJson(endpoints map { ep => Output.renderInstance(ep) } ))
          }
          case Failure(error) => {
            log.error(error.getMessage)
            error.printStackTrace()
            InternalServerError(error.getMessage)
          }
        }
        case None => OrgNotFound(fqon)
      }  
    }
  }

  def postLambdaEndpointFqon(fqon: String, lambda: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      orgFqon(fqon) match {
        case None => OrgNotFound(fqon)
        case Some(org) => {
          createLambdaEndpoint(lambda, request.body.as[JsObject]) match {
            case Failure(e) => HandleRepositoryExceptions(e)
            case Success(r) => postEndpoint(org.id, lambda, r) match {
              case Success(endpoints) => {
                Created(Json.toJson(endpoints map { ep => Output.renderInstance(ep) }))
              }
              case Failure(error) => {
                log.error(error.getMessage)
                InternalServerError(error.getMessage)
              }
            }
          }
        }
      }
    }
  }
  
  def createLambdaEndpoint(lambdaId: UUID, json: JsObject) = Try {
    trace(s"createLambdaEndpoint($lambdaId, <json>)")
    /*
     * To post /lambdas/:id/endpoints, caller must supply implementation.function
     */
    
    //1.) Ensure caller supplied implementation.function
    println("RECEIVED : " + Json.prettyPrint(json))
    
    val impl = Try {
      json \ "properties" \ "implementation" \ "function" match {
        case u: JsUndefined => 
          throw new BadRequestException("No value for implementation.function was found.")
        case f => (json \ "properties" \ "implementation").as[JsObject] ++ Json.obj(
          "type" -> "Lambda",
          "id" -> lambdaId.toString)
      }
    }
    
    impl match {
      case Failure(e) => throw e
      case Success(i) => {
        val newprops = (json \ "properties").as[JsObject] ++ Json.obj("implementation" -> i)
        println("UPDATED PROPERTIES:")
        println(Json.prettyPrint(newprops))
        println
        json ++ Json.obj("properties" -> newprops)
      }
    }
  }   
  

  def getImplProps(json: JsValue): Try[Map[String,String]] = Try {
    log.debug("--getImpleProps")
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
  
  import scala.util.Either
  
  def getEndpointImplementation(json: JsValue): Either[Throwable, Option[Map[String,String]]] = {
    log.debug("--getEndpointImplementation(...)")
    getJsonPropertyField(json, "implementation") match {
      case None => Right(None)
      case Some(impl) => {
        getImplProps(impl) match {
          case Success(props) => Right(Some(props))
          case Failure(error) => Left(error)
        }
      }
    }
  }

  def createLaserEndpoint(input: GestaltResourceInput, api: UUID, upstream: String, provider: JsValue) = {
    val json = toLaserEndpoint(input, api, upstream, provider)
    laser.createEndpoint(json, api.toString) match {
      case Success(r) => {
        r.output.get.validate[LaserEndpoint].map {
          case lep: LaserEndpoint => {
            lep
          }
        }.recoverTotal { e =>
          throw new RuntimeException("Error parsing Laser Endpoint JSON: " + JsError.toFlatJson(e).toString)
        }
      }
      case Failure(e) => throw e
    }
  }

  def findLaserProviderId(metaProviderId: String) = {
    ResourceFactory.findById(metaProviderId) match {
      case Some(mp) => mp.properties.get("external_id")
      case None => throw new RuntimeException(s"Could not find ApiGatewayProvider with ID '$metaProviderId'")
    }    
  }
  
  def postEndpoint(org: UUID, parent: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = Try {
    log.debug("--postEndpoint(...)")
    val (lambda, impl) = getEndpointImplementation(json) match {
      case Left(err) => throw err
      case Right(props) => {
        log.debug("PROPS:")
        log.debug(props.toString)
        if (props.isEmpty) (None,None) else {
          val id = props.get("id")
          log.debug("id -> " + id)
          (ResourceFactory.findById(ResourceIds.Lambda, id) match {
            case Some(res) => { 
              log.debug("Returning Resource:\n" + res)
              Some(res)
            }
            case None => throw new ResourceNotFoundException(s"Lambda with ID '$id' not found.")
          }, props)
        }
      }
    }

    // Get list of providers from the Lambda.
    val props = lambda.get.properties.get
    val providers = Json.parse(props("providers")).validate[Seq[JsValue]].map {
      case providers: Seq[JsValue] => {
        providers
      }
    }.recoverTotal { e =>
      throw new RuntimeException(JsError.toFlatJson(e).toString)
    } map { p =>
      val metaProviderId = (p \ "id").as[String] 
      val laserProviderId = findLaserProviderId(metaProviderId)
      (p.as[JsObject] ++ Json.obj("external_id" -> laserProviderId)).validate[LambdaProviderInfo].get 
    }
    

    def go(ps: Seq[LambdaProviderInfo], acc: Seq[GestaltResourceInstance]): Seq[GestaltResourceInstance] = {
      
      ps match {
        case Nil => acc
        case h :: t => h.locations map { loc =>
          
          //val locationName = (loc \ "name").as[String]
          
          val locationName = parseLocationName(loc)
          
          val providerObj = Json.obj("id" -> h.id, "location" -> loc)
          val lambdaBaseUrl = lambdaConfig.port match {
            case Some(port) =>
              s"${lambdaConfig.protocol}://${lambdaConfig.host}:${port}"
            case None =>
              s"${lambdaConfig.protocol}://${lambdaConfig.host}"
          }
          val upstream = s"$lambdaBaseUrl/lambdas/${lambda.get.id.toString}/invoke"
          val api = ResourceFactory.findApiId(lambda.get.id, h.id, /*loc*/locationName)
          
          // Use client-supplied ID if given.
          val endpointId = request.body \ "id" match {
            case u: JsUndefined => UUID.randomUUID
            case i => UUID.fromString(i.as[String])
          }
  
          val input = safeGetInputJson(ResourceIds.ApiEndpoint, request.body).map( in =>
            in.copy(id = Some(endpointId))
          ).get
          
          log.debug(Json.prettyPrint{
            Json.obj(
              "provider" -> providerObj,
              "upstream" -> upstream,
              "api" -> api.toString,
              "input" -> input)
          })
          
          // Create LaserEndpoint
          val laserEndpoint = createLaserEndpoint(input, api.get, upstream, providerObj)

          log.debug("Laser endpoint gateway url: " + laserEndpoint.url)
          
          // Inject 'gateway_url' and 'api' properties
          val metaJson = input.copy(
              properties = Some(input.properties.get ++ Map(
                  "gateway_url" -> JsString(laserEndpoint.url.get),
                  "api" -> JsString(api.get.toString))))
          
          // Create meta-endpoint
          val metaEndpoint = 
            CreateResource(ResourceIds.User, 
              request.identity.account.id,
              org, Json.toJson(metaJson), 
              request.identity,
              typeId = Some(ResourceIds.ApiEndpoint),
              parentId = Some(parent))

          // Write meta_x_laser map
          val out = metaEndpoint match {
            case Failure(err) => throw err
            case Success(enp) => {
              if (lambda.isDefined) {
                ResourceFactory.addEndpointToLambda(enp.id, lambda.get.id, impl.get("function"))
              }
              enp
            }
          }
          
          // Write lambda_x_endpoint map
          ResourceFactory.mapLaserType(ResourceIds.ApiEndpoint, endpointId, endpointId, h.id, /*loc*/locationName)
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
      throw new BadRequestException(s"Only supporting implementations of type 'Lambda' at this time. found: $tpe")
    }
    
    val id = json \ "id" match {
      case u : JsUndefined => throw new BadRequestException("Must specify 'id'")
      case i => i.as[String]
    }
    
    if (parseUUID(id).isEmpty)
      throw new BadRequestException(s"'id' property must be a valid v4 UUID. found: $id")
    
    UUID.fromString(id)
  }
  
  def postLambda(org: UUID, parent: UUID) = Authenticate(org).async(parse.json) { implicit request =>
    ResourceFactory.findById(ResourceIds.Environment, parent) match {
      case Some(env) => createLambdaCommon(org, env)
      case None => Future{ NotFoundResult(s"Environment ID $parent not found.") }
    }
  }
  
  
  def postLambdaFqon(fqon: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ResourceFactory.findById(ResourceIds.Environment, parent) match {
      case Some(env) => createLambdaCommon(fqid(fqon), env)
      case None      => Future { NotFoundResult(s"Environment ID $parent not found.") }
    }    
  }

  
  def createApiCommon(org: UUID, parentId: UUID, json: JsValue)(implicit request: SecuredRequest[JsValue]) = {
    CreateResourceResult(ResourceIds.User, request.identity.account.id,
        org, json, request.identity,
        typeId = Some(ResourceIds.Api),
        parentId = Some(parentId))  
  }
  
  
  def createLambdaCommon(org: UUID, parent: GestaltResourceInstance)(implicit request: SecuredRequest[JsValue]) = {
    Future {

      safeGetInputJson(ResourceIds.Lambda, request.body) match {
        case Failure(e) => {
          log.error("BAD REQUEST : " + e)
          BadRequestResult(e.getMessage)
        }
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
            // Create Lambda(s) in Laser
            a <- createLaserLambdas(lambdaId, input, ps)
            
            b <- createApisSynchronized(org, parent.id, lambdaId, input.name, ps)
            c <- createResourceInstance(org, newjson, Some(ResourceIds.Lambda), Some(parent.id))
          } yield c 
          
          resource match {
            case Failure(err) => HandleExceptions(err)
            case Success(res) => Created(Output.renderInstance(res, META_URL))
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
  
  def getProviderInfo(lambdaJson: JsValue): Seq[LambdaProviderInfo] = {
    val providers = lambdaJson \ "properties" \ "providers" match {
      case u: JsUndefined => None
      case j              => Some(j.validate[Seq[JsValue]].get)
    }
    providers.get map { p => 

      val id = (p \ "id").as[String]
      val exId = ResourceFactory.findById(id) match {
        case Some(mp) => mp.properties.get("external_id")
        case None => throw new RuntimeException(s"Could not find ApiGatewayProvider with ID '$id'")
      }
      
      (p.as[JsObject] ++ Json.obj("external_id" -> exId)).validate[LambdaProviderInfo].map {
        case lpi: LambdaProviderInfo => lpi
      }.recoverTotal { e =>
        log.error(JsError.toFlatJson(e).toString)
        throw new RuntimeException(JsError.toFlatJson(e).toString)
      }  
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
      ps match {
        case Nil => acc
        case h :: t => {

          val nids: Seq[UUID] = h.locations map { loc =>
            val id = UUID.randomUUID

            val locationName = (loc \ "name").as[String]
            
            // Create Laser API
            val laserjson = LaserApi(id = Some(id), name = lambdaName, description = None,
              provider = Some(Json.obj("id" -> h.external_id.toString, "location" -> locationName)))

            laser.createApi(laserjson) match {
              case Success(result) => log.debug("Laser API created: " + result)
              case Failure(e) => throw e
            }
            
            // Create api in meta
            val metaApiJson = toMetaApiInput(lambdaName, id, Json.obj("id" -> h.id.toString, "location" -> loc))
            val res = createApiCommon(org, parent, Json.toJson(metaApiJson))

            // Record lambda -> api association
            ResourceFactory.mapLambdaApi(metaLambdaId, id, h.id, locationName)
            
            // Record meta -> laser ID correlation
            ResourceFactory.mapLaserType(ResourceIds.Api, id, id, h.id, locationName)
            id
          }
          go(t, (acc ++ nids))
        }
      }
    }
    go(providers, Seq())
  }  
  
  /*
   * Create one lambda for each location in each provider in gestalt-lambda.
   */
  def createLaserLambdas(metaLambdaId: UUID, input: GestaltResourceInput, providers: Seq[LambdaProviderInfo]) = Try {
    
    log.debug("In createLaserLambdas(...)")
    
    for (p <- providers; l <- p.locations) {
      val laserId = Some(metaLambdaId)
      val locationName = parseLocationName(l)
      val lambda = toLaserLambda(input.copy(id = laserId), p.external_id, locationName)
      
      /*
       * Create the lambda in Laser and record the Meta ID -> Laser ID mapping.
       */
      laser.createLambda(lambda) map { m =>
        ResourceFactory.mapLaserType(ResourceIds.Lambda, metaLambdaId, laserId.get, p.id, locationName)
      }
      
    }
  }
  
  def parseLocationName(locationJson: JsValue) = {
    (locationJson \ "name") match {
      case u: JsUndefined => {
        val msg = "location is a JSON object of form: { name: <string>, enabled: <boolean> }"
        throw new BadRequestException(s"Could not parse 'name' from location. ${msg}")
      }
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
   * Gets the list of API Gateway providers from gestalt-apigateway and transforms them into
   * JSON that can be ingested by Meta.
   * 
   * Currently this is used only in create workspace where we're temporarily creating
   * gateway providers for the workspace at workspace creation. This will probably go away
   * once we add UI support for attaching providers at various points in the tree.
   */
  def getLaserProviders(org: java.util.UUID): Seq[JsObject] = {  
    val ps = laser.providers match {
      case Failure(e) => throw e
      case Success(r) => {
        r.output.get.validate[Seq[LaserProvider]] match {
          case e: JsError => throw new RuntimeException(JsError.toFlatJson(e).toString)
          case p => p.get
        }
      }
    }  

    def extractLocations(response: com.galacticfog.gestalt.laser.ApiResponse): Seq[LaserLocation] = {
      response.output.get.validate[Seq[LaserLocation]] match {
        case e: JsError => throw new RuntimeException(JsError.toFlatJson(e).toString)
        case p => p.get
      }
    }
    
    def go(ps: Seq[LaserProvider], acc: Seq[JsObject]): Seq[JsObject] = {
      ps match {
        case Nil => acc
        case h :: t => {
          laser.providerLocations(h.id.get) match {
            case Failure(e) => throw e
            case Success(ls) => go(t, acc :+ mkProviderJson(org, h, extractLocations(ls)))
          }
        }
      }
    }  
    go(ps, Seq())
  }  
  
  /**
   * TEMPORARY: Creates the provider JSON object used in Lambda
   */
  def mkProviderJson(org: UUID, provider: LaserProvider, locations: Seq[LaserLocation]) = {
    Json.obj(
        "id" -> provider.id.get,
        "name" -> provider.name,
        "org" -> org.toString,
        "resource_type" -> ResourceIds.ApiGatewayProvider,
        "properties" -> Json.obj(
          "locations" -> (locations map { _.name })
        )
    )  
  }
  
  /**
   * Temporary function - deletes all lambdas from the configured gestalt-lambda
   * and all APIs from the configured gestalt-apigateway. Not for production use
   * but very useful during development.
   */
  def resetLaser() = Authenticate() { implicit request =>
    val lambdastats = laser.lambdas map { m =>
      laser.deleteLambda(m.id.get) match {
        case Success(_) => (m.id.get, "SUCCESS")
        case Failure(e) => (m.id.get, "FAILURE: " + e.getMessage)
      }
    }

    val apistats = laser.apis map { a =>
      laser.deleteApi(a.id.get) match {
        case Success(_) => (a.id.get, "SUCCESS")
        case Failure(e) => (a.id.get, "FAILURE: " + e.getMessage)
      }
    }

    val result = Json.obj(
      "deleted_lambdas" -> Json.toJson(lambdastats.toMap),
      "deleted_apis" -> Json.toJson(apistats.toMap))

    Ok(result)
  }
  
}
