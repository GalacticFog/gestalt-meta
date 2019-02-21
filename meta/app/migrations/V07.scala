package migrations

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceOwnerLink, _}
import com.galacticfog.gestalt.meta.auth._
import com.google.inject.Inject


import play.api.libs.json._
import play.api.Logger

import controllers.util.JsonUtil
import controllers.util.ProviderMethods

import controllers.util.AccountLike

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

import scala.util.{Either/*,Left,Right*/,Try,Success,Failure}

/*
 * TODO: This migration is way over-tasked. Migrations should only modify the meta schema and type-system.
 * This migration creates resource instances as well (a lambda, a stream provider instance, streamspec instance).
 * Instance creation should be handled external to migrations (i.e. the installer or other external script)
 */
/**
 * Create New Types: DataFeed, StreamProvider, and StreamSpec
 */
trait LaserTransformationMigrationV7 {
  // copying this here because I'm in serious doubt that we can keep this migration working while evolving
  // the interface and behaviour of laser lambdas

  private val log = Logger(this.getClass)
  
  implicit lazy val laserApiFormat = Json.format[LaserApi]
  implicit lazy val laserEndpointFormat = Json.format[LaserEndpoint]
  implicit lazy val laserArtifactDescriptionFormat = Json.format[LaserArtifactDescription]
  implicit lazy val laserLambdaFormat = Json.format[LaserLambda]

  sealed trait SecretMount {
    def secret_id: UUID
    def path: String
    def mount_type: String
  }

  sealed trait VolumeSecretMount {
    def path: String
  }
  case class SecretEnvMount(secret_id: UUID, path: String, secret_key: String) extends SecretMount {
    override def mount_type: String = "env"
  }
  case class SecretFileMount(secret_id: UUID, path: String, secret_key: String) extends SecretMount with VolumeSecretMount {
    override def mount_type: String = "file"
  }
  case class SecretDirMount(secret_id: UUID, path: String) extends SecretMount with VolumeSecretMount {
    override def mount_type: String = "directory"
  }

  val secretDirMountReads = Json.reads[SecretDirMount]
  val secretFileMountReads = Json.reads[SecretFileMount]
  val secretEnvMountReads = Json.reads[SecretEnvMount]

  val secretDirMountWrites = Json.writes[SecretDirMount]
  val secretFileMountWrites = Json.writes[SecretFileMount]
  val secretEnvMountWrites = Json.writes[SecretEnvMount]

  implicit val secretMountReads = new Reads[SecretMount] {
    override def reads(json: JsValue): JsResult[SecretMount] = (json \ "mount_type").asOpt[String] match {
      case Some("directory") => secretDirMountReads.reads(json)
      case Some("file")      => secretFileMountReads.reads(json)
      case Some("env")       => secretEnvMountReads.reads(json)
      case _                 => JsError(__ \ "mount_type", "must be one of 'directory', 'file' or 'env'")
    }
  }

  implicit val secretMountWrites = new Writes[SecretMount] {
    override def writes(sm: SecretMount): JsValue = (sm match {
      case env: SecretEnvMount   => secretEnvMountWrites.writes(env)
      case file: SecretFileMount => secretFileMountWrites.writes(file)
      case dir: SecretDirMount   => secretDirMountWrites.writes(dir)
    }) ++ Json.obj("mount_type" -> sm.mount_type)
  }

  case class LaserApi(
      id: Option[String],
      name: String, 
      gatewayId: Option[String] = None,
      provider: Option[JsValue] = None, 
      description: Option[String] = None)

  case class LaserEndpoint( id: Option[String],
                            apiId: String,
                            upstreamUrl: String,
                            path: Option[String],
                            domain: Option[JsValue] = None,
                            url: Option[String] = None,
                            provider: Option[JsValue] = None,
                            endpointInfo: Option[JsValue] = None,
                            authentication: Option[JsValue] = None,
                            methods: Option[Seq[String]] = None,
                            plugins: Option[JsValue] = None,
                            hosts: Option[Seq[String]] = None) {


    def updateWithAuthorization(users: Seq[UUID], groups: Seq[UUID]): LaserEndpoint = {
      val updated = for {
        plugins <- this.plugins.flatMap(_.asOpt[JsObject])
        secPlugin <- (plugins \ "gestaltSecurity").asOpt[JsObject]
        if (secPlugin \ "enabled").asOpt[Boolean].contains(true)
        updatedSecPlugin = secPlugin ++ Json.obj(
          "users" -> users,
          "groups" -> groups
        )
        updatedPlugins = plugins ++ Json.obj(
          "gestaltSecurity" -> updatedSecPlugin
        )
      } yield this.copy(
        plugins = Some(updatedPlugins)
      )
      updated getOrElse this
    }

  }

  case class LaserArtifactDescription(
      artifactUri: Option[String],
      runtime: String,
      handler: String,
      memorySize: Int,
      cpus: Double,
      description: Option[String] = None,
      compressed: Boolean = false,
      publish: Boolean = false,
      role: String = "none",
      timeoutSecs: Int = 180,
      code: Option[String] = None,
      periodicInfo : Option[JsValue] = None,
      headers : Map[String,String] = Map.empty,
      computePathOverride: Option[String] = None,
      secrets: Option[Seq[JsObject]] = None,
      preWarm: Option[Int] = None,
      isolate: Option[Boolean] = None)

  case class LaserLambda(
      id: Option[String], 
      eventFilter: Option[String],
      public: Boolean,
      provider: Option[JsValue],
      artifactDescription: LaserArtifactDescription)

  def toLaserLambda(lambda: ResourceLike, providerId: UUID): Try[LaserLambda] = Try {

    def maybeToBool(s: String): Option[Boolean] = Try{s.toBoolean}.toOption
    def maybeToJson(s: String): Option[JsValue] = Try{Json.parse(s)}.toOption
    def maybeToInt(s: String): Option[Int] = Try{s.toInt}.toOption

    log.debug("toLaserLambda(...)")

    val props = lambda.properties.getOrElse(Map.empty)

    val handler = props.get("handler").getOrElse(throw new BadRequestException("Lambda was missing property: 'handler'"))
    val isPublic = props.get("public").flatMap(maybeToBool) getOrElse false
    val compressed = props.get("compressed").flatMap(maybeToBool) getOrElse false
    val artifactUri = props.get("package_url")
    val periodic = props.get("periodic_info").flatMap(maybeToJson)
    val preWarm = props.get("pre_warm").flatMap(maybeToInt)

    val secretMounts = (for {
      secrets <- props.get("secrets")
      mounts = Json.parse(secrets).as[Seq[SecretMount]]
    } yield mounts).getOrElse(Seq.empty)

    val secrets = secretMounts.map {
      sm => ResourceFactory.findById(ResourceIds.Secret, sm.secret_id).getOrElse(throw new BadRequestException(s"Secret '${sm.secret_id}' does not exist'"))
    }

    val fqon = OrgCache.getFqon(lambda.orgId).getOrElse(
      throw new ConflictException("Could not determine FQON for lambda.")
    )

    val lambdaProviderId = Try{(Json.parse(props("provider")) \ "id").as[UUID]}.getOrElse(
      throw new BadRequestException(s"Lambda '${lambda.id}' did not have valid 'provider' block")
    )
    val lambdaProvider = ResourceFactory.findById(ResourceIds.LambdaProvider, lambdaProviderId).getOrElse(
      throw new BadRequestException(s"Lambda '${lambda.id}' provider did not exist")
    )

    val lambdaCaasProvider = Try{(Json.parse(lambdaProvider.properties.get("config")) \ "env" \ "public" \ "META_COMPUTE_PROVIDER_ID").as[UUID]}.getOrElse(
      throw new ConflictException(s"Lambda '${lambda.id}' provider '${lambdaProviderId}' did not have '.properties.config.env.public.META_COMPUTE_PROVIDER_ID'")
    )

    val lambdaEnvironment = ResourceFactory.findParent(parentType=ResourceIds.Environment, childId=lambda.id).getOrElse(
      throw new ConflictException(s"Could not locate parent Environment for Secret '${lambda.id}'")
    )

    val secretProviders = secrets.map({
      s => Try{(Json.parse(s.properties.get("provider")) \ "id").as[UUID]}.getOrElse(
        throw new ConflictException(s"Secret '${s.id}' did not have valid 'provider' block")
      )
    }).distinct
    if (secretProviders.length > 1) throw new BadRequestException("Secrets must have the same CaaS provider")
    else if (secretProviders.headOption.exists(_ != lambdaCaasProvider)) throw new BadRequestException(s"Lambda '${lambda.id}' provider '${lambdaProviderId}' did not have same CaaS provider as mounted Secrets")

    val secretEnvs = secrets.map({
      s => ResourceFactory.findParent(parentType=ResourceIds.Environment, childId=s.id).map(_.id).getOrElse(throw new ConflictException(s"Could not locate parent Environment for Secret '${s.id}'"))
    }).distinct
    if (secretEnvs.length > 1) throw new BadRequestException("All mounted Secrets must belong to the same Environment")
    else if (secretEnvs.headOption.exists(_ != lambdaEnvironment.id)) throw new BadRequestException(s"Lambda '${lambda.id}' must belong to the same Environment as all mounted Secrets")

    val executorEnv = if (preWarm.exists(_ > 0)) Some(lambdaEnvironment.id) else secretEnvs.headOption

    val computePath = executorEnv.map(controllers.routes.ContainerController.postContainer(fqon, _).url)

    LaserLambda(
      id          = Some(lambda.id.toString),
      eventFilter = Some(UUID.randomUUID.toString),
      public      = isPublic,
      provider    = Some(Json.obj(
        "id" -> providerId.toString,
        "location" -> "",
        "href" -> "/foo/bar"
      )),
      LaserArtifactDescription(
        artifactUri = artifactUri,
        description = props.get("description"),
        handler     = handler,
        memorySize  = props("memory").toInt,
        cpus        = props("cpus").toDouble,
        publish     = false,     // <- currently not used
        role        = "none",    // <- currently not used
        preWarm     = preWarm,
        runtime     = props("runtime"),
        timeoutSecs = props("timeout").toInt,
        compressed  = compressed,
        periodicInfo= periodic,
        code        = props.get("code"),
        headers     = props.get("headers").flatMap(maybeToJson).map(_.as[Map[String,String]]) getOrElse Map.empty,
        isolate     = props.get("isolate").flatMap(maybeToBool),
        secrets = Some(secretMounts.map(Json.toJson(_).as[JsObject])),
        computePathOverride = computePath
      )
    )
  }

}

import com.galacticfog.gestalt.meta.genericactions._

class V7 @Inject()()(implicit actorSystem: ActorSystem, mat: Materializer) extends MetaMigration
 with AuthorizationMethods with LaserTransformationMigrationV7 {

  import V7._

  private implicit val acc = new MessageAccumulator()
  
  private val ENTITLEMENTS = Option(Seq(ResourceIds.Entitlement))
  
  private val SYSTEM_WORKSPACE_NAME = "gestalt-system-workspace"
  private val SYSTEM_ENVIRONMENT_NAME = "gestalt-system-environment"
  val LAMBDA_PROVIDER_TIMEOUT_MS = 20000  
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    val process = if (payload.isEmpty) {
      
      Failure(new RuntimeException("Must provide payload!!!"))
        
    } else { 
        for {
          root <- {
            acc push "Looking up 'root' org"
            ResourceFactory.findRootOrg
          }
          creator <- Try {
            acc push "Looking up creator"
            ResourceFactory.findById(ResourceIds.User, identity) getOrElse {
              throw new RuntimeException(s"Could not locate creator with id '${identity}'")
            }
          }
          _ <- {
            acc push "Adding DataFeed Resource Type to /root/resourcetypes"
            addDataFeedType(root.id, root)
          }
          env <- {
            acc push "Looking up environment for Stream Provider lambda"
            getLambdaEnvironment(root.id, creator)
          }
          
          lamProvider <- {
            lookupLambdaProvider(payload.get)
          }

          lam <- {
            // Create lambda
            acc push "Creating StreamProvider Lambda"
            createStreamProviderLambda(root.id, env, creator, payload.get, lamProvider)
          }
          
          _ <- {
            acc push "Adding StreamProvider Resource Type to /root/resourcetypes"
            addStreamProviderType(root.id, creator)
          }
          _ <- {
            acc push "Creating StreamProvider instance in /root/providers"
            val actions = Seq(
                  "streamspec.create", 
                  "streamspec.update", 
                  "streamspec.delete", 
                  "streamspec.start", 
                  "streamspec.stop"/*, 
                  "streamspec.restart"*/)
                  
            createStreamProviderInstance(root.id, env.id, lam.id, actions, creator, payload.get, lamProvider)
          }
          x <- {
            acc push "Adding StreamSpec Resource Type to /root/resourcetypes"
            addStreamSpecType(root.id, creator)
          }
      } yield x
    }
    handleResultStatus(process)
  }  
  
  private[migrations] def createStreamProviderInstance(
      rootId: UUID, 
      envId: UUID, 
      lambdaId: UUID,
      actions: Seq[String],
      creator: GestaltResourceInstance, 
      payload: JsValue,
      lambdaProvider: GestaltResourceInstance): Try[GestaltResourceInstance] = {
    
    /*
     * Map Lambda IDs/functions to JSON Endpoints for Provider payload.
     */

    val providerUrl = (for {
      props <- lambdaProvider.properties
      config <- Try{Json.parse(props("config")).as[JsObject]}.toOption
      host <- Js.find(config, "/env/public/SERVICE_HOST_OVERRIDE").flatMap(_.asOpt[String])
        .orElse(Js.find(config, "/env/public/SERVICE_HOST").flatMap(_.asOpt[String]))
      port <- Js.find(config, "/env/public/SERVICE_PORT_OVERRIDE").flatMap(_.asOpt[String])
          .orElse(Js.find(config, "/env/public/SERVICE_PORT").flatMap(_.asOpt[String]))
    } yield s"http://$host:$port").getOrElse {
      throw new RuntimeException("Could not find Lambda Provider URL in properties/config/env/public")
    }
    
    // Configure streamspec CRUD and stop/start endpoint
    val crudUrl = s"${providerUrl}/lambdas/${lambdaId}/invokeSync"
    val crudFunctions = actions.map(action => newStreamFunction(action))
    val crudEndpoint = mkGestaltEndpoint(crudUrl, crudFunctions)
    log.debug("CRUD-Endpoint:\n" + Json.prettyPrint(Json.toJson(crudEndpoint)))
    
    // Configure streamspec 'viewstatus' endpoint.
    val statusFunction = mkStatusFunction()
    val statusUrl = s"${providerUrl}/streams/<queryParams.persistenceId>/status"
    val statusEndpoint = mkGestaltEndpoint(statusUrl, Seq(statusFunction))
    log.debug("Status-Endpoint:\n" + Json.prettyPrint(Json.toJson(statusEndpoint)))
    
    // Assemble StreamProvider payload for create.
    val providerPayload = formatStreamProviderJson(
        "default-stream-provider", Seq(crudEndpoint, statusEndpoint), providerUrl)  
    log.debug("StreamProvider Payload : " + Json.prettyPrint(providerPayload))
    
    // Create StreamProvider in Meta
    for {
      prv <- CreateNewResource(
          org = rootId,
          creator = creator,
          json = providerPayload,
          typeId = Option(STREAM_PROVIDER_TYPE_ID),
          parent = Option(rootId))
      _ = setNewResourceEntitlements(rootId, prv.id, creator, Some(rootId))
    } yield prv    
  }

  /**
   * Assemble a GestaltEndpoint for Provider configuration.
   */
  private[migrations] def mkGestaltEndpoint(lambdaUrl: String, functions: Seq[GestaltFunction], kind: String = "http") = {
    GestaltEndpoint(
        kind = kind,
        url = lambdaUrl,
        actions = functions,
        authentication = None)
  }

  /**
   * Assemble final StreamProvider JSON payload
   */
  private[migrations] def formatStreamProviderJson(
      providerName: String, 
      endpoints: Seq[GestaltEndpoint],
      lambdaProviderUrl: String): JsValue = {
    
    Json.obj(
      "name" -> providerName,
      "resource_type" -> STREAM_PROVIDER_TYPE_ID,
      "properties" -> Json.obj(
        "config" -> Json.obj(
            "endpoints" -> Json.toJson(endpoints),
            "lambda_provider_url" -> lambdaProviderUrl
        )
      )
    )
  }  
  
  /**
   * Map StreamProvider function names to GestaltFunction objects.
   */
  private[migrations] def newStreamFunction(action: String): GestaltFunction = {
    action match {
      case "streamspec.create" => mkPostFunction(
          name = "streamspec.create",
          displayName = None,
          description = Some("Create a new StreamSpec"),
          resultCodeGood = 201
      )
      
      case "streamspec.update" => mkPostFunction(
          name = "streamspec.update",
          displayName = None,
          description = Some("Update an existing StreamSpec"),
          resultCodeGood = 202    
      )
      
      case "streamspec.delete" => mkPostFunction(
          name = "streamspec.delete",
          displayName = None,
          description = Some("Delete a StreamSpec"),
          resultCodeGood = 204
      )
      
      case "streamspec.start" => mkPostFunctionUi(
          name = "streamspec.start",
          displayName = Some("Start Stream"),
          description = Some("Start a new StreamSpec instance."),
          resultCodeGood = 202,
          renderType = "none",
          locations = Seq("streamspec.edit"),
          queryParameters = Some(Seq(
              RequestQueryParameter(
                  name = "resource_id",
                  value = Some("id")
              )
          ))
      )
      
      case "streamspec.stop" => mkPostFunctionUi(
          name = "streamspec.stop",
          displayName = Some("Stop Stream"),
          description = Some("Delete a StreamSpec"),
          resultCodeGood = 202,
          renderType = "none",
          locations = Seq("streamspec.instances"),
          queryParameters = Some(Seq(
              RequestQueryParameter(
                name = "resource_id",
                value = Some("definitionId")
              ),
              RequestQueryParameter(
                name = "pid",
                value = Some("persistenceId")
              )
          ))
      )
      case _ => throw new RuntimeException(s"Unknown streamspec action found: '${action}'. This is a bug")
    }
  }
  
  private val JSON_CONTENT = "application/json"
  
  /**
   * Create the 'viewstatus' function configuration.
   */
  private[migrations] def mkStatusFunction(): GestaltFunction = {
    val output = GestaltFunction(
      "streamspec.viewstatus",
      get = Some(GetVerb(
        body = None,
        responses = Seq(FunctionResponse(
          code = 200,
          content_type = Some(JSON_CONTENT))
        )
      ))
    )
    log.debug("VIEW-STATUS ENDPOINT JSON:\n" + Json.prettyPrint(Json.toJson(output)))
    output
  }

  /**
   * Create configuration for POST-type provider function.
   */
  private[migrations] def mkPostFunction(
      name: String, 
      displayName: Option[String], 
      description: Option[String],
      resultCodeGood: Int, 
      queryParameters: Option[Seq[RequestQueryParameter]] = None): GestaltFunction = {
    
    val output = GestaltFunction(
      name, displayName, description,
      post = Some(PostVerb(
        body = Some(RequestBody(
            content_type = JSON_CONTENT
        )),
        responses = Seq(
          FunctionResponse(
            code = resultCodeGood,
            content_type = Some(JSON_CONTENT)
          )
        ),
        query_parameters = queryParameters
      ))
    )
    log.debug("FUNCTION-ENDPOINT :\n" + Json.prettyPrint(Json.toJson(output)))
    output
  }
  
  /**
   * Create configuration for POST-type provider function with UI-integration.
   */
  private[migrations] def mkPostFunctionUi(
      name: String, 
      displayName: Option[String], 
      description: Option[String], 
      resultCodeGood: Int,
      renderType: String,
      locations: Seq[String],
      queryParameters: Option[Seq[RequestQueryParameter]] = None,
      icon: Option[ActionIcon] = None): GestaltFunction = {
    
    val output = GestaltFunction(
      name, 
      displayName, 
      description,
      post = Some(PostVerb(
        body = Some(RequestBody(
            content_type = JSON_CONTENT
        )),
        responses = Seq(
          FunctionResponse(
            code = resultCodeGood,
            content_type = Some(JSON_CONTENT),
            gestalt_ui = Some(GestaltUi(
              render = renderType,
              locations = locations,
              icon = None))
            )
          ),
        query_parameters = queryParameters
        ))
      )
    
    log.debug("FUNCTION-ENDPOINT-UI :\n" + Json.prettyPrint(Json.toJson(output)))
    output
  }
  
  /**
   * Create a new Workspace in the root Org
   */
  private[migrations] def newWorkspace(rootId: UUID, creator: GestaltResourceInstance) = {
    for {
      wrk <- CreateNewResource(
          org = rootId,
          creator = creator,
          json = Json.obj("name" -> SYSTEM_WORKSPACE_NAME),
          typeId = Option(ResourceIds.Workspace),
          parent = Option(rootId))
      _ = setNewResourceEntitlements(rootId, wrk.id, creator, Some(rootId))
    } yield wrk
  }
  
  /**
   * Create a new Environment in the given Workspace
   */
  private[migrations] def newEnvironment(rootId: UUID, workspaceId: UUID, envType: UUID, creator: GestaltResourceInstance) = {
    for { 
      env <- CreateNewResource(
        org = rootId,
        creator = creator,
        json = Json.obj(
          "name" -> SYSTEM_ENVIRONMENT_NAME,
          "properties" -> Json.obj(
            "environment_type" -> envType,
            "workspace" -> workspaceId
          )
        ),
        typeId = Option(ResourceIds.Environment),
        parent = Option(workspaceId))
      _ = setNewResourceEntitlements(rootId, env.id, creator, Some(workspaceId))
    } yield env
  }

  /**
   * Lookup or create the 'system' environment where the Stream Provider lambda will go.
   * /root/gestalt-system-workspace/gestalt-system-environment
   */
  private[migrations] def getLambdaEnvironment(
      rootId: UUID, creator: GestaltResourceInstance): Try[GestaltResourceInstance] = {
    for {
      w <- ResourceFactory.findChildByName(rootId, ResourceIds.Workspace, SYSTEM_WORKSPACE_NAME).fold {
        acc push "gestalt-system Workspace not found. Creating."
          newWorkspace(rootId, creator)
        
        }{ workspace => Try(workspace) }
        
      e <- ResourceFactory.findChildByName(w.id, ResourceIds.Environment, SYSTEM_ENVIRONMENT_NAME).fold {
        
        acc push "gestalt-system Environment not found. Creating."
        val envType = ReferenceFactory.findByName(ResourceIds.EnvironmentType, "production").get        
        newEnvironment(rootId, w.id, envType.id, creator)
        
      }{ environment => Try(environment) }
    } yield e
  }
  
  /**
   * Find and return the Lambda Provider given in the migration payload.
   */
  private[migrations] def lookupLambdaProvider(payload: JsValue): Try[GestaltResourceInstance] = Try {
    val providerId = {
      val jsId = Js.find(payload.as[JsObject], "/lambda/properties/provider/id") getOrElse {
        throw new BadRequestException("Bad migration payload. Missing: 'V7/lambda/properties/provider/id'")
      }
      UUID.fromString(jsId.as[String])
    }

    // Ensure given Lambda Provider Exists
    ResourceFactory.findById(providerId) getOrElse {
      throw new BadRequestException(s"Lambda Provider with ID '${providerId}' not found.")
    }
  }
  
  
  private[migrations] def createStreamProviderLambda(
      orgId: UUID, 
      env: GestaltResourceInstance,
      creator: GestaltResourceInstance, 
      payload: JsValue,      
      lambdaProvider: GestaltResourceInstance): Try[GestaltResourceInstance] = {
    
    // Get JSON for new Lambda from payload (what's passed into the migrate function)
    val lambdaJson = {
      val js = Js.find(payload.as[JsObject], "/lambda") getOrElse {
        throw new BadRequestException("Bad migration payload. Missing 'V7/lambda'.")
      }
      injectParentLink(js.as[JsObject], env)
    }    
    
    val providerMethods = new ProviderMethods()
    val metaLambda = CreateNewResource(
      org = orgId,
      creator = creator,
      json = lambdaJson,
      typeId = Option(ResourceIds.Lambda),
      parent = Option(env.id)
    )
    val metaLambdaWithEnts = metaLambda.flatMap(
      l => Try {
        setNewResourceEntitlements(orgId, l.id, creator, Some(env.id))
      } map (_ => l)
    )

    val laserLambda = metaLambdaWithEnts.flatMap(toLaserLambda(_, lambdaProvider.id.toString))
    val theWholeShebang = laserLambda flatMap { ll =>
      val client = providerMethods.configureWebClient(lambdaProvider, None)

      log.debug("Creating lambda in Laser...")
      log.debug("POSTING:\n")
      log.debug(Json.prettyPrint(Json.toJson(ll)))
      
      val fLaser = client.post("/lambdas", Option(Json.toJson(ll))) flatMap { result =>
        if (Seq(200, 201).contains(result.status)) {
          Future.successful(())
        } else {
          Future.failed(new RuntimeException("Error creating Lambda in backend system: " + result.statusText))
        }
      }
      Try(Await.result(fLaser, LAMBDA_PROVIDER_TIMEOUT_MS.millis))
    }

    theWholeShebang match {
      case Success(_) =>
        metaLambda
      case Failure(e) =>
        metaLambda.foreach { l =>
          val deleteMgr = new HardDeleteInstanceManager[AccountLike](external = Map())
          deleteMgr.delete(l, creator, force = true) match {
            case Success(_) =>
            case Failure(ex) =>
              log.error("Failure deleting meta lambda resource during cleanup")
          }
        }
        Failure(e)
    }
  }
  
  
  def injectParentLink(json: JsObject, parent: GestaltResourceInstance) = {
    val parentLink = toLink(parent, None)
    json ++ Json.obj("properties" -> 
      JsonUtil.replaceJsonPropValue(json, "parent", Json.toJson(parentLink)))
  }
  
  private[migrations] def addDataFeedType(org: UUID, creator: GestaltResourceInstance) = {
    if (TypeFactory.findById(DATA_FEED_TYPE_ID).isDefined) {
      acc push s"ResourceType '${DATA_FEED_TYPE_NAME}' already exists"
      Success(())
    } else Try {
      val owner = ResourceOwnerLink(ResourceIds.User, creator.id)
      SystemType(org, owner,
        typeId = DATA_FEED_TYPE_ID,
        typeName = DATA_FEED_TYPE_NAME,
        desc = Some("Configuration for arbitrary data-feeds."),
        extend = Some(ResourceIds.Configuration)

      ).withTypeProperties(
        TypeProperty("kind", "string", require = "required"),
        TypeProperty("provider", "json", require = "optional")

      ).withActionInfo(
        ActionInfo(
          prefix = "datafeed",
          verbs = Seq.empty)

      ).withLineageInfo(
        LineageInfo(
          parent_types = Seq(ResourceIds.Environment),
          child_types = ENTITLEMENTS)

      ).withApiInfo(
        TypeApiInfo(rest_name = "datafeeds")

      ).save()

      val newtype = TypeFactory.findById(DATA_FEED_TYPE_ID) getOrElse {
        throw new RuntimeException("Failed creating DataFeed Resource Type")
      }

      setTypeLineage(newtype, creator)
    }
  }
  

  private[migrations] def addStreamProviderType(org: UUID, creator: GestaltResourceInstance) = {
    if (TypeFactory.findById(STREAM_PROVIDER_TYPE_ID).isDefined) {
      acc push s"ResourceType '${STREAM_PROVIDER_TYPE_NAME}' already exists"
      Success(())
    } else Try {
      val owner = ResourceOwnerLink(ResourceIds.User, creator.id)
      SystemType(org, owner,
        typeId = STREAM_PROVIDER_TYPE_ID,
        typeName = STREAM_PROVIDER_TYPE_NAME,
        desc = Some("Implements Gestalt data streams."),
        extend = Some(ResourceIds.Provider)

      ).withActionInfo(
        ActionInfo(
          prefix = "provider",
          verbs = Seq.empty)

      ).withLineageInfo(
        LineageInfo(
          parent_types = Seq(
            ResourceIds.Org,
            ResourceIds.Workspace,
            ResourceIds.Environment
          ),
          child_types = ENTITLEMENTS)

      ).withApiInfo(
        TypeApiInfo(rest_name = "providers")

      ).save()

      val newtype = TypeFactory.findById(STREAM_PROVIDER_TYPE_ID) getOrElse {
        throw new RuntimeException("Failed creating StreamProvider Resource Type")
      }
      setTypeLineage(newtype, creator)
    }
  }
  
  private[migrations] def addStreamSpecType(org: UUID, creator: GestaltResourceInstance) = {
    if (TypeFactory.findById(STREAM_SPEC_TYPE_ID).isDefined) {
      acc push s"ResourceType '${STREAM_SPEC_TYPE_NAME}' already exists"
      Success(())
    } else Try {
      val owner = ResourceOwnerLink(ResourceIds.User, creator.id)
      SystemType(org, owner,
        typeId = STREAM_SPEC_TYPE_ID,
        typeName = STREAM_SPEC_TYPE_NAME,
        desc = Some("Gestalt Stream Specification."),
        extend = Some(ResourceIds.Configuration),
        selfProps = Map("is_provider_backed" -> "true")

      ).withTypeProperties(

        TypeProperty("cpus", "float", require = "required"),
        TypeProperty("mem", "int", require = "required"),
        TypeProperty("parallelization", "int", require = "required"),
        TypeProperty("processor", "json", require = "required"),
        TypeProperty("streams", "json::list", require = "optional"),
        TypeProperty("lambda_provider", "json", require = "optional"),
        TypeProperty("provider", "resource::uuid::link", require = "required",
          refersTo = Some(STREAM_PROVIDER_TYPE_ID))

      ).withActionInfo(
        ActionInfo(
          prefix = "streamspec",
          verbs = Seq("start", "stop", "restart", "viewstatus"))

      ).withLineageInfo(
        LineageInfo(
          parent_types = Seq(ResourceIds.Environment),
          child_types = ENTITLEMENTS)

      ).withApiInfo(
        TypeApiInfo(rest_name = "streamspecs")

      ).save()

      val newtype = TypeFactory.findById(STREAM_SPEC_TYPE_ID) getOrElse {
        throw new RuntimeException("Failed creating StreamSpec Resource Type")
      }
      setTypeLineage(newtype, creator)
    }
  }
 
  

}

object V7 {
  val DATA_FEED_TYPE_ID = UUID.fromString("8f875ffc-69ff-48c8-9d6d-f3622b7b1062")
  val DATA_FEED_TYPE_NAME = "Gestalt::Resource::Configuration::DataFeed"

  val STREAM_PROVIDER_TYPE_ID = UUID.fromString("b7f764be-9ae6-4f70-9c42-849cc881125f")
  val STREAM_PROVIDER_TYPE_NAME = "Gestalt::Configuration::Provider::StreamProvider"

  val STREAM_SPEC_TYPE_ID = UUID.fromString("e9e90e0a-4f87-492e-afcc-2cd84057f226")
  val STREAM_SPEC_TYPE_NAME = "Gestalt::Resource::Spec::StreamSpec"  
}