package controllers


import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

import scala.util.{Try, Success, Failure}
import org.yaml.snakeyaml.Yaml
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.galacticfog.gestalt.caas.kube._
import com.galacticfog.gestalt.data.{ResourceFactory, ResourceState}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
//import com.galacticfog.gestalt.meta.api.output.gestaltResourceInstanceFormat
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceOwnerLink, ResourceStates}

import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ResourceNotFoundException}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecurity
import com.google.inject.Inject
import controllers.util.SecureController
import javax.inject.Singleton
import controllers.util.{CompiledHelmChart, HandleExceptions, QueryString}

import akka.util.ByteString
import play.api.i18n.MessagesApi
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, Result}

import services.SkuberFactory
import skuber._
import skuber.apps.v1.{ReplicaSet, ReplicaSetList}
import skuber.api.client._
import skuber.json.format._
import skuber.apps.v1beta1._
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt  

import skuber.LabelSelector
import LabelSelector.dsl._
import scala.language.postfixOps
import play.api.mvc.AnyContentAsRaw
import controllers.util._
import controllers.util.QueryString

case class UnsupportedMediaTypeException(message: String) extends RuntimeException
case class NotAcceptableMediaTypeException(message: String) extends RuntimeException


@Singleton
class KubeNativeController @Inject()( 
    messagesApi: MessagesApi,
    sec: GestaltFrameworkSecurity,
    skuberFactory: SkuberFactory )
      extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization with MetaController {
  
  override val log = play.api.Logger(this.getClass)
  
  private type Headers = Map[String, Seq[String]]
  private type YamlString = String
  
  private type FunctionGetSingle[A <: ObjectResource] = () => A
  private type FunctionGetList[K <: KListItem] = () => Future[KList[K]]
  
  implicit val configMapListFmt: Format[ConfigMapList] = ListResourceFormat[ConfigMap]
  implicit val replicaSetListFmt: Format[ReplicaSetList] = ListResourceFormat[ReplicaSet]
  
  
  def createApplicationDeployment(fqon: String, provider: UUID): Action[AnyContent] = AsyncAuditedAny(fqon) { implicit request =>
    val kube = ResourceFactory.findById(provider) getOrElse {
      throw new ResourceNotFoundException(s"KubeProvider with ID '${provider}' not found.")
    }
    
    val contentType = request.contentType.getOrElse("")
    if (isYamlType(contentType)) {    
      val namespace = namespaceOrDefault(request.queryString)
      val creator = request.identity.account.id
      skuberFactory.initializeKube(kube, namespace)
          .flatMap { deploymentResult(fqid(fqon), creator, provider, request, _) }
          .recover { case t: Throwable => HandleExceptions(t) }
    } else {
      Future(UnsupportedMediaType("Content-Type must be a valid YAML Mime type."))
    }        
  }
  
  /**
   * List objects in a Kubernetes cluster
   */
  def get(fqon: String, provider: UUID, path: String): Action[AnyContent] = AsyncAuditedAny(fqon) { implicit request =>
    val kube = ResourceFactory.findById(provider) getOrElse {
      throw new ResourceNotFoundException(s"KubeProvider with ID '${provider}' not found.")
    }
    
    val namespace = namespaceOrDefault(request.queryString)
    skuberFactory.initializeKube(kube, namespace)
        .flatMap {  getResult(path, request, _) }
        .recover { case t: Throwable => HandleExceptions(t) }
  }
  
  /**
   * Delete objects in a Kubernetes cluster
   */
  def delete(fqon: String, provider: UUID, path: String): Action[AnyContent] = AsyncAuditedAny(fqon) { implicit request =>
    val kube = ResourceFactory.findById(provider) getOrElse {
      throw new ResourceNotFoundException(s"KubeProvider with ID '${provider}' not found.")
    }
    
    val namespace = namespaceOrDefault(request.queryString)
    
    skuberFactory.initializeKube(kube, namespace)
        .flatMap {  deleteResult(path, request, _) }
        .recover { case t: Throwable => HandleExceptions(t) }
  }
  
  private def param[T](m: Map[String,Seq[T]], n: String) = QueryString.single[T](m, n)
  
  /**
   * Take the raw payload YAML for a Helm Chart and convert it to a string.
   */
  private def decodeCompiledChart(payload: AnyContentAsRaw): String = {
    val bytes = for {
      r <- payload.asRaw
      b <- r.asBytes()
    } yield b

    bytes.fold {
      throw new BadRequestException("Payload must not be empty")
    }{ b =>
      b.utf8String
    }
  }

  private[controllers] def errorJson(t: Throwable, kind: String, name: String): JsValue = {
    Json.obj(
    "status" -> "failed",
    "kind" -> kind,
    "resource" -> Json.obj(
        "name" -> name),
    "message" -> t.getMessage)
  }
  
  private[controllers] def deploymentResult[R](
      org: UUID,
      creator: UUID,
      provider: UUID, 
      request: SecuredRequest[_,_], 
      context: RequestContext): Future[Result] = {
    
    val payload = request.body.asInstanceOf[AnyContentAsRaw]
    val rawBody = decodeCompiledChart(payload)
    
    // This is a list of each YAML document in the composite input as a YAML String
    val documents: List[String] = CompiledHelmChart.splitToYamlString(rawBody)

    val qs = request.queryString
    val source = QueryString.requiredSingle(qs, "source")
    val release = QueryString.requiredSingle(qs, "releaseName")
    val namespace = QueryString.requiredSingle(qs, "namespace")
    val metaenv = {
    /*
     * TODO: Validate and handle possible 'invalid UUID' error.
     */
      UUID.fromString(QueryString.requiredSingle(qs, "metaEnv"))
    }
    
    log.debug(
      "Query Params: source=[%s], release=[%s], namespace=[%s], meta-env=[%s]".format(
        source, release, namespace, metaenv))    
    
    /*
     * TODO: Validations 
     * - Assert given Kubernetes namespace exists
     * - Assert given Meta Environment ID exists
     */
    
    val output = AppDeploymentData(
      provider = provider,
      status = "n/a",
      source = DeploymentSource(
          source = source, 
          release = release
      ),
      timestamp = java.time.ZonedDateTime.now,
      native_namespace = namespace,
      resources = DeploymentResources(
          KubeDeploymentResources(), 
          MetaDeploymentResources()
      )
    )
    
    /*
     * Iterate over YAML docs, attempt to create each in Kubernetes - pack each
     * response (success or failure) into AppDeployment response
     */    
    val deploymentRecord = documents.foldLeft(output) { (acc, doc) =>
      val result = Try {
        Await.result(createKubeResource(doc, context, request.queryString).transform(
          s => s,
          e => throw new Exception(s"Failed creating object: ${e.getMessage}")), 20.seconds)
      } match {
        // KUBE REQUEST SUCCEEDED - add JSON resource
        case Success(r) => {
          objectResource2Json(r) match {
            case Success(a) => a
            case Failure(e) => errorJson(e, r.kind, r.name)
          }
        }
        // KUBE REQUEST FAILED - add error JSON        
        case Failure(e) => {
          val res = CompiledHelmChart.string2Map(doc)
          val metadata = CompiledHelmChart.toScalaMap(res("metadata"))
          errorJson(e, res("kind").toString, metadata("name").toString)
        }
      }
      acc.withKubeResource(result)
    }
    /*
     * TODO: Re-evaluate how these implicitly created AppDeployments are named.
     * Here we're using the release name and a random UUID.
     * ...
     */
    val deploymentName = "%s-%s".format(releaseName, UUID.randomUUID.toString)
    Future {
      metaAppDeployment(org, metaenv, creator, deploymentName, deploymentRecord) match {
        case Failure(e) => HandleExceptions(e)
        case Success(deployment) => Created(RenderSingle(deployment)(request))
      }
    }
  
  }
  
  /**
   * Create and persist an AppDeployment Resource in Meta.
   * 
   * @return a Success containing the resource created in Meta or a Failure
   */
  private[controllers] def metaAppDeployment(
      org: UUID, env: UUID, creator: UUID, name: String, info: AppDeploymentData): Try[GestaltResourceInstance] = {
      
    ResourceFactory.create(ResourceIds.User, creator)(
        GestaltResourceInstance(
          id = UUID.randomUUID(),
          typeId = migrations.V25.APPDEPLOYMENT_TYPE_ID,
          state = ResourceState.id(ResourceStates.Active),
          orgId = org,
          owner = ResourceOwnerLink(ResourceIds.User, creator.toString),
          name = name,
          properties = Some(Map("data" -> Json.stringify(Json.toJson(info)) ))
        ),
        parentId = Some(env)
     )
  }
  
  def objectResource2Json(r: ObjectResource): Try[JsValue] = Try {
    r match {
      case x: Pod => Json.toJson(x) 
      case x: Service => Json.toJson(x) 
      case x: Deployment => Json.toJson(x) 
      case x: ReplicaSet => Json.toJson(x)
      case x: Secret => Json.toJson(x)
      case x: ConfigMap => Json.toJson(x)
      case x: StatefulSet => Json.toJson(x)
      case x: PersistentVolume => Json.toJson(x)
      case x: PersistentVolumeClaim => Json.toJson(x)
      case x: Namespace => Json.toJson(x)
      case _ => throw new RuntimeException(s"Unknown ObjectResource Type. found: ${r.getClass.getSimpleName}")
    }
  }

  private[controllers] def deleteResult[R](path: String, request: SecuredRequest[_,_], context: RequestContext): Future[Result] = {
    
    val qs = request.queryString
    val headers = request.headers.toMap
    val gracePeriod = QueryString.singleInt(qs, "k8s_gracePeriodSeconds").getOrElse(-1)
    val propagationPolicy = QueryString.single(qs, "k8s_propagationPolicy")
    
    val one = """([a-z]+)/([a-zA-Z0-9_-]+)""".r
    path match {
      case one("pods", nm)         => kubeDelete[Pod](context, nm, gracePeriod)
      case one("secrets", nm)      => kubeDelete[Secret](context, nm, gracePeriod)
      case one("services", nm)     => kubeDelete[Service](context, nm, gracePeriod)
      case one("configmaps", nm)   => kubeDelete[ConfigMap](context, nm, gracePeriod)
      case one("deployments", nm)  => kubeDelete[Deployment](context, nm, gracePeriod)
      case one("replicasets", nm)  => kubeDelete[ReplicaSet](context, nm, gracePeriod)      
      case one("statefulsets", nm) => kubeDelete[StatefulSet](context, nm, gracePeriod)      
      case one("persistentvolumes", nm)       => kubeDelete[PersistentVolume](context, nm, gracePeriod)
      case one("persistentvolumeclaims", nm)  => kubeDelete[PersistentVolumeClaim](context, nm, gracePeriod)
      case one("namespaces", nm) => kubeDelete[Namespace](context, nm, gracePeriod)
      case _ => Future(NotFound(s"$path is not a valid URI."))
    }
  }
  
  def kubeDelete[T <: ObjectResource](context: RequestContext, name: String, grace: Int = -1)(implicit rd: skuber.ResourceDefinition[T]): Future[Result] = {
    context.delete[T](name, grace).transform( 
      s => NoContent, 
      e => {
        println("*****E : " + e)
        new RuntimeException(s"There was an error: ${e.getMessage}")
      })
  }
  
  private[controllers] def getResult[R](path: String, request: SecuredRequest[_,_], context: RequestContext): Future[Result] = {
    log.debug(s"getResult($path,_,_)")
    
    val headers = request.headers.toMap
    /*
     * TODO: Check for k8s_labelSelector queryparam and handle accordingly...
     */
    val lists: PartialFunction[String, Future[Result]] = {
      case "api"         => Future(Ok(api).withHeaders(ContentType("text/plain")))
      case "pods"        => context.list[PodList].map(RenderObject(_, headers))
      case "services"    => context.list[ServiceList].map(RenderObject(_, headers))
      case "deployments" => context.list[DeploymentList].map(RenderObject(_, headers))
      case "replicasets" => context.list[ReplicaSetList].map(RenderObject(_, headers))
      case "secrets"     => context.list[SecretList].map(RenderObject(_, headers))
      case "configmaps"   => context.list[ConfigMapList].map(RenderObject(_, headers))
      case "statefulsets" => context.list[StatefulSetList].map(RenderObject(_, headers))      
      case "persistentvolumes"      => context.list[PersistentVolumeList].map(RenderObject(_, headers))
      case "persistentvolumeclaims" => context.list[PersistentVolumeClaimList].map(RenderObject(_, headers))
      case "namespaces" => context.list[NamespaceList].map(RenderObject(_, headers))
    }
    
    val one = """([a-z]+)/([a-zA-Z0-9_-]+)""".r
    val singles: PartialFunction[String, Future[Result]] = {
      case one("pods", nm)        => context.get[Pod](nm).map(RenderObject(_, headers))
      case one("services", nm)    => context.get[Service](nm).map(RenderObject(_, headers))
      case one("deployments", nm) => context.get[Deployment](nm).map(RenderObject(_, headers))
      case one("replicasets", nm) => context.get[ReplicaSet](nm).map(RenderObject(_, headers)) 
      case one("secrets", nm)     => context.get[Secret](nm).map(RenderObject(_, headers))
      case one("configmaps", nm)   => context.get[ConfigMap](nm).map(RenderObject(_, headers))
      case one("statefulsets", nm) => context.get[StatefulSet](nm).map(RenderObject(_, headers))      
      case one("persistentvolumes", nm)       => context.get[PersistentVolume](nm).map(RenderObject(_, headers))
      case one("persistentvolumeclaims", nm)  => context.get[PersistentVolumeClaim](nm).map(RenderObject(_, headers))
      case one("namespaces", nm) => context.get[Namespace](nm).map(RenderObject(_, headers))
    }
    
    val notfound: PartialFunction[String, Future[Result]] = {
      case e => Future(NotFound(s"$path is not a valid URI."))
    }
    
    (lists orElse singles orElse notfound)(path)    
  }    
  
  private[controllers] def defaultHelmChartReleaseName() = {
    java.time.LocalDateTime.now()
      .format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm-ss"))
      .toString    
  }
  
  private[controllers] def processCreatePayload(payload: JsValue, qs: Map[String, Seq[String]]): JsValue = {
    
    val source = QueryString.single(qs, "source").map(_.trim.toLowerCase)
    source.fold( payload ) {
      _ match {
        case "helm" => {
          // releaseName is required when source is 'helm'
          val release = QueryString.single(qs, "releaseName").getOrElse {
            log.warn("Creating helm-sourced kube resource - no 'releaseName' given")
            defaultHelmChartReleaseName()
          }
          // replace all occurrences of 'RELEASE-NAME' with the given value
          Json.parse {
            payload.toString.replaceAll("RELEASE-NAME", release)
          }       
        }
        case _ => payload
      }
    }
  }
  
  /**
   * Create objects in a Kubernetes cluster
   */
  def post(fqon: String, provider: UUID, path: String): Action[AnyContent] = AsyncAuditedAny(fqon){ implicit request =>
    val kube = ResourceFactory.findById(provider) getOrElse {
      throw new ResourceNotFoundException(s"KubeProvider with ID '${provider}' not found.")
    }    
    val namespace = namespaceOrDefault(request.queryString)
    val headers = request.headers.toMap

    (for {
      context <- skuberFactory.initializeKube(kube, namespace)
      json    <- Future.fromTry(jsonPayload(request.body, request.contentType))
      body = processCreatePayload(json, request.queryString)
      createResponse <- parseKind(body) match {
        case None => Future(BadRequest("Malformed request. Cannot find object 'kind'"))
        case Some(kind) => kind.toLowerCase match {
          case "pod"        => CreateResult(createKubeObject[Pod](body, context), headers)
          case "service"    => CreateResult(createKubeObject[Service](body, context), headers)
          case "deployment" => CreateResult(createKubeObject[Deployment](body, context), headers)
          case "replicaset" => CreateResult(createKubeObject[ReplicaSet](body, context), headers)
          case "secret"     => CreateResult(createKubeObject[Secret](body, context), headers)
          case "configmap"  => CreateResult(createKubeObject[ConfigMap](body, context), headers)
          case "statefulset" => CreateResult(createKubeObject[StatefulSet](body, context), headers)
          case "persistentvolume"       => CreateResult(createKubeObject[PersistentVolume](body, context), headers)
          case "persistentvolumeclaim"  => CreateResult(createKubeObject[PersistentVolumeClaim](body, context), headers)
          case "namespace" => CreateResult(createKubeObject[Namespace](body, context), headers)
          case e => Future(BadRequest(s"Cannot process requests for object kind '$kind'"))
        }
      }
    } yield createResponse) recover {case t: Throwable => HandleExceptions(t)}
  }  
  
  
  private[controllers] def createKubeResource[T <: ObjectResource](
      payload: String, context: RequestContext, qs: Map[String,Seq[String]]): Future[ObjectResource] = {
      
    val newKubeResource = for {
      json <- Future.fromTry(YamlJson.fromYamlString(payload))
      body = processCreatePayload(json, qs)
      
      createResponse <- parseKind(body) match {
        case None => throw new Exception("foo") //Future.failed(new BadRequestException("Malformed request. Cannot find object 'kind'"))
        case Some(kind) => kind.toLowerCase match {
          case "pod"         => createKubeObject[Pod](body, context)
          case "service"     => createKubeObject[Service](body, context)
          case "deployment"  => createKubeObject[Deployment](body, context)
          case "replicaset"  => createKubeObject[ReplicaSet](body, context)
          case "secret"      => createKubeObject[Secret](body, context)
          case "configmap"   => createKubeObject[ConfigMap](body, context)
          case "statefulset" => createKubeObject[StatefulSet](body, context)
          case "persistentvolume"       => createKubeObject[PersistentVolume](body, context)
          case "persistentvolumeclaim"  => createKubeObject[PersistentVolumeClaim](body, context)
          case "namespace" => createKubeObject[Namespace](body, context)
          case e => throw new Exception("bar") //Future.failed(new BadRequestException(s"Cannot process requests for object kind '$kind'"))
        }
      }
    } yield createResponse
    
    newKubeResource
  }
  
  
  
  /**
   * Handles marshaling a Kubernetes object to an appropriate data-type based on Accept headers.
   */
  private[controllers] def CreateResult[A <: ObjectResource](obj: Future[A], hdrs: Headers)(
      implicit fmt: Format[A]): Future[Result] = {
    
    obj.map { o =>
      if (AcceptsJson(hdrs)) 
        Created(Json.toJson(o)).withHeaders(ContentType("application/json"))
      else if (AcceptsYaml(hdrs)) 
        Created(toYaml(o)).withHeaders(ContentType("text/vnd.yaml"))
      else  
        Created(toYaml(o)).withHeaders(ContentType("text/plain"))
    }
  }

  /**
   * Attempt to parse request.body to a JsValue
   */
  private[controllers] def jsonPayload(payload: AnyContent, contentType: Option[String]): Try[JsValue] = {
    contentType match {
      case None => 
        throw UnsupportedMediaTypeException("You must specify header `Content-Type`.")
      case Some(content) => 
        if (isJsonType(content)) Try(payload.asJson.get)
        else if (isYamlType(content)) {
          //
          // TODO: What could go wrong blindly packing bytes into a string and parsing to YAML???
          // :)
          //          
          val byteString = {
            payload.asRaw.get.asBytes().get.decodeString(ByteString.UTF_8)
          }
          
          //YamlJson.fromYamlString(new String(payload.asRaw.get.asBytes().get))
          YamlJson.fromYamlString(new String(byteString))
        }
        else throw UnsupportedMediaTypeException(s"Cannot process content-type '$content'")
    }
  }

  
  val sel = LabelSelector(
  "tier" is "frontend",
  "release" doesNotExist,
  "env" isNotIn List("production", "staging")
)
  
  /**
   * Create an object in Kubernetes
   */
  private[controllers] def createKubeObject[B <: ObjectResource](payload: JsValue, context: RequestContext)(
      implicit fmt: Format[B], rd: ResourceDefinition[B]): Future[B] = {
    
    KubeLoader.fromJsonValue[B](payload) match {
      case Failure(e) => throw e 
      case Success(v) => context.create[B](v)
    }
  }
  
  /**
   * Render Kube object to the correct format based on Accept header.
   * 
   * @param obj a Kubernetes object (skuber)
   * @param hdrs Request.Headers
   * @param default if true, a request without an Accept header will be allowed (returning text
   * formatted as YAML with content-type set to 'text/plain'). If false and Accept header is missing
   * a '406 Unacceptable' response is generated.
   */
  private[controllers] def RenderObject[A](
      obj: A, hdrs: Headers, default: Boolean = true)(implicit fmt: Format[A]) = {
    
    if (AcceptsJson(hdrs)) 
      Ok(Json.toJson(obj)).withHeaders(ContentType("application/json"))
    else if (AcceptsYaml(hdrs)) 
      Ok(toYaml(obj)).withHeaders(ContentType("text/vnd.yaml"))
    else if (default) 
      Ok(toYaml(obj)).withHeaders(ContentType("text/plain"))
    else {
      val mimetypes = (MimeYaml ++ MimeJson).mkString(",") 
      NotAcceptable(s"Acceptable mime-types are: $mimetypes")
    }
  }
  
  /**
   * Convert a Kubernetes object to YAML
   */
  private[controllers] def toYaml[A](objs: A)(implicit fmt: Format[A]): YamlString = {
    val yaml = new Yaml()
    yaml.dump {
      yaml.load(Json.toJson(objs).toString)
    }
  }    

  /**
   * Parse top-level 'kind' property from Kubernetes JSON object.
   */
  private[controllers] def parseKind(js: JsValue): Option[String] = 
     Js.find(js.as[JsObject], "/kind") map { _.as[String] }  

  /**
   * Parse given namespace from querystring - use literal 'default' if none given.
   */
  private[controllers] def namespaceOrDefault(qs: Map[String, Seq[String]]): String = {
    QueryString.single[String](qs, "namespace").getOrElse {
      log.info("No namespace given in query - using 'default'")
      "default"
    }
  }  
  
  private lazy val MimeYaml = List(
    "text/vnd.yaml", // <-- the default
    "text/yaml",
    "text/x-yaml",
    "application/yaml",
    "application/x-yaml")

  private lazy val MimeJson = List(
      "text/json",
      "application/json")

  private def ContentType(mime: String) = "Content-Type" -> mime        
  private def isYamlType(mime: String) = MimeYaml.contains(mime.trim.toLowerCase)
  private def isJsonType(mime: String) = MimeJson.contains(mime.trim.toLowerCase)

  private[controllers] def valueList(hvs: Seq[String]): Seq[String] =
    if (hvs.isEmpty) hvs else hvs.head.split(",").map(_.trim).toList

  private[controllers] def AcceptsYaml(headers: Map[String, Seq[String]]): Boolean = 
    valueList(headers("Accept")).intersect(MimeYaml).nonEmpty

  private[controllers] def AcceptsJson(headers: Map[String, Seq[String]]): Boolean = 
    valueList(headers("Accept")).intersect(MimeJson).nonEmpty
    
  private lazy val api = """
    | /*
    |  * Retrieve data as JSON or YAML
    |  * Set Accept to 'application/json' or 'text/vnd.yaml'
    |  * Defaults to YAML with content type 'text/plain' if not set.
    |  */
    | GET /{fqon}/providers/{id}/kube/pods
    | GET /{fqon}/providers/{id}/kube/pods/{name}
    | GET /{fqon}/providers/{id}/kube/services
    | GET /{fqon}/providers/{id}/kube/services/{name}
    | GET /{fqon}/providers/{id}/kube/deployments
    | GET /{fqon}/providers/{id}/kube/deployments/{name}
    | GET /{fqon}/providers/{id}/kube/replicasets
    | GET /{fqon}/providers/{id}/kube/replicasets/{name}
    | GET /{fqon}/providers/{id}/kube/secrets/{name}
    | 
    | /*
    |  * All POST requests accept JSON or YAML.
    |  * Set Content-Type to 'application/json' or 'text/vnd.yaml'
    |  */
    | POST /{fqon}/providers/{id}/kube/pods
    | POST /{fqon}/providers/{id}/kube/services
    | POST /{fqon}/providers/{id}/kube/deployments
    | POST /{fqon}/providers/{id}/kube/replicasets
    | POST /{fqon}/providers/{id}/kube/secrets
    | POST /{fqon}/providers/{id}/kube/configmaps
    | POST /{fqon}/providers/{id}/kube/statefulsets
    | POST /{fqon}/providers/{id}/kube/persistentvolumes
    | POST /{fqon}/providers/{id}/kube/persistentvolumeclaims
    | POST /{fqon}/providers/{id}/kube/namespaces
    """.stripMargin.trim

}