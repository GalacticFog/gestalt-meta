package controllers

import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import org.yaml.snakeyaml.Yaml
import com.galacticfog.gestalt.caas.kube._
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecurityEnvironment
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util.SecureController
import javax.inject.Singleton
import controllers.util.HandleExceptions

import play.api.i18n.MessagesApi
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, Result}
import services.SkuberFactory
import skuber._
import skuber.api.client._
import skuber.ext._
import skuber.json.ext.format._
import skuber.json.format._

case class UnsupportedMediaTypeException(message: String) extends RuntimeException
case class NotAcceptableMediaTypeException(message: String) extends RuntimeException


@Singleton
class KubeController @Inject()( messagesApi: MessagesApi,
                                env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
                                skuberFactory: SkuberFactory )
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  private type Headers = Map[String, Seq[String]]
  private type YamlString = String

  private type FunctionGetSingle[A <: ObjectResource] = () => A
  private type FunctionGetList[K <: KListItem] = () => Future[KList[K]]


  private val api = """
    | /*
    |  * Retrieve data as JSON or YAML
    |  * Set Accept to 'application/json' or 'text/vnd.yaml'
    |  * Defaults to YAML with content type 'text/plain' if not set.
    |  */
    | GET /{fqon}/kube/pods
    | GET /{fqon}/kube/pods/{name}
    | GET /{fqon}/kube/services
    | GET /{fqon}/kube/services/{name}
    | GET /{fqon}/kube/deployments
    | GET /{fqon}/kube/deployments/{name}
    | GET /{fqon}/kube/replicasets
    | GET /{fqon}/kube/replicasets/{name}
    | GET /{fqon}/kube/secrets/{name}
    | 
    | /*
    |  * All POST requests accept JSON or YAML.
    |  * Set Content-Type to 'application/json' or 'text/vnd.yaml'
    |  */
    | POST /{fqon}/kube/pods
    | POST /{fqon}/kube/services
    | POST /{fqon}/kube/deployments
    | POST /{fqon}/kube/replicasets
    | POST /{fqon}/kube/secrets
    """.stripMargin.trim

  /**
   * List objects in a Kubernetes cluster
   */
  def get(fqon: String, provider: UUID, path: String): Action[AnyContent] = Authenticate(fqon).async { implicit request =>
    skuberFactory.initializeKube(provider, "default")
        .flatMap {  getResult(path, request, _) }
        .recover { case t: Throwable => HandleExceptions(t) }
  }

  def getResult[R](path: String, request: SecuredRequest[_], context: RequestContext): Future[Result] = {
    val headers = request.headers.toMap
    val lists: PartialFunction[String, Future[Result]] = {
      case "api"         => Future(Ok(api).withHeaders(ContentType("text/plain")))
      case "pods"        => context.list[PodList].map(RenderObject(_, headers))
      case "services"    => context.list[ServiceList].map(RenderObject(_, headers))
      case "deployments" => context.list[DeploymentList].map(RenderObject(_, headers))
      case "replicasets" => context.list[ReplicaSetList].map(RenderObject(_, headers))
      case "secrets"     => context.list[SecretList].map(RenderObject(_, headers))
    }

    val one = """([a-z]+)/([a-zA-Z0-9_-]+)""".r
    val singles: PartialFunction[String, Future[Result]] = {
      case one("pods", nm)        => context.get[Pod](nm).map(RenderObject(_, headers))
      case one("services", nm)    => context.get[Service](nm).map(RenderObject(_, headers))
      case one("deployments", nm) => context.get[Deployment](nm).map(RenderObject(_, headers))
      case one("replicasets", nm) => context.get[ReplicaSet](nm).map(RenderObject(_, headers)) 
      case one("secrets", nm)     => context.get[Secret](nm).map(RenderObject(_, headers))
    }
    
    val notfound: PartialFunction[String, Future[Result]] = {
      case e => Future(NotFound(s"$path is not a valid URI."))
    }
    
    (lists orElse singles orElse notfound)(path)    
  }

  /**
   * Create objects in a Kubernetes cluster
   */
  def post(fqon: String, provider: UUID, path: String): Action[AnyContent] = Authenticate(fqon).async { implicit request =>
    val headers = request.headers.toMap
    (for {
      context <- skuberFactory.initializeKube(provider, "default")
      body <- Future.fromTry(jsonPayload(request.body, request.contentType))
      createResponse <- parseKind(body) match {
        case None => Future(BadRequest("Malformed request. Cannot find object 'kind'"))
        case Some(kind) => kind.toLowerCase match {
          case "pod"        => CreateResult(createKubeObject[Pod](body, context), headers)
          case "service"    => CreateResult(createKubeObject[Service](body, context), headers)
          case "deployment" => CreateResult(createKubeObject[Deployment](body, context), headers)
          case "replicaset" => CreateResult(createKubeObject[ReplicaSet](body, context), headers)
          case "secrets"    => CreateResult(createKubeObject[Secret](body, context), headers)
          case e => Future(BadRequest(s"Cannot process requests for object kind '$kind'"))
        }
      }
    } yield createResponse) recover {case t: Throwable => HandleExceptions(t)}
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
          // TODO: What could go wrong blindly packing bytes into a string a parsing to YAML??
          //
          YamlJson.fromYamlString(new String(payload.asRaw.get.asBytes().get))
        }
        else throw UnsupportedMediaTypeException(s"Cannot process content-type '$content'")
    }
  }    

  /**
   * Create an object in Kubernetes
   */
  private[controllers] def createKubeObject[B <: ObjectResource](payload: JsValue, context: RequestContext)(
      implicit fmt: Format[B], kind: ObjKind[B]): Future[B] = {
    
    KubeLoader.fromJsonValue[B](payload) match {
      case Failure(e) => throw e 
      case Success(v) => context create[B] v
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

  private lazy val MimeYaml = List(
    "text/vnd.yaml", // <-- the default
    "text/yaml",
    "text/x-yaml",
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

}