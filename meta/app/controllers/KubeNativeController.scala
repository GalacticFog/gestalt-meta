package controllers


import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

import scala.util.{Try, Success, Failure}
import org.yaml.snakeyaml.Yaml
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import com.galacticfog.gestalt.caas.kube._
import com.galacticfog.gestalt.data.{ResourceFactory, ResourceState, parseUUID}
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
import skuber.api.client._
import skuber.apps.v1beta1._

import skuber.apps.v1.{DaemonSet, DaemonSetList, ReplicaSet, ReplicaSetList}
import skuber.batch.{Job, JobList, CronJob, CronJobList}
import skuber.rbac.{Role, RoleBinding}


import skuber.json.format._
import skuber.json.batch.format._
import skuber.json.rbac.format._
//import skuber.json.ext.format._ // <- DaemonSetList

import LabelSelector.dsl._
import scala.language.postfixOps
import play.api.mvc.AnyContentAsRaw
import controllers.util._
import controllers.util.QueryString

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt 

case class UnsupportedMediaTypeException(message: String) extends RuntimeException
case class NotAcceptableMediaTypeException(message: String) extends RuntimeException

/*
 * TODO: A lot of the code here needs to move to KubeNativeMethods. 
 * For now there's some duplication.
 */
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
  
  /*
   * TODO: These shouldn't be necessary - need to dig them out of the appropriate 
   * skuber namespace.
   */
  implicit val configMapListFmt: Format[ConfigMapList] = ListResourceFormat[ConfigMap]
  implicit val replicaSetListFmt: Format[ReplicaSetList] = ListResourceFormat[ReplicaSet]
  implicit val daemonSetListFmt: Format[DaemonSetList] = Json.format[DaemonSetList]
  

  /**
   * List objects in a Kubernetes cluster
   */
  def get(fqon: String, provider: UUID, path: String): Action[AnyContent] = AsyncAuditedAny(fqon) { implicit request =>
    kubeResult(provider, request, path, Seq("api", "view"))(getResult)
  }
  
  /**
   * Delete objects in a Kubernetes cluster
   */
  def delete(fqon: String, provider: UUID, path: String): Action[AnyContent] = AsyncAuditedAny(fqon) { implicit request =>
    kubeResult(provider, request, path)(deleteResult)
  }
  
  def createApplicationDeployment(fqon: String, provider: UUID): Action[AnyContent] = AsyncAuditedAny(fqon) { implicit request =>

    val contentType = request.contentType.getOrElse("")
    
    if (isYamlType(contentType)) {    
      val creator = request.identity.account.id
      initializeContext(provider, request)
          .flatMap { deploymentResult(fqid(fqon), creator, provider, request, _) }
          .recover { case t: Throwable => HandleExceptions(t) }
    } else {
      Future(UnsupportedMediaType("Content-Type must be a valid YAML Mime type."))
    }
  }
  
  type KubeEndpoint = (PathArity, SecuredRequest[_,_], RequestContext) => Future[Result]
  
  sealed trait PathArity {
    val kind: String
  }
  object PathArity {
    private val matchSingle = """^([a-z]+)/([a-zA-Z0-9_-]+)""".r
    private val matchPlural = """^([a-z]+)$""".r
    
    def test(s: String) = {
      s match {
        case matchSingle(kind, name) => Single(kind, name)
        case matchPlural(kind) => Plural(kind)
      }
    }
  }
  case class Single(kind: String, name: String) extends PathArity
  case class Plural(kind: String) extends PathArity
  
  /**
   * Get a reference to an initialized skuber RequestContext.
   * Asserts that given KubeProvider exists, and sets kube namespace to
   * 'default' if none is given in the querystring.
   */
  def initializeContext(kubeProviderId: UUID, request: SecuredRequest[_,_]): Future[RequestContext] = {
    val kube = ResourceFactory.findById(ResourceIds.KubeProvider, kubeProviderId) getOrElse {
      throw new ResourceNotFoundException(s"KubeProvider with ID '${kubeProviderId}' not found.")
    }  
    val namespace = namespaceOrDefault(request.queryString)
    skuberFactory.initializeKube(kube, namespace)    
  }

  /**
   * Execute the given function against kubernetes, returning an HTTP Result
   */
  def kubeResult(
      kubeProviderId: UUID, 
      request: SecuredRequest[_,_], 
      path: String,
      additionalTypes: Seq[String] = Seq.empty[String])(f: KubeEndpoint) = {
    
    val endpoint: PathArity = {
      val p = PathArity.test(path)
      if (Kube.isSupported(p.kind.trim.toLowerCase, additionalTypes)) p 
      else Kube.throwUnsupported(p.kind)
    }

    initializeContext(kubeProviderId, request)
      .flatMap { f(endpoint, request, _) }
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
  
  /**
   * Get metaEnv param from QueryString as a UUID.
   * Validates both that the string is a valid UUID and that the Environment exists.
   */
  private[controllers] def validateMetaEnv(qs: Map[String, Seq[String]]): UUID = {
    val sid = parseUUID(QueryString.requiredSingle(qs, Params.Environment)).getOrElse {
      throw new BadRequestException(s"'${Params.Environment}' query param not found in querystring.")
    }
    ResourceFactory.findById(ResourceIds.Environment, UUID.fromString(sid)).getOrElse {
      throw new BadRequestException(s"Environment ${sid} not found.")
    }.id
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
    val source = QueryString.requiredSingle(qs, Params.Source)
    val release = QueryString.requiredSingle(qs, Params.Release)
    val namespace = QueryString.requiredSingle(qs, Params.Namespace)
    val metaenv = validateMetaEnv(qs)
    
    log.debug(
      "Query Params: source=[%s], release=[%s], namespace=[%s], meta-env=[%s]".format(
        source, release, namespace, metaenv))    
    
    /*
     * TODO: Validations 
     * - Assert given Kubernetes namespace exists
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
        // KUBE REQUEST SUCCEEDED - add resoure serialized to JSON
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
     * TODO: At this point everything has been created in kubernetes (errors aside)
     * Here's where we can kick off 'import'. TBD if that will handle Secrets 
     * as well as Containers.
     */
    val deploymentName = "%s-%s".format(release, UUID.randomUUID.toString)
    Future {
      metaAppDeployment(org, metaenv, creator, deploymentName, 
          deploymentRecord.withStatus(deploymentRecord.getStatus)) match {
        case Failure(e) => HandleExceptions(e)
        case Success(deployment) => {
          Created(RenderSingle(deployment)(request))
        }
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
      case x: ConfigMap => Json.toJson(x)
      case x: CronJob => Json.toJson(x)
      case x: DaemonSet => Json.toJson(x)
      case x: Deployment => Json.toJson(x) 
      case x: Job => Json.toJson(x)
      case x: Namespace => Json.toJson(x)
      case x: PersistentVolume => Json.toJson(x)
      case x: PersistentVolumeClaim => Json.toJson(x)
      case x: Pod => Json.toJson(x) 
      case x: ReplicaSet => Json.toJson(x)
      case x: ReplicationController => Json.toJson(x)
      case x: Role => Json.toJson(x)
      case x: RoleBinding => Json.toJson(x)
      case x: Secret => Json.toJson(x)
      case x: Service => Json.toJson(x) 
      case x: ServiceAccount => Json.toJson(x)
      case x: StatefulSet => Json.toJson(x)
      //case x: Volume => Json.toJson(x)      
      case _ => throw new RuntimeException(s"Unknown ObjectResource Type. found: ${r.getClass.getSimpleName}")
    }
  }
  
  object Kube {
    
    val supportedTypes = Seq(
        "configmap", "container", "cronjob",
        "daemonset", "deployment",
        "job",
        "namespace",
        "persistentvolumeclaim", "persistentvolume", "pod",
        "replicaset", "replicationcontroller", "rolebinding", "role",
        "secret", "serviceaccount", "service", "statefulset"/*,
        "volume"*/)
    
    def isSupported(t: String, extra: Seq[String] = Seq.empty) = {
      val test = t.trim.toLowerCase
      /*
       * TODO: Not the most robust way to test for the plural-case...
       */
      val singular = test.dropRight(1)
      val allSupported = singular +: (supportedTypes ++ extra)
      allSupported.contains(test) || allSupported.contains(singular) 
    }
    
    def throwUnsupported(kind: String) = {
      throw new BadRequestException(
          s"Unknown resource 'kind'. expected one of: ${supportedTypes.mkString("[",",","]")}. found: ${kind}")
    }
    
    object Single {  
      val ConfigMap = "configmap"
      val Container = "container"
      val CronJob = "cronjob"
      val DaemonSet = "daemonset"
      val Deployment = "deployment"
      val Job = "job"
      val Namespace = "namespace"
      val PersistentVolumeClaim = "persistentvolumeclaim"
      val PersistentVolume = "persistentvolume"
      val Pod = "pod"
      val ReplicaSet = "replicaset"
      val ReplicationController = "replicationcontroller"
      val RoleBinding = "rolebinding"
      val Role = "role"
      val Secret = "secret"
      val ServiceAccount = "serviceaccount"
      val Service = "service"
      val StatefulSet = "statefulset"
      val Volume = "volume"
    }
    
    object Plural {
      val ConfigMap = "configmaps"
      val Container = "containers"
      val CronJob = "cronjobs"
      val DaemonSet = "daemonsets"
      val Deployment = "deployments"
      val Job = "jobs"
      val Namespace = "namespaces"
      val PersistentVolumeClaim = "persistentvolumeclaims"
      val PersistentVolume = "persistentvolumes"
      val Pod = "pods"
      val ReplicaSet = "replicasets"
      val ReplicationController = "replicationcontrollers"
      val RoleBinding = "rolebindings"
      val Role = "roles"
      val Secret = "secrets"
      val ServiceAccount = "serviceaccounts"
      val Service = "services"
      val StatefulSet = "statefulsets"
      val Volume = "volumes"
    }
  }
  
  private[controllers] def deleteResult[R](path: PathArity, request: SecuredRequest[_,_], context: RequestContext): Future[Result] = {
    
    val qs = request.queryString
    //val headers = request.headers.toMap
    val gracePeriod = QueryString.singleInt(qs, "k8s_gracePeriodSeconds").getOrElse(-1)
    //val propagationPolicy = QueryString.single(qs, "k8s_propagationPolicy")
    
    path match {
      case Plural(kind) => 
        throw new BadRequestException(s"Bad path: DELETE path must include identifier for type $kind")
      case Single(kind, nm) => kind match {
        case Kube.Plural.Pod         => kubeDelete[Pod](context, nm, gracePeriod)
        case Kube.Plural.Secret      => kubeDelete[Secret](context, nm, gracePeriod)
        case Kube.Plural.Service     => kubeDelete[Service](context, nm, gracePeriod)
        case Kube.Plural.ConfigMap   => kubeDelete[ConfigMap](context, nm, gracePeriod)
        case Kube.Plural.Deployment  => kubeDelete[Deployment](context, nm, gracePeriod)
        case Kube.Plural.ReplicaSet  => kubeDelete[ReplicaSet](context, nm, gracePeriod)
        case Kube.Plural.StatefulSet => kubeDelete[StatefulSet](context, nm, gracePeriod)
        case Kube.Plural.PersistentVolume      => kubeDelete[PersistentVolume](context, nm, gracePeriod)
        case Kube.Plural.PersistentVolumeClaim => kubeDelete[PersistentVolumeClaim](context, nm, gracePeriod)
        case Kube.Plural.Namespace      => kubeDelete[Namespace](context, nm, gracePeriod)
        case Kube.Plural.ServiceAccount => kubeDelete[ServiceAccount](context, nm, gracePeriod)
        case Kube.Plural.Role           => kubeDelete[Role](context, nm, gracePeriod)
        case Kube.Plural.RoleBinding    => kubeDelete[RoleBinding](context, nm, gracePeriod)
        case Kube.Single.CronJob        => kubeDelete[CronJob](context, nm, gracePeriod)
        case Kube.Single.DaemonSet      => kubeDelete[DaemonSet](context, nm, gracePeriod)
        case Kube.Single.Job            => kubeDelete[Job](context, nm, gracePeriod)
        case Kube.Single.ReplicationController   => kubeDelete[ReplicationController](context, nm, gracePeriod)          
         
        //Kube.Plural.Volume => ???
      }
    }
  }
  
  def kubeDelete[T <: ObjectResource](context: RequestContext, name: String, grace: Int = -1)(implicit rd: skuber.ResourceDefinition[T]): Future[Result] = {
    context.delete[T](name, grace).transform( 
      s => NoContent, 
      e => new RuntimeException(s"There was an error: ${e.getMessage}"))
  }
  
  /**
   * Extract Containers from the selected Pods
   */
  def extractContainers[A](
      context: RequestContext, request: SecuredRequest[_,_])(
          implicit fmt: Format[A]): Future[Result] = {
    
    def get() = QueryString.single(request.queryString, Params.Release) match {
      case None => context.list[PodList]
      case Some(lbl) => {
        val selector = new LabelSelector(LabelSelector.IsEqualRequirement(Labels.Release, lbl))
        context.listSelected[PodList](selector)
      }
    }
    
    val containers: Future[List[skuber.Container]] = for {
      pl <- get()
      cs <- Future(pl.flatMap { p => 
        p.spec.map(_.containers) 
      }.flatten)
    } yield cs

    containers.map(RenderObject(_, request.headers.toMap))
  }
  
  

  /**
   * Generate a view of several kubernetes native resources (some that are not
   * represented in Meta). Uses querystring parameters to determine the namespace
   * to query.
   */
  def kubernetesView(request: SecuredRequest[_,_], context: RequestContext) = {
    
    def listResource2Json[L <: ListResource[_]](lst: L): JsValue = {
      if (lst.items.isEmpty) Json.obj()
      else {
        val items = lst.items.map { 
          case a: Deployment => Json.toJson(a)
          case a: Pod => Json.toJson(a)
          case a: ReplicaSet => Json.toJson(a)
          case a: StatefulSet => Json.toJson(a)
          case a: Service => Json.toJson(a)
          case a: Secret => Json.toJson(a)
          case a: ConfigMap => Json.toJson(a)
          case x => throw new BadRequestException(s"Unsupported resource type. found: ${x.getClass.getSimpleName}")
        }
        Json.toJson(items)
      }
    }
    
    val view: Future[Map[String, JsValue]] = Future.sequence(Seq(  
       
      selected2[DeploymentList](request, context).map("deployments" -> listResource2Json(_)),
      selected2[PodList](request, context).map("pods" -> listResource2Json(_)),
      selected2[ReplicaSetList](request, context).map("replicasets" -> listResource2Json(_)),
      selected2[StatefulSetList](request, context).map("statefulsets" -> listResource2Json(_)),
      selected2[ServiceList](request, context).map("services" -> listResource2Json(_)),
      selected2[SecretList](request, context).map ("secrets" -> listResource2Json(_)),
      selected2[ConfigMapList](request, context).map("configmaps" -> listResource2Json(_))
      
    )).transform(
      s => s.foldLeft(Map.empty[String, JsValue]) { (acc, tup) => acc + tup },
      e => throw new RuntimeException(s"Failed retrieving objects from kube: ${e.getMessage}")
    )

    /*
     * TODO: Respect Accept-Header and return YAML if requested.
     */    
    view.map { m =>
      Ok(Json.toJson(m))
    }.recover {
      case e: Throwable => HandleExceptions(e)
    }
  }

  def selected[L <: ListResource[_]](
      request: SecuredRequest[_,_], context: RequestContext)(
          implicit fmt: Format[L], rd: ResourceDefinition[L]) = {
    
    val headers = request.headers.toMap
    QueryString.single(request.queryString, Params.Release) match {
      case None => context.list[L].map(RenderObject(_, headers))
      case Some(label) => {
        val selector = new LabelSelector(LabelSelector.IsEqualRequirement(Labels.Release, label))
        context.listSelected[L](selector).map(RenderObject(_, headers))        
      }
    }
  }

  /**
   * TODO: Refactor to eliminate need for 'selected()' method above.
   * This version is more general
   */
  def selected2[L <: ListResource[_]](
      request: SecuredRequest[_,_], context: RequestContext)(
          implicit fmt: Format[L], rd: ResourceDefinition[L]): Future[L] = {
    
    val headers = request.headers.toMap
    QueryString.single(request.queryString, Params.Release) match {
      case None => context.list[L]
      case Some(label) => {
        val selector = new LabelSelector(LabelSelector.IsEqualRequirement(Labels.Release, label))
        context.listSelected[L](selector)        
      }
    }
  }  
  
  
  private[controllers] def getResult[R](path: PathArity, request: SecuredRequest[_,_], context: RequestContext): Future[Result] = {
    log.debug(s"getResult($path,_,_)")
    
    val headers = request.headers.toMap
    
    val lists: PartialFunction[String, Future[Result]] = {
      case "view"        => kubernetesView(request, context)
      case "api"         => Future(Ok(api).withHeaders(ContentType("text/plain")))
      case Kube.Plural.Pod        => selected[PodList](request, context) 
      case Kube.Plural.Container  => extractContainers[Seq[Container]](context, request)
      //case Kube.Plural.Volume     => ???
      case Kube.Plural.Service    => selected[ServiceList](request, context)
      case Kube.Plural.Deployment => selected[DeploymentList](request, context)
      case Kube.Plural.ReplicaSet => selected[ReplicaSetList](request, context)
      case Kube.Plural.Secret     => selected[SecretList](request, context)
      case Kube.Plural.ConfigMap   => selected[ConfigMapList](request, context)
      case Kube.Plural.StatefulSet => selected[StatefulSetList](request, context)      
      case Kube.Plural.PersistentVolume      => selected[PersistentVolumeList](request, context)
      case Kube.Plural.PersistentVolumeClaim => selected[PersistentVolumeClaimList](request, context)
      case Kube.Plural.Namespace => selected[NamespaceList](request, context)
      case Kube.Plural.Job => selected[JobList](request, context)
      case Kube.Plural.CronJob => selected[CronJobList](request, context)
      case Kube.Plural.DaemonSet => selected[DaemonSetList](request, context)
    }
    
    val one = """([a-z]+)/([a-zA-Z0-9_-]+)""".r
    val singles: PartialFunction[String, Future[Result]] = {
      case one(Kube.Plural.Pod, nm)        => context.get[Pod](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.Service, nm)    => context.get[Service](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.Deployment, nm) => context.get[Deployment](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.ReplicaSet, nm) => context.get[ReplicaSet](nm).map(RenderObject(_, headers)) 
      case one(Kube.Plural.Secret, nm)     => context.get[Secret](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.ConfigMap, nm)   => context.get[ConfigMap](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.StatefulSet, nm) => context.get[StatefulSet](nm).map(RenderObject(_, headers))      
      case one(Kube.Plural.PersistentVolume, nm)       => context.get[PersistentVolume](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.PersistentVolumeClaim, nm)  => context.get[PersistentVolumeClaim](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.Namespace, nm) => context.get[Namespace](nm).map(RenderObject(_, headers))
    }
    
    val notfound: PartialFunction[String, Future[Result]] = {
      case e => Future(NotFound(s"$path is not a valid URI."))
    }
    
    path match {
      case Plural(kind) => lists(kind) 
      case Single(kind, name) => singles(kind)
    }
  }    
  
  /*
   * TODO: The system currently allows a caller to omit the 'releaseName' query param. If
   * omitted we assign a string-formatted timestamp as the name. Not sure this is a great idea.
   * We may want to make 'releaseName' required and avoid surprises. 
   */
  private[controllers] def defaultHelmChartReleaseName() = {
    java.time.LocalDateTime.now()
      .format(java.time.format.DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm-ss"))
      .toString    
  }
  
  /**
   * Pre-process a compiled Helm Chart (validation, variable-replacement, etc.)
   */
  private[controllers] def processHelmChart(payload: JsValue, qs: Map[String, Seq[String]]): JsValue = {
    
    val source = QueryString.single(qs, Params.Source).map(_.trim.toLowerCase)
    source.fold( payload ) {
      _ match {
        case "helm" => {
          // releaseName is required when source is 'helm'
          val release = QueryString.single(qs, Params.Release).getOrElse {
            log.warn(s"Creating helm-sourced kube resource - no '${Params.Release}' given")
            defaultHelmChartReleaseName()
          }
          // replace all occurrences of 'RELEASE-NAME' with the given value
          Json.parse {
            payload.toString.replaceAll("(?i)RELEASE-NAME", release)
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
      body = processHelmChart(json, request.queryString)
      createResponse <- parseKind(body) match {
        case None => Future(BadRequest("Malformed request. Cannot find key 'kind' in payload."))
        case Some(kind) => kind.toLowerCase match {
          case Kube.Single.Pod        => CreateResult(createKubeObject[Pod](body, context), headers)
          case Kube.Single.Service    => CreateResult(createKubeObject[Service](body, context), headers)
          case Kube.Single.Deployment => CreateResult(createKubeObject[Deployment](body, context), headers)
          case Kube.Single.ReplicaSet => CreateResult(createKubeObject[ReplicaSet](body, context), headers)
          case Kube.Single.Secret     => CreateResult(createKubeObject[Secret](body, context), headers)
          case Kube.Single.ConfigMap  => CreateResult(createKubeObject[ConfigMap](body, context), headers)
          case Kube.Single.StatefulSet => CreateResult(createKubeObject[StatefulSet](body, context), headers)
          case Kube.Single.PersistentVolume       => CreateResult(createKubeObject[PersistentVolume](body, context), headers)
          case Kube.Single.PersistentVolumeClaim  => CreateResult(createKubeObject[PersistentVolumeClaim](body, context), headers)
          case Kube.Single.Namespace      => CreateResult(createKubeObject[Namespace](body, context), headers)
          case Kube.Single.CronJob        => CreateResult(createKubeObject[CronJob](body, context), headers)
          case Kube.Single.DaemonSet      => CreateResult(createKubeObject[DaemonSet](body, context), headers)
          case Kube.Single.Role           => CreateResult(createKubeObject[Role](body, context), headers)
          case Kube.Single.RoleBinding    => CreateResult(createKubeObject[RoleBinding](body, context), headers)
          case Kube.Single.Job            => CreateResult(createKubeObject[Job](body, context), headers)
          case Kube.Single.ServiceAccount => CreateResult(createKubeObject[ServiceAccount](body, context), headers)
          case Kube.Single.ReplicationController   => CreateResult(createKubeObject[ReplicationController](body, context), headers)          
          case e => Future(BadRequest(s"Cannot process requests for object kind '$kind'"))
        }
      }
    } yield createResponse) recover {case t: Throwable => HandleExceptions(t)}
  }  
  
  
  private[controllers] def createKubeResource[T <: ObjectResource](
      payload: String, context: RequestContext, qs: Map[String,Seq[String]]): Future[ObjectResource] = {
      
    val newKubeResource = for {
      json <- Future.fromTry(YamlJson.fromYamlString(payload))
      body = processHelmChart(json, qs)
      
      createResponse <- parseKind(body) match {
        case None => throw new BadRequestException("Malformed request. Cannot find object 'kind'")
        case Some(kind) => kind.toLowerCase match {
          case Kube.Single.Pod         => createKubeObject[Pod](body, context)
          case Kube.Single.Service     => createKubeObject[Service](body, context)
          case Kube.Single.Deployment  => createKubeObject[Deployment](body, context)
          case Kube.Single.ReplicaSet  => createKubeObject[ReplicaSet](body, context)
          case Kube.Single.Secret      => createKubeObject[Secret](body, context)
          case Kube.Single.ConfigMap   => createKubeObject[ConfigMap](body, context)
          case Kube.Single.StatefulSet => createKubeObject[StatefulSet](body, context)
          case Kube.Single.PersistentVolume       => createKubeObject[PersistentVolume](body, context)
          case Kube.Single.PersistentVolumeClaim  => createKubeObject[PersistentVolumeClaim](body, context)
          case Kube.Single.Namespace => createKubeObject[Namespace](body, context)
          
          case Kube.Single.CronJob        => createKubeObject[CronJob](body, context)
          case Kube.Single.DaemonSet      => createKubeObject[DaemonSet](body, context)
          case Kube.Single.Role           => createKubeObject[Role](body, context)
          case Kube.Single.RoleBinding    => createKubeObject[RoleBinding](body, context)
          case Kube.Single.Job            => createKubeObject[Job](body, context)
          case Kube.Single.ServiceAccount => createKubeObject[ServiceAccount](body, context)
          case Kube.Single.ReplicationController   => createKubeObject[ReplicationController](body, context)          

          case e => throw new BadRequestException(s"Cannot process requests for object kind '$kind'")
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
  
//  private[controllers] def Render[A](
//      obj: A, hdrs: Headers, default: Boolean = true)(implicit fmt: Format[A]) = {
//    if (AcceptsJson(hdrs))
//      Json.toJson(obj)
//    else if (AcceptsYaml(hdrs))
//      toYaml(obj)
//    else if (default)
//      toYaml(obj)
//    else {
//      val mimetypes = (MimeYaml ++ MimeJson).mkString(",") 
//      throw new BadRequestException(s"Acceptable mime-types are: $mimetypes")
//    }      
//  }
  
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
    
  object Labels {
    val Release = "release"
  }
  
  object Params {
    val Environment = "metaEnv"
    val Namespace = "namespace"
    val Release = "releaseName"
    val Source = "source"
  }    
    
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