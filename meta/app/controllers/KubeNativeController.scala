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
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceOwnerLink, ResourceStates}

import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ResourceNotFoundException}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecurity
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import com.google.inject.Inject
import controllers.util.SecureController
import javax.inject.Singleton
import controllers.util.{CompiledHelmChart, HandleExceptions, QueryString}

import akka.util.ByteString
import play.api.i18n.MessagesApi
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, Result}

import services.kubernetes.SkuberFactory


// apiVersion => v1
import skuber.ConfigMap
import skuber.Namespace
import skuber.PersistentVolume
import skuber.PersistentVolumeClaim
import skuber.Pod
import skuber.ReplicationController
import skuber.Secret
import skuber.Service
import skuber.ServiceAccount
//import skuber.Volume

// apiVersion => apps/v1
import skuber.apps.v1.Deployment
import skuber.apps.v1.DeploymentList
import skuber.apps.v1.DaemonSet
import skuber.apps.v1.DaemonSetList
import skuber.apps.v1.ReplicaSet
import skuber.apps.v1.ReplicaSetList
 
// apiVersion => apps/v1beta1
import skuber.apps.v1beta1.StatefulSet
import skuber.apps.v1beta1.StatefulSetList

// apiVersion => batch/v1
import skuber.batch.Job
import skuber.batch.JobList
import skuber.batch.CronJob
import skuber.batch.CronJobList

// apiVersion => rbac.authorization.k8s.io/v1beta1
import skuber.rbac.Role
import skuber.rbac.RoleList
import skuber.rbac.RoleBinding
import skuber.rbac.RoleBindingList
import skuber.rbac.ClusterRole
import skuber.rbac.ClusterRoleList
import skuber.rbac.ClusterRoleBinding
import skuber.rbac.ClusterRoleBindingList

import skuber.json.format._
import skuber.json.batch.format._
import skuber.json.rbac.format._


import skuber._
import skuber.api.client._

import LabelSelector.dsl._
import scala.language.postfixOps
import play.api.mvc.AnyContentAsRaw
import controllers.util._
import controllers.util.QueryString

import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt 
import com.galacticfog.gestalt.meta.api.ContainerSpec

import com.github.ghik.silencer.silent
//import controllers.util.KubeNativeMethods
import controllers.util.KubeNativeMethods.META_DEPLOYMENT_ANNOTATION

import scala.concurrent.duration.FiniteDuration
  
case class ResourceUnavailableException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
case class UnsupportedMediaTypeException(message: String) extends RuntimeException
case class NotAcceptableMediaTypeException(message: String) extends RuntimeException

case class RetrySpec(retryUnavailable: Boolean = false, delay: FiniteDuration = 3.seconds, attempts: Int = 5)

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
  
  private type KubeEndpoint = (PathArity, SecuredRequest[_,_], RequestContext) => Future[Result]
  
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
      for {
        context <- initializeContext(provider, request)
        result  <- deployHelmChart(fqid(fqon), creator, provider, request, context).map { app =>
          Created(RenderSingle(app)(request))
        }.recover { case t: Throwable => HandleExceptions(t) }
      } yield result
    } else {
      Future(UnsupportedMediaType("Content-Type must be a valid YAML Mime type."))
    }
  }
  
  def viewSupportedResources(fqon: String, providerId: UUID) = Audited(fqon) { implicit request =>
    Ok(KubeDeploymentResources.supportedAsJson)
  }
  
  /**
   * Get a reference to an initialized skuber RequestContext.
   * Asserts that given KubeProvider exists, and sets kube namespace to
   * 'default' if none is given in the querystring.
   */
  def initializeContext(kubeProviderId: UUID, request: SecuredRequest[_,_]): Future[RequestContext] = {
    val kube = ResourceFactory.findById(ResourceIds.KubeProvider, kubeProviderId).getOrElse {
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
  private[controllers] def validateMetaEnv(qs: Map[String, Seq[String]]): GestaltResourceInstance = {
    val sid = parseUUID(QueryString.requiredSingle(qs, Params.Environment)).getOrElse {
      throw new BadRequestException(s"'${Params.Environment}' query param not found in querystring.")
    }
    ResourceFactory.findById(ResourceIds.Environment, UUID.fromString(sid)).getOrElse {
      throw new BadRequestException(s"Environment ${sid} not found.")
    }
  }
  
  case class ResourceContext(
      kind: String, 
      namespace: String, 
      providerId: UUID, 
      orgId: UUID,
      environmentId: UUID, 
      workspace: GestaltResourceInstance, 
      annotation: Map[String,String]) {
    
    val typeId = ResourceContext.typeFromKind(kind)
    
    def withKind(kind: String): ResourceContext = this.copy(kind = kind)
  }
  
  object ResourceContext {
    import migrations.V13.VOLUME_TYPE_ID
    
    def fromChart(kind: String, chart: ChartData): ResourceContext = {
      ResourceContext(
        kind = kind,
        namespace = chart.namespace,
        providerId = chart.provider,
        orgId = chart.org,
        environmentId = chart.environment,
        workspace = chart.workspace,
        annotation = chart.metaAnnotation
      )
    }
    
    def typeFromKind(kind: String): UUID = {
      kind match {
        case "container" => ResourceIds.Container
        case "secret" => ResourceIds.Secret
        case "volume" | "persistentvolume" | "persistentvolumeclaim" => VOLUME_TYPE_ID
        case _ => throw new RuntimeException(s"Unsupported resource kind '$kind'. This is a bug.")
      }
    }
  }
  
  def importResourceMeta(resource: JsValue, creator: AuthAccountWithCreds, context: ResourceContext, retry: RetrySpec = RetrySpec()) = {
    val name = (resource \ "metadata" \ "name").as[String]
    val externalId = formatExternalId(context.kind, context.namespace, name)
    val actionPayload = 
      formatSecretImportActionPayload(name, 
        context.providerId, 
        context.environmentId, 
        externalId, 
        context.workspace, 
        context.annotation)
        
    formatImportJson2(
        actionPayload, s"${context.kind}.import", context.providerId, context.environmentId).map { newJson =>
      CreateWithEntitlements(context.orgId, creator, newJson, context.typeId, Some(context.environmentId)) match {
        case Failure(e) => {
          log.error(s"Failed creating imported ${context.kind} in Meta: ${e.getMessage}")
          throw e
        }
        case Success(imported) => {
          log.info(s"${context.kind} imported successfully.")   
        }
      }
    }
  }  
  
  /*
   * TODO: Move ChartData to own file.
   */
  type YamlDoc = String
  type YamlMap = Map[String, Object]
  
  case class ChartData( 
      queryString: Map[String, Seq[String]],
      source: String, 
      release: String, 
      namespace: String, 
      environment: UUID, 
      creator: AuthAccountWithCreds, 
      raw: String, 
      documents: List[YamlDoc], 
      deploymentId: UUID,
      org: UUID,
      provider: UUID) {

    val metaAnnotation = Map(META_DEPLOYMENT_ANNOTATION -> deploymentId.toString)
    val workspace: GestaltResourceInstance = ResourceFactory.findParent(ResourceIds.Workspace, environment).getOrElse {
      throw new RuntimeException(s"Could not find parent workspace for given environment '${environment}'")
    }
    def withDeploymentId(id: UUID) = this.copy(deploymentId = id)
  }
  
  object ChartData {
    
    def validate(kubeContext: RequestContext, org: UUID, provider: UUID, request: SecuredRequest[_,_]): Future[ChartData] = {
      ChartData.fromRequest(org, provider, request) match {
        case Failure(e) => Future(throw e)
        case Success(chart) => this.validate(kubeContext, chart)
      }
    }
    
    def validate(kubeContext: RequestContext, chart: ChartData, testNamespace: Boolean = true): Future[ChartData] = {
      
      def assertValidResourceKinds(documents: List[YamlDoc]): Unit = {
        documents.map { doc =>
          val yaml = CompiledHelmChart.string2Map(doc)
          val kind = yaml.getOrElse("kind", 
              throw new BadRequestException(
                  s"Chart resource is missing 'kind' property. No changes made.")).toString
          val apiVersion = yaml.getOrElse("apiVersion",
              throw new BadRequestException(
                  s"Chart resource of type '${kind}' is missing 'apiVersion' property. No changes made.")).toString
          val name = CompiledHelmChart.toScalaMap(yaml("metadata")).getOrElse("name",
              throw new BadRequestException(
                  s"Chart resource of type '${kind}' is missing 'name' property. No changes made.")).toString

          log.debug(s"Testing resource for compatibility => [name]: ${name}, [kind]: ${kind}, [apiVersion]: ${apiVersion}")
          KubeDeploymentResources.isSupportedResource(kind, apiVersion) match {
            case Left(e) => throw new BadRequestException(e)
            case Right(_) => ()
          }
        }
        ()
      }

      for {
        _ <- Future.fromTry(Try(assertValidResourceKinds(chart.documents)))
        _ <- Future.fromTry(findSingleDeployment(chart.documents))
        
        result <- testNamespace match {
          case false => Future.successful(chart) 
          case true  => findOrCreateNamespace(kubeContext, chart.namespace).map { _ =>
            log.info("Using Kubernetes namespace: " + chart.namespace)
            chart
          }.recover {
            case e: Throwable =>
              log.error("Failed to find or create Kubernetes namespace for deployment.")
              throw e
          }
        }        
      } yield result
    }
    
    def findSingleDeployment(documents: Seq[YamlDoc]): Try[YamlMap] = Try {
      documents
        .map(doc => CompiledHelmChart.string2Map(doc))
        .find(m => m("kind").toString.toLowerCase == "deployment").getOrElse {
          throw new BadRequestException(s"Chart MUST contain an explicit 'Deployment' object")
        }
    }

    def fromRequest(org: UUID, provider: UUID, request: SecuredRequest[_,_]): Try[ChartData] = Try {
      val qs = request.queryString
      val body = decodeCompiledChart(request.body.asInstanceOf[AnyContentAsRaw])
      val docs = CompiledHelmChart.splitToYamlString(body)
      
      ChartData(
        queryString = qs,
        source = QueryString.requiredSingle(qs, Params.Source),
        release = QueryString.requiredSingle(qs, Params.Release),
        namespace = QueryString.requiredSingle(qs, Params.Namespace),
        environment = validateMetaEnv(qs).id,
        creator = request.identity.asInstanceOf[AuthAccountWithCreds],
        raw = body,
        documents = docs,
        deploymentId = UUID.randomUUID,
        org = org,
        provider = provider
      )
    }
  }  
  
  /**
   * Create a resource in Kubernetes converting the output to JSON.
   */

  private[controllers] def createDeploymentResource(yaml: String, context: RequestContext, qs: Map[String,Seq[String]]): Future[JsValue] = {  
    createKubeResource(yaml, context, qs).map { kubeResource =>
      log.debug(s"Adding resource ${kubeResource.kind}(${kubeResource.name}) to AppDeployment.")
      objectResource2Json(kubeResource) match {
        case Success(a) => a
        case Failure(e) => errorJson(e, kubeResource.kind, kubeResource.name)
      }
    }.recover {
      case e: Throwable => {
        val res = CompiledHelmChart.string2Map(yaml)
        val metadata = CompiledHelmChart.toScalaMap(res("metadata"))
        errorJson(e, res("kind").toString, metadata("name").toString)
      }
    }      
  }
  
  /**
   * Creates all of the Kubernetes resources in a compiled Helm chart - formats the
   * results as a Meta AppDeployment resource.
   */
  private[controllers] def executeChart(context: RequestContext, chart: ChartData, z: AppDeploymentData): Future[AppDeploymentData] = {
    // Split the chart into individual YAML docs.
    val docs = CompiledHelmChart.splitToYamlString(chart.raw)
    docs.foldLeft(Future.successful(z)) { (acc, doc) =>        
      for {
        result <- createDeploymentResource(doc, context, chart.queryString)
        deployment <- acc
      } yield deployment.withKubeResource(result)
    }
  }

  private[controllers] def importResources(resources: Seq[JsValue], creator: AuthAccountWithCreds, context: ResourceContext): Future[Seq[_]] = {
    Future.sequence(resources.map(r => importResourceMeta(r, creator, context)))
  }
    
  private[controllers] def deployHelmChart[R](
      org: UUID,
      creator: UUID,
      provider: UUID, 
      request: SecuredRequest[_,_], 
      context: RequestContext): Future[GestaltResourceInstance] = {

    def metaImportContainer(chart: ChartData, request: SecuredRequest[_,_]): Future[GestaltResourceInstance] = {
      val finalDeploymentName = genReleaseName(findSingleDeployment(chart.documents), chart.release)
      val externalContainerId = formatExternalId("container", chart.namespace, finalDeploymentName)    
      val actionPayload = formatImportActionPayload(finalDeploymentName, chart.provider, chart.environment, externalContainerId)
      val newContainerJson = formatImportJson(actionPayload, "container.import", chart.provider, chart.environment)
      
      Future.fromTry(
        CreateWithEntitlements(org, chart.creator, newContainerJson, ResourceIds.Container, Some(chart.environment)) match {
          case Failure(e) => {
            log.error(s"Failed creating imported container in Meta: ${e.getMessage}")
            throw e
          }
          case Success(importedContainer) => {
            log.debug("Container imported successfully.")
            Success(importedContainer)    
          }
        }
      )
    }    
  
    for {
      // Validate chart, create resources in kube

      chart <- ChartData.validate(context, org, provider, request)
      output = newAppDeployment(chart)
      deploymentData <- executeChart(context, chart, output)
      
      // Import dependent (non-container) resources into Meta

      importableSecrets = deploymentData.resources.kube.secrets.getOrElse(Seq())
      secretContext = ResourceContext.fromChart("secret", chart)
      importableVolumeClaims = deploymentData.resources.kube.persistentvolumeclaims.getOrElse(Seq())    
      volumeContext = secretContext.withKind("volume")
      dependentImports <- Future.sequence(Seq(
          importResources(importableSecrets, chart.creator, secretContext),
          importResources(importableVolumeClaims, chart.creator, volumeContext)))

      // Create final AppDeployment in Meta and import deployment container.

      appName = "%s-%s".format(chart.release, UUID.randomUUID.toString)
      finalApp = deploymentData.withStatus(deploymentData.getStatus)
      deployment <- createMetaAppDeployment(chart, org, appName, finalApp).transform(
        success => {
          log.debug("AppDeployment created successfully.")
          success
        },
        failure => {
          log.error(s"Failed creating AppDeployment in Meta: ${failure.getMessage}")
          failure
        })
      result <- metaImportContainer(chart, request).map(_ => deployment)    
    } yield result
  }
    

  /**
   * Find the Deployment in a collection of YAML documents comprising a Helm Chart.
   * Ensures there is a single Deployment. 
   */
  private[controllers] def findSingleDeployment(documents: List[String]): Map[String,Object] = {
    documents
      .map(doc => CompiledHelmChart.string2Map(doc))
      .find(m => m("kind").toString.toLowerCase == "deployment").getOrElse {
        throw new BadRequestException(s"Chart MUST contain an explicit 'Deployment' object")
      }      
  }
  
  /**
   * Find and replace the 'release-name' token in kube resource YAML. Replaces
   * the Helm-based 'release-name' token in [object].metadata.name. 
   */
  private[controllers] def genReleaseName(yaml: Map[String,Object], releaseName: String): String = {
    CompiledHelmChart.toScalaMap(yaml("metadata"))("name")
      .toString.replaceAll("(?i)RELEASE-NAME", releaseName)      
  }  
  
  /**
   * If given namespace exists in Kubernetes, use it - else create new.
   * TODO: Possibly use a query param to control dynamic namespace creation.
   */  
  private[controllers] def findOrCreateNamespace(context: RequestContext, namespace: String): Future[Namespace] = {
    for {
      maybeExistingNamespace <- context.getOption[Namespace](namespace)
      targetNamespace        <- {
        if (maybeExistingNamespace.isDefined) {
          log.info(s"Found existing namespace '${namespace}' for deployment")
          Future.successful(maybeExistingNamespace.get)
        } else {
          // Given namespace does not exist - create it.
            val json = YamlJson.fromYamlString {
                s"""
                |apiVersion: v1
                |kind: Namespace
                |metadata:
                |  name: ${namespace}
                """.stripMargin
              }
            log.info(s"Creating new Kubernetes namespace '${namespace}' for deployment")
            createKubeObject[Namespace](json.get, context)
        }
      }
    } yield targetNamespace      
  }  
  
  /*
   * Format properties.external_id for Meta Resources deployed to kubernetes.
   */
  private[controllers] def formatExternalId(objectType: String, namespace: String, objectName: String): String = {
    objectType match {
      case "container" => s"/namespaces/${namespace}/deployments/${objectName}"
      case "secret"    => s"/namespaces/${namespace}/secrets/${objectName}"
      case "volume"    => s"/namespaces/${namespace}/persistentvolumeclaims/${objectName}"
      case _ => throw new RuntimeException(s"Invalid objectType '${objectType}' - this is a bug")
    }
  }  
  
  private[controllers] def newAppDeployment(chart: ChartData) = {
    AppDeploymentData(
      provider = chart.provider,
      status = "n/a",
      source = DeploymentSource(
          source = chart.source, 
          release = chart.release
      ),
      timestamp = java.time.ZonedDateTime.now,
      native_namespace = chart.namespace,
      resources = DeploymentResources(
          KubeDeploymentResources(), 
          MetaDeploymentResources()
      )
    )      
  }  
  
  
  def formatSecretImportActionPayload(
      name: String, 
      provider: UUID, 
      envid: UUID, 
      externalId: String, 
      workspace: GestaltResourceInstance,
      variables: Map[String,String] = Map.empty): JsObject = {
    
    val json = Json.obj(
        "id"   -> UUID.randomUUID.toString,
        "name" -> name,
        "properties" -> Json.obj(
            "external_id" -> externalId,
            "provider" -> Json.obj(
                "id" -> provider.toString
            )
         ),
        "variables" -> Json.toJson(variables)
    )
    contextObject(workspace.id, json)
  }
  
  def contextObject(workspaceId: UUID, resourceJson: JsObject): JsObject = {
    Json.obj(
        "context" -> Json.obj(
            "org" -> Json.obj("fqon" -> "" /* not currently used */),
            "workspace" -> Json.obj("id" -> workspaceId)),
        "resource" -> resourceJson)
  }
  
  def formatImportActionPayload(name: String, provider: UUID, envid: UUID, externalId: String) = {
    // Get ID of parent workspace
    val workspaceId = ResourceFactory.findParent(ResourceIds.Workspace, envid).fold {
      throw new RuntimeException(s"Could not find parent workspace for environment '${envid}'")
    }{ w => w.id }
    
    Json.obj(
      "context" -> Json.obj(
        "org" -> Json.obj("fqon" -> "" /* not currently used */),
        "workspace" -> Json.obj("id" -> workspaceId)
       ),
       "resource" -> Json.obj(
          "name" -> name,
          "properties" -> Json.obj(
            "image" -> "n/a",
            "container_type" -> "n/a",
            "external_id" -> externalId,
            "provider" -> Json.obj(
                "id" -> provider.toString
             )
           )
        )
     )
  }
  
  def selectByRelease(context: RequestContext, releaseName: String): List[skuber.Container] = {
    log.debug(s"Importing containers by label => 'release: ${releaseName}'")
    val selector = new LabelSelector(LabelSelector.IsEqualRequirement(Labels.Release, releaseName))
          
    val containers: Future[List[skuber.Container]] = for {
      pl <- context.listSelected[PodList](selector)
      cs <- Future(pl.flatMap { p => 
        p.spec.map(_.containers) 
      }.flatten)
    } yield cs

    Await.result(containers, 10.seconds)
  }   
  
  /**
   * Create and persist an AppDeployment Resource in Meta.
   * 
   * @return a Success containing the resource created in Meta or a Failure
   */
  private[controllers] def createMetaAppDeployment(
      uuid: UUID,
      org: UUID, env: UUID, creator: UUID, name: String, info: AppDeploymentData): Try[GestaltResourceInstance] = {
      
    ResourceFactory.create(ResourceIds.User, creator)(
        GestaltResourceInstance(
          id = uuid,
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

  private[controllers] def createMetaAppDeployment(
      chart: ChartData, org: UUID, name: String, info: AppDeploymentData): Future[GestaltResourceInstance] = {
      
    Future.fromTry {
      ResourceFactory.create(ResourceIds.User, chart.creator.account.id)(
          GestaltResourceInstance(
            id = chart.deploymentId,
            typeId = migrations.V25.APPDEPLOYMENT_TYPE_ID,
            state = ResourceState.id(ResourceStates.Active),
            orgId = org,
            owner = ResourceOwnerLink(ResourceIds.User, chart.creator.account.id.toString),
            name = name,
            properties = Some(Map("data" -> Json.stringify(Json.toJson(info)) ))
          ),
          parentId = Some(chart.environment)
       )
    }
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
      case x: ClusterRole => Json.toJson(x)
      case x: ClusterRoleBinding => Json.toJson(x)
      //case x: Volume => Json.toJson(x)      
      case _ => throw new RuntimeException(s"Unknown ObjectResource Type. found: ${r.getClass.getSimpleName}")
    }
  }
  
  
  private[controllers] def deleteResult[R](path: PathArity, request: SecuredRequest[_,_], context: RequestContext): Future[Result] = {
    
    val qs = request.queryString
    val gracePeriod = QueryString.singleInt(qs, "k8s_gracePeriodSeconds").getOrElse(-1)
    
    //val headers = request.headers.toMap    
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
        case Kube.Single.ClusterRole    => kubeDelete[ClusterRole](context, nm, gracePeriod)
        case Kube.Single.ClusterRoleBinding    => kubeDelete[ClusterRoleBinding](context, nm, gracePeriod)
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
          case x => throw new BadRequestException(
              s"Unsupported resource type. found: ${x.getClass.getSimpleName}")
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
      case Kube.Plural.Role => selected[RoleList](request, context)
      case Kube.Plural.RoleBinding => selected[RoleBindingList](request, context)
      case Kube.Plural.DaemonSet => selected[DaemonSetList](request, context)
      case Kube.Plural.ClusterRole    => selected[ClusterRoleList](request, context)
      case Kube.Plural.ClusterRoleBinding => selected[ClusterRoleBindingList](request, context)
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
      case one(Kube.Plural.Job, nm) => context.get[Job](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.CronJob, nm) => context.get[CronJob](nm).map(RenderObject(_, headers))      
      case one(Kube.Plural.DaemonSet, nm) => context.get[DaemonSet](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.ClusterRole, nm) => context.get[ClusterRole](nm).map(RenderObject(_, headers))
      case one(Kube.Plural.ClusterRoleBinding, nm) => context.get[ClusterRoleBinding](nm).map(RenderObject(_, headers))
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
          case Kube.Single.ClusterRole           => CreateResult(createKubeObject[ClusterRole](body, context), headers)
          case Kube.Single.ClusterRoleBinding    => CreateResult(createKubeObject[ClusterRoleBinding](body, context), headers)          
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
          case Kube.Single.Namespace      => createKubeObject[Namespace](body, context)
          case Kube.Single.CronJob        => createKubeObject[CronJob](body, context)
          case Kube.Single.DaemonSet      => createKubeObject[DaemonSet](body, context)
          case Kube.Single.Role           => createKubeObject[Role](body, context)
          case Kube.Single.RoleBinding    => createKubeObject[RoleBinding](body, context)
          case Kube.Single.Job            => createKubeObject[Job](body, context)
          case Kube.Single.ServiceAccount => createKubeObject[ServiceAccount](body, context)
          case Kube.Single.ReplicationController   => createKubeObject[ReplicationController](body, context)
          case Kube.Single.ClusterRole           => createKubeObject[ClusterRole](body, context)
          case Kube.Single.ClusterRoleBinding    => createKubeObject[ClusterRoleBinding](body, context)          

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
  

  def initKube(providerResource: GestaltResourceInstance, namespaceValue: String) = {
    Await.result(skuberFactory.initializeKube(providerResource, namespaceValue), 5.seconds)
  }

  def getMetaId(obj: skuber.ObjectResource, envId: String, uuidLabel: String): UUID = {
    log.debug(s"Testing import status of ${obj.kind}(${obj.metadata.name}])")
    log.debug("given labels: " + obj.metadata.labels)
    
    // Assert that resource exists in Meta
    val idInMeta = obj.metadata.labels.get(s"meta/${uuidLabel}").map(UUID.fromString(_)) getOrElse {
      throw new RuntimeException(s"Resource ${obj.kind}(${obj.metadata.name}) in namespace ${obj.ns} has not been imported to gestalt")
    }
    
    // Get the environment the resource is actually in
    val objEnvId = obj.metadata.labels.get("meta/environment").getOrElse("")
    
    // Ensure the
    if(envId != objEnvId) {
      throw new RuntimeException(
          s"Resource ${obj.name} (env. id=${objEnvId}) and resource being imported (env. id=${envId}) must belong to the same gestalt environment")
    }
    idInMeta
  }  

  import akka.actor.ActorSystem
  import akka.pattern.Patterns.after
  
  lazy val VOLUMECLAIM_BINDING_ATTEMPTS = 5
  lazy val VOLUMECLAIM_BINDING_DELAY = 3.seconds  
  
  //import com.galacticfog.gestalt.data.TypeFactory
  import controllers.util.TypeMethods
  
  private def formatImportJson2(
      payload: JsObject,
      action: String,
      providerId: UUID,
      envId: UUID): Future[JsValue] = {
    
    log.debug(s"formatImportJson2(_, ${action}, _, _)")
    
    val providerResource = ResourceFactory.findById(ResourceIds.KubeProvider, providerId).getOrElse {
      throw new RuntimeException(s"Could not find KubeProvider '${providerId}'.")
    }
    val providerTypeName = TypeMethods.typeName(providerResource.typeId).getOrElse {
      throw new RuntimeException(s"Provider with type ID '${providerResource.typeId}' not found. This is a bug.")
    }
    
    val providerLink = Json.obj(
      "name" -> providerResource.name,          
      "id" -> providerResource.id.toString,
      "resource_type" -> providerTypeName)
   
    val inputProps = action match {
      case "container.import" => Json.obj("provider" -> providerLink)
      case "secret.import" => Json.obj("provider" -> providerId)
      case "volume.import" => Json.obj("provider" -> providerId)
      case _ => throw new RuntimeException(s"Invalid action '${action}'. This is a bug")
    }
    
    val contextLabels = Json.obj(
      "meta/fqon"        -> (payload \ "context" \ "org" \ "fqon").as[String],
      "meta/workspace"   -> (payload \ "context" \ "workspace" \ "id").as[String],
      "meta/environment" -> envId.toString,
      "meta/provider"    -> providerId.toString
    )
    
    val resource = (payload \ "resource").as[JsObject]
    val importTarget = (resource \ "properties" \ "external_id").as[String]
    
    val fResp = (action, importTarget.split("/", 5)) match {
      case ("container.import", Array("", "namespaces", namespaceValue, "deployments", deplNameValue)) => {
        log.debug(s"Looking up Container data => namespace: $namespaceValue, deployment: $deplNameValue")
      /*
       * TODO: This is used as /id of container. secret already include id in the resource payload
       * Normalize this.
       */        
        val uuid = UUID.randomUUID
        val kube = initKube(providerResource, namespaceValue)
        importContainer(kube, envId.toString, namespaceValue, deplNameValue, contextLabels, (resource ++ Json.obj("id" -> uuid.toString)))
      }

      case ("secret.import", Array("", "namespaces", namespaceValue, "secrets", secretName)) => {
        log.debug(s"Looking up Secret data => namespace: $namespaceValue, secret: $secretName")
        
        val kube = initKube(providerResource, namespaceValue)
        importSecret(kube, namespaceValue, secretName, contextLabels, resource)
      }
      
      case ("volume.import", Array("", "namespaces", namespaceValue, "persistentvolumeclaims", pvcName)) => {
        log.debug(s"Looking up Volume data => namespace: $namespaceValue, pvc: $pvcName")

        val kube = initKube(providerResource, namespaceValue)
        val system = ActorSystem()
        val maybeVolume = importDynamicVolumeRetry(kube, namespaceValue, pvcName, contextLabels, resource,
            attempts = VOLUMECLAIM_BINDING_ATTEMPTS,
            delay = VOLUMECLAIM_BINDING_DELAY)(system)
        maybeVolume.onComplete { case _ => system.terminate() }
        maybeVolume
      }
      case _ => 
        throw BadRequestException(s"Invalid combination of action and External ID: (`${action}`, `${importTarget}`)")
    }
    fResp
  }    
  

  private def formatImportJson(
      payload: JsObject,
      action: String,
      providerId: UUID,
      envId: UUID): JsValue = {
    
    log.debug(s"formatImportJson(_, action = ${action}, _, _)")
    
    val providerResource = ResourceFactory.findById(ResourceIds.KubeProvider, providerId).getOrElse {
      throw new RuntimeException(s"Could not find KubeProvider '${providerId}'.")
    }
    
    val resource = (payload \ "resource").as[JsObject]
    
    import com.galacticfog.gestalt.data.TypeFactory
    
    val providerTypeName: String = {
      TypeFactory.findById(providerResource.typeId).fold {
        throw new RuntimeException(s"Provider with type ID '${providerResource.typeId}' not found. This is a bug.")
      }{ tpe => tpe.name }
    }
    
    val providerLink = Json.obj(
      "name" -> providerResource.name,          
      "id" -> providerResource.id.toString,
      "resource_type" -> providerTypeName
   )
   
    val inputProps = {
      action match {
        case "container.import" =>
          Json.obj(
            "image"          -> "n/a",
            "container_type" -> "n/a",
            "external_id"    -> "n/a",
            "provider"       -> providerLink)
        case "secret.import" => 
          Json.obj(
              "provider" -> providerId
          )
        case "volume.import" => Json.obj(
              "provider" -> providerId
          )
        case _ => throw new RuntimeException(s"Invalid action '${action}'. This is a bug")
      }
    }
    
    val importTarget = (resource \ "properties" \ "external_id").as[String]
    
    /*
     * TODO: This is used as /id of container. secret already include id in the resource payload
     * Normalize this.
     */
    val uuid = UUID.randomUUID
    
    val contextLabels = Json.obj(
      "meta/fqon"        -> (payload \ "context" \ "org" \ "fqon").as[String],
      "meta/workspace"   -> (payload \ "context" \ "workspace" \ "id").as[String],
      "meta/environment" -> envId.toString,
      "meta/provider"    -> providerId.toString
    )
    
    val fResp = (action, importTarget.split("/", 5)) match {
      case ("container.import", Array("", "namespaces", namespaceValue, "deployments", deplNameValue)) => {
        log.debug(s"Looking up Container data => namespace: $namespaceValue, deployment: $deplNameValue")
        
        val kube = initKube(providerResource, namespaceValue)
        importContainer(kube, envId.toString, namespaceValue, deplNameValue, contextLabels, (resource ++ Json.obj("id" -> uuid.toString)))
      }

      case ("secret.import", Array("", "namespaces", namespaceValue, "secrets", secretName)) => {
        log.debug(s"Looking up Secret data => namespace: $namespaceValue, secret: $secretName")
        
        val kube = initKube(providerResource, namespaceValue)
        importSecret(kube, namespaceValue, secretName, contextLabels, resource)
      }
      
      case ("volume.import", Array("", "namespaces", namespaceValue, "persistentvolumeclaims", pvcName)) => {
        log.debug(s"namespace: $namespaceValue, pvc: $pvcName")

        val kube = initKube(providerResource, namespaceValue)
        val system = ActorSystem()
        importDynamicVolumeRetry(kube, namespaceValue, pvcName, contextLabels, resource,
            attempts = VOLUMECLAIM_BINDING_ATTEMPTS,
            delay = VOLUMECLAIM_BINDING_DELAY)(system)
      }
      case _ => throw BadRequestException(s"Invalid combination of action and External ID: (`${action}`, `${importTarget}`)")
    }
    Await.result(fResp, 15.seconds)
  }  
  
  
  
  def importDynamicVolumeRetry(
      kube: RequestContext, 
      namespace: String, 
      name: String,
      labels: JsObject,
      resource: JsObject,
      attempt: Int = 0,
      attempts: Int = VOLUMECLAIM_BINDING_ATTEMPTS,      
      delay: FiniteDuration = VOLUMECLAIM_BINDING_DELAY,
      ec: ExecutionContext = scala.concurrent.ExecutionContext.global)(
          implicit as: ActorSystem): Future[JsValue] = {
  
    importDynamicVolume(kube, namespace, name, labels, resource).recoverWith {
      case a: ResourceUnavailableException => {
        if (attempt >= attempts) {
          log.error("Max retries exhausted - failing.")
          throw a
        }
        else {
          log.debug(s"PVC was unbound. Retrying in ${delay}: (${attempt+1}/${attempts})")
          after(delay, as.scheduler, ec, Future.successful(1)).flatMap { _ =>
            importDynamicVolumeRetry(kube, namespace, name, labels, resource, (1+attempt), attempts, delay)
          }
        }
      }
      case e: Throwable => {
        as.terminate()
        throw e
      }      
    }
  }
  
  def importDynamicVolume(
      kube: RequestContext, 
      namespace: String, 
      name: String,
      labels: JsObject,
      resource: JsObject) = {
    
    log.debug(s"importDynamicVolume(_, namespace=$namespace, name=$name ...)")

    val inputProps = (resource \ "properties").asOpt[JsObject].getOrElse(Json.obj())
    val metaId = ((resource\"id").asOpt[String]).fold(UUID.randomUUID)(UUID.fromString(_))
    
    for {
      pvc <- kube.getInNamespace[PersistentVolumeClaim](name, namespace)

      pvcPhase = pvc.status.flatMap(_.phase)

      _ = if (pvcPhase.isEmpty || pvcPhase.get != PersistentVolume.Phase.Bound) 
              throw new ResourceUnavailableException(s"PVC is not yet bound. Status: ${pvcPhase.getOrElse("N/A")}")
      
      _   <- assertUnmanaged(pvc, "volume")
      pvcSpec = pvc.spec.get
      pv  <- kube.getInNamespace[PersistentVolume](pvcSpec.volumeName, namespace)
      _ = println("GOT PV => " + pv)
      _ = println("PV SPEC == " + pv.spec)
      pvSpec     = pv.spec.get
      size       = pvSpec.capacity.get("storage").map(_.amount.toInt / 1024 / 1024).getOrElse(0)
      accessMode = (pvcSpec.accessModes.headOption orElse pvSpec.accessModes.headOption).map(_.toString).getOrElse("")
      configObj  = pvcSpec.storageClassName match {
        case None    => Json.obj()
        case Some(n) => Json.obj("storage_class" -> n)
      }
      importProps = Json.obj(
        "type"   -> "dynamic",
        "size"   -> size,
        "config" -> configObj,
        "access_mode" -> accessMode
      )
      _ <- setLabels(kube, pvc, "volume", metaId, labels)
    } 
    yield resource ++ Json.obj("properties" -> (inputProps ++ importProps))
  }

//      pvType = pvSpec.source match {
//        // not sure if this is the correct correspondence
//        case _: skuber.Volume.AWSElasticBlockStore => "external"
//        case _: skuber.Volume.GCEPersistentDisk => "persistent"
//        case _: skuber.Volume.Glusterfs => "external"
//        case _: skuber.Volume.HostPath => "host_path"
//        case _: skuber.Volume.ISCSI => "external"
//        case _: skuber.Volume.NFS => "external"
//        case _: skuber.Volume.RBD => "external"
//        case _: skuber.Volume.GenericVolumeSource => "external"
//      }  
  
  def importSecret(
      kube: RequestContext, 
      namespaceValue: String, 
      secretName: String,
      contextLabels: JsObject,
      resource: JsObject /*secret json*/) = {
    
    val inputProps = (resource \ "properties").asOpt[JsObject].getOrElse(Json.obj())
    val secretUuid = UUID.fromString((resource \ "id").as[String])           
    
    log.debug(s"namespace: $namespaceValue, secret: $secretName")

    for {
      secret <- kube.getInNamespace[skuber.Secret](secretName, namespaceValue)
      _      <- assertUnmanaged(secret, "secret")
      importProps = Json.obj(
        "items" -> (secret.data map { case(key, value) =>
          Json.obj("key" -> key, "value" -> new String(value, "UTF-8"))
        })
      )
      _ <- setLabels(kube, secret, "secret", secretUuid, contextLabels)
    } yield resource ++ Json.obj(
      "properties" -> (inputProps ++ importProps)
    )
  }

  
  def importContainer(
      kube: RequestContext, 
      envId: String, 
      namespaceValue: String, 
      deplNameValue: String,
      contextLabels: JsObject,
      resource: JsObject /*container json*/) = {
    
    val inputProps = (resource \ "properties").asOpt[JsObject].getOrElse(Json.obj())
    val resourceUuid = UUID.fromString((resource \ "id").as[String])    
    
    for {
      depl <- kube.getInNamespace[Deployment](deplNameValue, namespaceValue)
      _    <- assertUnmanaged(depl, "container")    // so that failed imports don't set labels
      /*
       * Here is where we limit import targets to a single Container.
       */
      containerSpec: Container <- depl.getPodSpec.map(_.containers) match {
        case Some(List(single)) => Future.successful(single)
        case None => Future.failed(new RuntimeException("Kubernetes deployment did not have a Pod spec"))
        case Some(_) => Future.failed(new RuntimeException(
            "Kubernetes container.import currently only supports Deployments with a single container spec"))
      }
      
      volumesSpec  = depl.getPodSpec.get.volumes
      portMappings = containerSpec.ports.map { kp =>
        Json.obj(
          "container_port" -> kp.containerPort,
          "service_port"   -> kp.hostPort,
          "name"           -> kp.name,
          "virtual_hosts"  -> Seq.empty[String],
          "protocol"       -> (kp.protocol match {
            case skuber.Protocol.TCP => "tcp"
            case skuber.Protocol.UDP => "udp"
          }),
          "expose_endpoint" -> false // ...for now
           // "service_port" -> (pm \ "servicePort").asOpt[Int],
           // "lb_port" -> vipHostPort.map(_._2),
           // "expose_endpoint" -> vipHostPort.isDefined,
           // "service_address" -> vipHostPort.map {
           //   case (host, port) => Json.obj(
           //     "protocol" -> (pm \ "protocol").asOpt[String],
           //     "host" -> host,
           //     "port" -> port
           //   )
           // }
        )
      }
      
      cpuLimits = for(
        r <- containerSpec.resources;
        cpu <- r.limits.get(skuber.Resource.cpu)
      ) yield cpu.amount.toDouble
      
      cpuRequests = for(
        r <- containerSpec.resources;
        cpu <- r.requests.get(skuber.Resource.cpu)
      ) yield cpu.amount.toDouble
      
      memLimits = for(
        r <- containerSpec.resources;
        memory <- r.limits.get(skuber.Resource.memory)
      ) yield (memory.amount / 1.0e6).toDouble
      
      memRequests = for(
        r <- containerSpec.resources;
        memory <- r.requests.get(skuber.Resource.memory)
      ) yield (memory.amount / 1.0e6).toDouble
      
      /*
       * Variable Secrets
       */
      secretsAsEnvVars <- Future.traverse(
          containerSpec.env collect { 
            case skuber.EnvVar(name, skuber.EnvVar.SecretKeyRef(secretKey, secretName)) => {
              (name, secretKey, secretName)
        }
      }) { case(name, secretKey, secretName) =>
        log.debug(s"Found Env Var Secret Reference: $secretName")
        log.debug(s"Attempting to lookup underlying Secret resource in kubernetes...")
        kube.getInNamespace[skuber.Secret](secretName, namespaceValue).map { secret =>
          Json.toJson(ContainerSpec.SecretEnvMount(getMetaId(secret, envId, "secret"), name, secretKey))
        }
      }
      
      /*
       * Volume Secrets
       */
      secretVolumes = (volumesSpec.collect {
        case skuber.Volume(name, skuber.Volume.Secret(secretName, _, _, _)) => (name, secretName)
      }).toMap
      secretsAsVolumes <- Future.traverse(containerSpec.volumeMounts collect { 
        case skuber.Volume.Mount(name, mountPath, readOnly, subPath, mountPropagation) if secretVolumes.contains(name) => {
          (secretVolumes(name), mountPath)
        }
      }) { case(secretName, mountPath) =>
        kube.getInNamespace[skuber.Secret](secretName, namespaceValue) map { secret =>
          Json.toJson(ContainerSpec.SecretDirMount(getMetaId(secret, envId, "secret"), mountPath))
        }
      }
      
      /*
       * VOLUMES
       */
      mounts = (containerSpec.volumeMounts.map {
        case skuber.Volume.Mount(name, mountPath, _, _, _) => (name, mountPath)
      }).toMap
      
      inlineVolumes = volumesSpec.collect {
        case skuber.Volume(name, skuber.Volume.HostPath(path, _)) => {
          Json.obj(
            "mount_path" -> mounts(name),
            "volume_resource"  -> Json.obj(
              "name"           -> s"${deplNameValue}-${name}",
              "resource_type"  -> "Gestalt::Resource::Volume",
              "resource_state" -> "Gestalt::Resource::State::Active",
              "properties" -> Json.obj(
                // "type" -> "",
                // "config" -> Json.obj(),
                // "reclamation_policy" -> "",
                "mount_path" -> path
              )
            )
          )
        }
      }
      pvcs <- Future.traverse(
        volumesSpec.collect {
          case skuber.Volume(name, skuber.Volume.PersistentVolumeClaimRef(claimName, _)) => 
            (name, claimName)
          }
        ){ 
          case(name, claimName) =>
            log.debug(s"Found PersistentVolumeClaim (PVC) Reference: $claimName")
            log.debug(s"Attempting to lookup underlying PVC resource in kubernetes...")
            kube.getInNamespace[skuber.PersistentVolumeClaim](claimName, namespaceValue).map { pvc =>
              // Json.toJson(ContainerSpec.ExistingVolumeMountSpec(mounts(claimName), getMetaId(pvc, envId)))
              // ^^ doesn't get serialized
              Json.obj(
                "mount_path" -> mounts(name),
                "volume_id"  -> getMetaId(pvc, envId, "volume")
              )
            }
        }
      
      importProps = Json.obj(
        "container_type" -> "DOCKER",
        "image" -> containerSpec.image,
        "force_pull" -> (containerSpec.imagePullPolicy == skuber.Container.PullPolicy.Always),
        "cpus"     -> (cpuLimits orElse cpuRequests).getOrElse[Double](2.0),
        "memory"   -> (memLimits orElse memRequests).getOrElse[Double](2056.0),
        "volumes"  -> (pvcs ++ inlineVolumes),
        "labels"   -> depl.metadata.labels,
        "env"      -> Json.toJson(containerSpec.env.collect({
          case skuber.EnvVar(name, skuber.EnvVar.StringValue(value)) => (name -> value)
        }).toMap),
        "num_instances" -> depl.spec.flatMap(_.replicas).getOrElse[Int](0),
        "port_mappings" -> portMappings,
        "secrets" -> (secretsAsEnvVars ++ secretsAsVolumes),
        "network" -> "default"
      ) ++ JsObject(
        Seq(
          Option(containerSpec.command).filter(_.nonEmpty).map(cmds => "cmd" -> JsString(cmds.mkString(" "))),
          Option(containerSpec.args).filter(_.nonEmpty).map(args => "args" -> Json.toJson(args))
        ).flatten
      )
      _ <- setLabels(kube, depl, "container", resourceUuid, contextLabels)
    } yield resource ++ Json.obj(
      "properties" -> (inputProps ++ importProps)
    )    
  }
  
  
  /**
   * Assert the given resource is NOT already managed by Gestalt.
   */
  protected[controllers] def assertUnmanaged(obj: skuber.ObjectResource, uuidLabel: String): Future[Unit] = {
    val uuid = obj.metadata.labels.get(s"meta/${uuidLabel}")
    val environment = obj.metadata.labels.get("meta/environment")
    if(uuid.isEmpty && environment.isEmpty) {
      Future.successful(())
    }else {
      Future.failed(new RuntimeException(s"Resource is already managed by gestalt: envId=${environment}; id=${uuid}"))
    }
  }
  
  /**
   * Set Meta management labels on a Kubernetes resource.
   */
  protected[controllers] def setLabels[T <: skuber.ObjectResource](
      kube: RequestContext, 
      obj: T, 
      uuidLabel: String,
      metaUuid: UUID,
      contextLabels: JsObject)(implicit rd: skuber.ResourceDefinition[T], fmt: Format[T]): Future[T] = {
    
    val labels = Json.obj(
      "metadata" -> Json.obj(
        "labels" -> (Json.obj(
          s"meta/${uuidLabel}" -> metaUuid.toString
        ) ++ contextLabels)
      )
    )
    kube.jsonMergePatch(obj, labels.toString): @silent
  }  
  
}

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


object Kube {
    
    val supportedTypes = Seq(
        "configmap", "container", "cronjob",
        "daemonset", "deployment",
        "job",
        "namespace",
        "persistentvolumeclaim", "persistentvolume", "pod",
        "replicaset", "replicationcontroller", "rolebinding", "role",
        "secret", "serviceaccount", "service", "statefulset",
        "clusterrole", "clusterrolebinding"/*,
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
      val ClusterRole = "clusterrole"
      val ClusterRoleBinding = "clusterrolebinding"      
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
      val ClusterRole = "clusterroles"
      val ClusterRoleBinding = "clusterrolebindings"
    }
  }