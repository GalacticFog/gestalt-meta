package controllers.util


import java.util.UUID
import java.time.ZonedDateTime
import play.api.libs.json._
import scala.util.Try
import com.galacticfog.gestalt.json.Js

case class DeploymentSource(source: String, release: String)
case class DeploymentResources(
    kube: KubeDeploymentResources,
    meta: MetaDeploymentResources
)

trait ResourceList[A]

case class KubeDeploymentResources(
    pods: Option[Seq[JsValue]] = None,
    secrets: Option[Seq[JsValue]] = None,
    services: Option[Seq[JsValue]] = None,
    configmaps: Option[Seq[JsValue]] = None,
    deployments: Option[Seq[JsValue]] = None,
    replicasets: Option[Seq[JsValue]] = None,
    statefulsets: Option[Seq[JsValue]] = None,
    persistentvolumes: Option[Seq[JsValue]] = None,
    persistentvolumeclaims: Option[Seq[JsValue]] = None,
    namespaces: Option[Seq[JsValue]] = None,
    /*containers: Option[Seq[JsValue]] = None,*/ 
    cronjobs: Option[Seq[JsValue]] = None,
    daemonsets: Option[Seq[JsValue]] = None, 
    jobs: Option[Seq[JsValue]] = None,
    replicationcontrollers: Option[Seq[JsValue]] = None, 
    rolebindings: Option[Seq[JsValue]] = None, 
    roles: Option[Seq[JsValue]] = None,
    serviceaccounts: Option[Seq[JsValue]] = None,
    clusterroles: Option[Seq[JsValue]] = None,
    clusterrolebindings: Option[Seq[JsValue]] = None,
    errors: Option[Seq[JsValue]] = None
      ) extends ResourceList[JsValue] {
  
  import KubeDeploymentResources.supported
  
  def withResource(res: JsValue): KubeDeploymentResources = {
    (res \ "kind") match {
      case JsDefined(k) => {
        val kind = k.as[String].trim.toLowerCase 
        kind match {
          case "pod" => this.copy(pods = prepend(res, pods))
          case "secret" => this.copy(secrets = prepend(res, secrets))
          case "service" => this.copy(services = prepend(res, services))
          case "configmap" => this.copy(configmaps = prepend(res, configmaps))
          case "deployment" => this.copy(deployments = prepend(res, deployments))
          case "replicaset" => this.copy(replicasets = prepend(res, replicasets))
          case "statefulset" => this.copy(statefulsets = prepend(res, statefulsets))
          case "persistentvolume" => this.copy(persistentvolumes = prepend(res, persistentvolumes))
          case "persistentvolumeclaim" => this.copy(persistentvolumeclaims = prepend(res, persistentvolumeclaims))
          case "namespace" => this.copy(namespaces = prepend(res, namespaces))
          case "cronjob" => this.copy(cronjobs = prepend(res, cronjobs))
          case "daemonset" => this.copy(daemonsets = prepend(res, daemonsets))
          case "job" => this.copy(jobs = prepend(res, jobs))
          case "replicationcontroller" => this.copy(replicationcontrollers = prepend(res, replicationcontrollers))
          case "rolebinding" => this.copy(rolebindings = prepend(res, rolebindings)) 
          case "role" => this.copy(roles = prepend(res, roles))
          case "serviceaccount" => this.copy(serviceaccounts = prepend(res, serviceaccounts))
          case _ => {
            throw new RuntimeException(s"Unknown resource 'kind'. expected one of: ${supported.keySet.mkString("[",",","]")}. found: ${kind}")
          }
        }
      }
      case _: JsUndefined => {
        throw new RuntimeException("Could not find /kind in JSON.")
      }
    }
  }
  
  /**
   * Add an item to an Option[Seq[_]]. If Seq is None, create empty Seq and add item.
   */
  private[util] def prepend[A](res: A, seq: Option[Seq[A]]): Option[Seq[A]] = {
    Some(res +: seq.getOrElse(Seq.empty[A]))
  }  
}

object KubeDeploymentResources {

  case class SupportedType(name: String, plural: String, apiVersion: String)
  object SupportedType {
    def makeSimple(name: String, apiVersion: String) =
      SupportedType(name, s"${name}s", apiVersion)
  }

  private val supportData = Seq(
    SupportedType.makeSimple("configmap", "v1"),
    SupportedType.makeSimple("namespace", "v1"),
    SupportedType.makeSimple("persistentvolume", "v1"),
    SupportedType.makeSimple("persistentvolumeclaim", "v1"),
    SupportedType.makeSimple("pod", "v1"),
    SupportedType.makeSimple("replicationcontroller", "v1"),
    SupportedType.makeSimple("secret", "v1"),
    SupportedType.makeSimple("service", "v1"),
    SupportedType.makeSimple("serviceaccount", "v1"),
    SupportedType.makeSimple("deployment", "apps/v1"),
    SupportedType.makeSimple("daemonset", "apps/v1"),
    SupportedType.makeSimple("replicaset", "apps/v1"),
    SupportedType.makeSimple("statefulset", "apps/v1beta1"),
    SupportedType.makeSimple("job", "batch/v1"),
    SupportedType.makeSimple("cronjob", "batch/v1"),
    SupportedType.makeSimple("role", "rbac.authorization.k8s.io/v1beta1"),
    SupportedType.makeSimple("rolebinding", "rbac.authorization.k8s.io/v1beta1"),
    SupportedType.makeSimple("clusterrole", "rbac.authorization.k8s.io/v1beta1"),
    SupportedType.makeSimple("clusterrolebinding", "rbac.authorization.k8s.io/v1beta1"))
    
  val supported: Map[String, SupportedType] = supportData.map(tpe => (tpe.name -> tpe)).toMap  
  
  def isSupportedResource(name: String, version: String): Either[String, Unit] = {
    val nm = name.trim.toLowerCase
    val vn = version.trim.toLowerCase    
    
    if (supported.get(nm).isEmpty) 
      Left(s"Resource kind '${nm}' is not supported.")
    else if (supported(nm).apiVersion.toLowerCase != vn) 
      Left(s"Bad apiVersion for resource kind '${nm}'. found: ${vn}, expected: ${supported(nm).apiVersion}")
    else Right(())
  }

  lazy val supportedAsJson = {
    Json.toJson(supportData.sortBy(_.name).map(st => Json.obj("kind" -> st.name, "apiVersion" -> st.apiVersion)))
  }
  
}


case class MetaDeploymentResources(
    secrets: Option[Seq[UUID]] = None,
    containers: Option[Seq[UUID]] = None,
    errors: Option[Seq[UUID]] = None
) extends ResourceList[UUID]

case class AppDeploymentData(
    provider: UUID,
    status: String,
    source: DeploymentSource,
    timestamp: ZonedDateTime,
    native_namespace: String,
    resources: DeploymentResources) {
  
  private lazy val kubeMap = {
    listToMap(Json.toJson(resources.kube).as[JsObject]).getOrElse {
      throw new RuntimeException(s"Failed converting Kube resource list to map. found: ${resources.kube}")
    }
  }
  private lazy val (failed, successful) = partitionStatus(flat(kubeMap))
  
  
  lazy val isSuccess: Boolean = failed.isEmpty
  lazy val isFailure: Boolean = !failed.isEmpty
  
  def getStatus() = if (isSuccess) Status.Success else Status.Failure
  
  import KubeDeploymentResources.supported
    
  /**
   * List resources that may be deleted from Kubernetes.
   * Ordered to avoid re-spawning resources if evaluated left-to-right.
   */
  def deletableKubeResources(): Seq[(String, String)] = {
    val ordered: Seq[JsValue] = orderForDelete(resources.kube)
    val (_, deletable) = partitionStatus(ordered)
    identityTuples(deletable)
  }
  
  /**
   * Set the overall (top-level) deployment status
   */
  def withStatus(stat: String) = this.copy(status = stat)
  
  /**
   * Add kube resource JSON to the appropriate collection
   */
  def withKubeResource(res: JsValue): AppDeploymentData = {
    val newKubeResources = 
      (res \ "kind") match {
        case JsDefined(k) => {
          val rs = resources.kube
          val kind = k.as[String].trim.toLowerCase 
          kind match {
            case "pod" => rs.copy(pods = prepend(res, rs.pods))
            case "secret" => rs.copy(secrets = prepend(res, rs.secrets))
            case "service" => rs.copy(services = prepend(res, rs.services))
            case "configmap" => rs.copy(configmaps = prepend(res, rs.configmaps))
            case "deployment" => rs.copy(deployments = prepend(res, rs.deployments))
            case "replicaset" => rs.copy(replicasets = prepend(res, rs.replicasets))
            case "statefulset" => rs.copy(statefulsets = prepend(res, rs.statefulsets))
            case "persistentvolume" => rs.copy(persistentvolumes = prepend(res, rs.persistentvolumes))
            case "persistentvolumeclaim" => rs.copy(persistentvolumeclaims = prepend(res, rs.persistentvolumeclaims))
            case "namespace" => rs.copy(namespaces = prepend(res, rs.namespaces))
            case "cronjob" => rs.copy(cronjobs = prepend(res, rs.cronjobs))
            case "daemonset" => rs.copy(daemonsets = prepend(res, rs.daemonsets))
            case "job" => rs.copy(jobs = prepend(res, rs.jobs))
            case "replicationcontroller" => rs.copy(replicationcontrollers = prepend(res, rs.replicationcontrollers))
            case "rolebinding" => rs.copy(rolebindings = prepend(res, rs.rolebindings)) 
            case "role" => rs.copy(roles = prepend(res, rs.roles))
            case "serviceaccount" => rs.copy(serviceaccounts = prepend(res, rs.serviceaccounts))    
            case "clusterrolebinding" => rs.copy(clusterrolebindings = prepend(res, rs.clusterrolebindings)) 
            case "clusterrole" => rs.copy(clusterroles = prepend(res, rs.clusterroles))            
            case _ => 
              throw new RuntimeException(s"Unknown resource 'kind'. expected one of: ${supported.keySet.mkString("[",",","]")}. found: ${kind}")
          }
        }
        case _: JsUndefined => {
          throw new RuntimeException("Could not find /kind in JSON.")
        }
      }
    this.copy(resources = resources.copy(kube = newKubeResources))
  }
  

  /**
   * Add an item to an Option[Seq[_]]. If Seq is None, create empty Seq and add item.
   */
  private[util] def prepend[A](res: A, seq: Option[Seq[A]]): Option[Seq[A]] = {
    Some(res +: seq.getOrElse(Seq.empty[A]))
  }
  
  
  private[util] def orderForDelete(kr: KubeDeploymentResources): Seq[JsValue] = {
    // Extract values for the keys we want to shuffle
    val d = kr.deployments.getOrElse(Seq.empty[JsValue])
    val s = kr.statefulsets.getOrElse(Seq.empty[JsValue])
    val r = kr.replicasets.getOrElse(Seq.empty[JsValue])
    
    val remove = Seq("deployments", "statefulsets", "replicasets")
    val objects = listToMap(Json.toJson(kr).as[JsObject]).get
    
    // Remove keys from map
    val filteredMap: Map[String,Seq[JsValue]] = remove.foldLeft(objects) { 
      (filtered, target) => filtered - target
    }
    
    // Reorder sequence
    (d ++ s ++ r ++ filteredMap.values.toList.flatten)
  }  
  
  /**
   * Parse identifying information (kind and name) from Seq of JSON values.
   */
  private[util] def identityTuples(rs: Seq[JsValue]): Seq[Tuple2[String, String]] = {
    rs.map { r =>
      val kind = (r \ "kind").asOpt[String].getOrElse {
        throw new RuntimeException(s"Could not parse '/kind' from JSON object. found: ${r}")
      }
      val name = (r \ "metadata" \ "name").asOpt[String].getOrElse {
        throw new RuntimeException(s"Could not parse '/metadata/name' from JSON object. found: ${r}")
      }
      (kind -> name)
    }
  }
  
  /**
   * Convert a JSON object to a Scala Map of JSON values
   */
  private[util] def listToMap(rs: JsObject): Try[Map[String, Seq[JsValue]]] = {
    Js.parse[Map[String, Seq[JsValue]]](rs)
  }
  
  /**
   * Partition list of JSON objects into two Seqs by status.
   * 
   * @return (Failed, Successful)
   */
  private[util] def partitionStatus(rs: Seq[JsValue]): Tuple2[Seq[JsValue], Seq[JsValue]] = {
    rs.partition { r =>
      (r \ "status").asOpt[String] match {
        case Some(Status.Failure) => true
        case _ => false
      }
    }
  }  
  
  /**
   * Flatten a Map[String, Seq[A]] to a simple List[A] of values
   */
  private[util] def flat[A](lst: Map[String, Seq[A]]): List[A] = {
    lst.values.flatten.toList
  }
  
  
  case class KubernetesView(
      namespace: String,
      resources: KubeDeploymentResources)  
  
  object Status {
    val Success = "success"
    val Failure = "failed"
  }
}
    