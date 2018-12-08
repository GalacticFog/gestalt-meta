package controllers.util


import java.util.UUID
import java.time.ZonedDateTime
import play.api.libs.json._


case class DeploymentSource(source: String, release: String)
case class DeploymentResources(
    kube: KubeDeploymentResources,
    meta: MetaDeploymentResources
)
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
    errors: Option[Seq[JsValue]] = None
)
case class MetaDeploymentResources(
    secrets: Option[Seq[UUID]] = None,
    containers: Option[Seq[UUID]] = None,
    errors: Option[Seq[UUID]] = None
)
case class AppDeploymentData(
    provider: UUID,
    status: String,
    source: DeploymentSource,
    timestamp: ZonedDateTime,
    native_namespace: String,
    resources: DeploymentResources) {
  
  private val validKubeTypes = Seq(
    "pod",
    "secret",
    "service",
    "configmap",
    "deployment",
    "replicaset",
    "statefulset",
    "persistentvolume",
    "persistentvolumeclaim",
    "namespace")
  
  
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
            case _ => 
              throw new RuntimeException(s"Unknown resource 'kind'. expected one of: ${validKubeTypes.mkString("[",",","]")}. found: ${kind}")
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
}
    