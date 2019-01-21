package controllers.util

import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds}
import scala.util.{Success,Failure}
import javax.inject.Inject
import play.api.Logger
import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.data.ResourceFactory
import play.api.libs.json._

//import javax.inject.Singleton
import services.SkuberFactory

import scala.util.{/*Try,*/ Success, Failure}

import skuber._
import skuber.apps.v1.{ReplicaSet /*, ReplicaSetList*/}
import skuber.api.client._
import skuber.json.format._
import skuber.apps.v1beta1._

import skuber.LabelSelector
import scala.util.Try
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

object KubeNativeMethods {
  val META_DEPLOYMENT_ANNOTATION = "meta/appdeployment"  
}
class KubeNativeMethods @Inject()(skuberFactory: SkuberFactory) {

  import KubeNativeMethods._
  
  private val log = Logger(this.getClass)
  
  def deleteAppDeployment(
      auth: AuthAccountWithCreds, 
      r: GestaltResourceInstance, 
      qs: Map[String,Seq[String]]): Try[Seq[GestaltResourceInstance]] = {
    
    log.info(s"Received request to delete AppDeployment ${r.id}")
    
    val ps = r.properties.getOrElse {
      throw new RuntimeException(s"Empty properties collection on resource ${r.id}")
    }
    
    val data = ps.get("data").getOrElse {
      throw new RuntimeException(s"Could not find /properties/data on resource ${r.id}")
    }
    
    val appDeployment = Js.parse[AppDeploymentData](Json.parse(data)) match {
      case Success(d) => d
      case Failure(e) =>
        throw new RuntimeException(s"Failed parsing resource ${r.id} to AppDeployment: ${e.getMessage}")
    }
    
    log.debug("Successfully parsed AppDeployment from resource.")
    
    /*
     *  Find the Kubernetes Deployment object in the AppDeployment
     *  Currently we require the source Helm Chart to include an explicit Deployment
     *  object.
     */
    log.debug("Finding Kubernetes Deployment in AppDeployment resources.")
    
    val kubeDeployment: JsValue = (for {
      ds <- appDeployment.resources.kube.deployments
      d  <- ds.headOption
    } yield d).getOrElse {
      throw new RuntimeException("Could not find Kube Deployment for Meta AppDeployment Resource.")
    }
    
    /*
     *  Find the AppDeployment Kube Provider
     */
    log.debug("Looking up KubeProvider")
    
    val kube = ResourceFactory.findById(appDeployment.provider).getOrElse {
      log.error(s"Kube provider with ID '${appDeployment.provider}' not found.")
      throw new RuntimeException(s"Kube provider with ID '${appDeployment.provider}' not found.")
    }

    /*
     * TODO: There doesn't seem to be a need for the caller to specify a kube namespace.
     * The namespace should come from the AppDeployment - we can get rid of the 'namespace'
     * query parameter for `DELETE /appdeployments/{id}`
     * 
     * val namespace = namespaceOrDefault(qs)
     */

    val (namespace, deploymentName) = {
      ((kubeDeployment \ "metadata" \ "namespace").as[String],
       (kubeDeployment \ "metadata" \ "name").as[String])
    }
    val externalId = s"/namespaces/${namespace}/deployments/${deploymentName}"
    
    log.debug(s"[deployment-name]: ${deploymentName}, [namespace]: ${namespace}, [external-id]: ${externalId}")

    // Attempt to delete the Kube Deployment along with dependent resources.
    val maybeKubeDelete = for {
      context <- skuberFactory.initializeKube(kube, namespace)
      r1 <- deleteDeployment(context, deploymentName, DeletePropagation.Foreground)
      r2 <- ancillaryCleanup(context, appDeployment)
    } yield r2
    
    val result = maybeKubeDelete.transform(
      s => Try(findDeploymentContainer(externalId, r.id)),
      e => throw e
    )

    Await.result(result, 10.seconds)
  }

  
  def findDeletableMetaResources(deploymentId: UUID): Seq[GestaltResourceInstance] = {
    ResourceFactory.findByVariable(META_DEPLOYMENT_ANNOTATION, deploymentId.toString)
  }
  
  /**
   * Find the (single) Container that may be associated with an AppDeployment. Not finding
   * the Container is currently NOT considered an error since there may (?) be valid cases
   * where a chart deployment does not produce a meta container.
   */
  def findDeploymentContainer(externalId: String, deploymentId: UUID): Seq[GestaltResourceInstance] = { //Either[Unit, GestaltResourceInstance] = {
    ResourceFactory.findContainersByExternalId(externalId) ++ 
    ResourceFactory.findByVariable(META_DEPLOYMENT_ANNOTATION, deploymentId.toString)
  }

  /**
   * Cleanup Kubernetes resources that are listed in an AppDeployment.
   */
  def ancillaryCleanup(context: RequestContext, app: AppDeploymentData): Future[Seq[Unit]] = Future.sequence {
    val results: Seq[Future[Unit]] = app.deletableKubeResources.map { res =>
      log.debug(s"Deleting [${res._1} ${res._2}] from kube.")
      deleteResource(res._1, res._2, context)
    }
    results
  }    
    
  private[controllers] def kubeProvider(providerId: UUID) = {
    ResourceFactory.findById(providerId).getOrElse {
      log.error(s"Kube provider with ID '${providerId}' not found.")
      throw new RuntimeException(s"Kube provider with ID '${providerId}' not found.")
    }    
  }
  
  
  def listContainersInNamespace(provider: UUID, qs: Map[String, Seq[String]]) = {
    val kube = kubeProvider(provider)
    val namespace = namespaceOrDefault(qs)
    
    skuberFactory.initializeKube(kube, namespace).flatMap { context =>
      QueryString.single(qs, "label") match {
        case None => context.list[PodList]()
        case Some(lbl) => {
          val selector = new LabelSelector(LabelSelector.IsEqualRequirement("release", lbl))
          val fContainers = context.listSelected[PodList](selector)
        }
      }
      ???
    }
  }
  /*
   * DeletePropagation.Foreground
   * DeletePropagation.Background
   * DeletePropagation.Orphan
   */
  private[controllers] def deleteDeployment(context: RequestContext, deploymentName: String, policy: DeletePropagation.Value): Future[Unit] = {
    val options = DeleteOptions(propagationPolicy = Some(policy))
    context.deleteWithOptions[Deployment](deploymentName, options)    
  }
  
  private[controllers] def deleteResource(kind: String, nm: String, context: RequestContext): Future[Unit] = {
    
    val gracePeriod = -1
    
    kind.trim.toLowerCase match {
      case "pod"         => kubeDelete[Pod](context, nm, gracePeriod)
      case "secret"      => kubeDelete[Secret](context, nm, gracePeriod)
      case "service"     => kubeDelete[Service](context, nm, gracePeriod)
      case "configmap"   => kubeDelete[ConfigMap](context, nm, gracePeriod)
      case "namespace"   => kubeDelete[Namespace](context, nm, gracePeriod)
      case "deployment"  => kubeDelete[Deployment](context, nm, gracePeriod)
      case "replicaset"  => kubeDelete[ReplicaSet](context, nm, gracePeriod)
      case "statefulset" => kubeDelete[StatefulSet](context, nm, gracePeriod)
      case "persistentvolume"       => kubeDelete[PersistentVolume](context, nm, gracePeriod)
      case "persistentvolumeclaim"  => kubeDelete[PersistentVolumeClaim](context, nm, gracePeriod)
      case _ => Future(throw new Exception(s"/$kind/$nm is not a valid URI."))
    }
  }
  
  /**
   * Parse given namespace from querystring - use literal 'default' if none given.
   */
  private[controllers] def namespaceOrDefault(qs: Map[String, Seq[String]]): String = {
    QueryString.single[String](qs, "namespace").getOrElse {
      log.info("No namespace given in query - using 'default'")
      "default"
    }
  }
  
  def kubeDelete[T <: ObjectResource](context: RequestContext, name: String, grace: Int = -1)(implicit rd: skuber.ResourceDefinition[T]): Future[Unit] = {
    context.delete[T](name, grace).transform( 
      s => (), 
      e => {
        println("*****E : " + e)
        new RuntimeException(s"There was an error: ${e.getMessage}")
      })
  }  
  
}