package controllers.util

import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

import com.galacticfog.gestalt.data.models._
//import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds}
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

class KubeNativeMethods @Inject()(skuberFactory: SkuberFactory) {

  private val log = Logger(this.getClass)
  
  def deleteAppDeployment(r: GestaltResourceInstance, qs: Map[String,Seq[String]]) = {
    log.info(s"Received request to delete AppDeployment ${r.id}")
    
    val ps = r.properties.getOrElse {
      throw new RuntimeException(s"Empty properties collection on resource ${r.id}")
    }
    val data = ps.get("data").getOrElse {
      throw new RuntimeException(s"Could not find /properties/data on resource ${r.id}")
    } 
    val dep = Js.parse[AppDeploymentData](Json.parse(data)) match {
      case Success(d) => d
      case Failure(e) =>
        throw new RuntimeException(s"Failed parsing resource ${r.id} to AppDeployment: ${e.getMessage}")
    }
    log.debug("Successfully parsed AppDeployment from resource.")
    
    val kube = ResourceFactory.findById(dep.provider).getOrElse {
      log.error(s"Kube provider with ID '${dep.provider}' not found.")
      throw new RuntimeException(s"Kube provider with ID '${dep.provider}' not found.")
    }
    
    val namespace = namespaceOrDefault(qs)
    /*
     * TODO: 
     * Delete everything EXCEPT Pods from kube. 
     * Delete the containers through meta, which will clean up the pods.
     *   
     */
    skuberFactory.initializeKube(kube, namespace).flatMap { context =>
      val results = dep.deletableKubeResources.map { res =>
        log.debug(s"Deleting [${res._1} ${res._2}] from kube.")
        deleteResource(res._1, res._2, context)
      }
      Future.sequence(results)
    }
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
  
  private[controllers] def deleteResource(kind: String, nm: String, context: RequestContext): Future[_] = {
    
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
      case _ => Future(new Exception(s"/$kind/$nm is not a valid URI."))
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
  
  def kubeDelete[T <: ObjectResource](context: RequestContext, name: String, grace: Int = -1)(implicit rd: skuber.ResourceDefinition[T]): Future[_] = {
    context.delete[T](name, grace).transform( 
      s => (), 
      e => {
        println("*****E : " + e)
        new RuntimeException(s"There was an error: ${e.getMessage}")
      })
  }  
  
}