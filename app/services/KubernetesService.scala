package services

import java.util.UUID

import com.galacticfog.gestalt.events.{AmqpClient, AmqpConnection, AmqpEndpoint, PolicyEvent}

import play.api.{Logger => log}

import play.api.Logger

import play.api.libs.ws.WS
import play.api.Play.current
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, ResourceIds}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.{ContainerInstance, ContainerSpec, Resource, ResourcePath}
import com.galacticfog.gestalt.events._
import com.google.inject.Inject


import skuber._
import skuber.api.client._
import skuber.json.format._
import skuber.ext._
import skuber.json.ext.format._
import org.yaml.snakeyaml._
import play.api.libs.json._
import skuber.api.client.ObjKind

import com.galacticfog.gestalt.caas.kube._

import controllers.util._
import com.galacticfog.gestalt.json.Js
import scala.concurrent.ExecutionContext

/*
 * TODO: provider constructor argument appears to be unnecessary
 */
class KubernetesService(provider: UUID) extends CaasService with JsonInput with MetaControllerUtils {
  
  //private[services] lazy val k8s = initializeKube(provider, namespace).get
  
  def setPostCreateStatus(container: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    /*
     * Set container.properties.status == RUNNING | LAUNCHED | PENDING
     */
    ???
  }
  
  def create(context: ProviderContext, container: GestaltResourceInstance)(
      implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    
    ContainerSpec.fromResourceInstance(container) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) =>
        for {
          k8s        <- initializeKube(context.provider.id, "default")
          namespace  <- getNamespace(k8s, context.environment.id, create = true)
          kube       <- initializeKube(context.provider.id, namespace.name)
          deployment <- kube.create[Deployment](
            mkDeployment(container.id, spec, namespace.name)) map { _ => container }
          //SET STATUS
        } yield container
    }
  }
  
  
//  def launchContainer(
//      fqon: String,
//      workspace: GestaltResourceInstance,
//      environment: GestaltResourceInstance,
//      user: AuthAccountWithCreds,
//      containerSpec: ContainerSpec,
//      inId : Option[UUID] = None ): Future[(GestaltResourceInstance, Seq[ContainerInstance])] = {
//    
//    val usertype = ResourceIds.User
//    val userid = user.account.id
//    val containerId = inId getOrElse UUID.randomUUID()
//    
//    // Create container in Meta
//    val metaCreate = for {
//      input    <- specToInstance(fqon, user, containerSpec, Some(containerId))
//      resource <- ResourceFactory.create(usertype, userid)(input, Some(environment.id))
//    } yield resource
//    
//    // Create container in Kubernetes
//    metaCreate match {
//      case Failure(e) => Future.failed(e)
//      case Success(resource) => {
//        for {
//          namespace  <- getNamespace(environment.id, create = true)
//          deployment <-  k8s.create[Deployment](
//              mkDeployment(containerId, containerSpec, namespace.name)) map { _ => (resource, Seq.empty) }
//        } yield (resource, Seq.empty)
//      }
//    }
//  }

  def getNamespace(rc: RequestContext, environment: UUID, create: Boolean = false): Future[Namespace] = {
    rc.get[Namespace](environment.toString) recoverWith { case _ =>
      rc create[Namespace] Namespace(metadata = ObjectMeta(name = environment.toString))
    }
  }
  
  
  private def providerIdProperty(ps: Map[String, String]): Option[UUID] = {
    Js.find(Json.parse(ps("provider")).as[JsObject], "/id") map { id =>
      UUID.fromString(id.as[String])
    }
  }
  
  private def containerProvider(container: GestaltResourceInstance): GestaltResourceInstance = {
    val providerId = providerIdProperty(container.properties.get) getOrElse {
      throw new ResourceNotFoundException(
        s"Could not parse provider ID from container '${container.id}'")
    }
    
    ResourceFactory.findById(providerId) getOrElse {
      throw new ResourceNotFoundException(
        s"Provider with ID '$providerId' not found. Container '${container.id}' is corrupt.")
    }    
  }
  
//  def deleteContainer(container: GestaltResourceInstance): Future[Unit] = {
//    
//    val provider = containerProvider(container)
//    val depname  = deploymentName(container.name)
//    
//    for {
//      deployment  <- k8s.get[Deployment](depname)
//      replicasets <- k8s.list[ReplicaSetList]
//      pods        <- k8s.list[PodList]
//      
//      replicaToDelete = getByName[ReplicaSet, ReplicaSetList](replicasets, depname).get
//      podsToDelete = listByName[Pod, PodList](pods, depname)
//      
//      _  <- k8s.delete[Deployment](depname)
//      _  <- k8s.delete[ReplicaSet](replicaToDelete.name)
//      dp <- Future(podsToDelete.map(pod => k8s.delete[Pod](pod.name)).headOption.get)
//    } yield dp
//    
//  }
  
  
  def listByName[O <: ObjectResource, L <: KList[O]](objs: L, prefix: String): List[O] = {
    objs.items filter { _.name.startsWith(prefix) }
  }
  
  def getByName[O <: ObjectResource, L <: KList[O]](objs: L, prefix: String): Option[O] = {
    val found = listByName[O, L](objs, prefix)
    if (found.size > 1) throw new IllegalArgumentException(s"Too many matches.")
    else found.headOption
  }
  
  private[services] def specToInstance(
      fqon: String, 
      user: AuthAccountWithCreds, 
      containerSpec: ContainerSpec, 
      containerId: Option[UUID]): Try[GestaltResourceInstance] = Try {
    
    val org = orgFqon(fqon)
      .map(_.id)
      .getOrElse(throw new BadRequestException("launchContainer called with invalid fqon"))
      
    val containerResourceInput: GestaltResourceInput = 
      ContainerSpec.toResourcePrototype(containerSpec).copy(id = containerId)
      
    withInputDefaults(org, containerResourceInput, user, None)
  }
  
  /**
   * Create a Kubernetes Deployment object in memory.
   * 
   * @param id UUID for the Meta Container. This will be used as a label on all of the.
   * @param containerSpec ContainerSpec with Container data
   */
  private[services] def mkDeployment(id: UUID, containerSpec: ContainerSpec, namespace: String = "default") = {
    // Container resource requirements.
    val requirements = skuber.Resource.Requirements(requests = Map(
        "cpu" -> containerSpec.cpus, 
        "memory" -> containerSpec.memory))
    
    // Container
    val container = skuber.Container(
        name = containerSpec.name, 
        image = containerSpec.image,
        resources = Some(requirements))

    // Pod Spec and Template        
    val podname = "pod-" + id.toString
    val labels = containerSpec.labels ++ Map("meta/container" -> id.toString)
    val podtemplatemeta = ObjectMeta(name = podname, labels = labels)
    val podtemplate = Pod.Template.Spec(
        metadata = podtemplatemeta, 
        spec = Some(Pod.Spec(containers = List(container))))

    // Deployment Spec
    val deployspec = Deployment.Spec(
        replicas = containerSpec.num_instances, 
        template = Some(podtemplate))
        
    // Deployment metadata
    val deploymentname = "deployment-" + containerSpec.name
    val objmeta = ObjectMeta(name = deploymentname, namespace = namespace)
            
    Deployment(metadata = objmeta, spec = Some(deployspec))    
  }
  
  private[services] def deploymentName(containerName: String) =
    "deployment-" + containerName
    
  /**
   * 
   */
  private[services] def initializeKube(provider: UUID, namespace: String)(
      implicit ec: ExecutionContext): Future[RequestContext] = for {
    config  <- loadProviderConfiguration(provider)
    context <- Future(KubeConfig.initializeString(config, namespace = Some(namespace)).get)
  } yield context
  
  
  /**
   * Get kube configuration from Provider. Performs lookup and validation of provider type.
   */
  private[services] def loadProviderConfiguration(provider: UUID)(
      implicit ec: ExecutionContext): Future[String] = Future {
    val prv = ResourceFactory.findById(provider) getOrElse { 
      throw new ResourceNotFoundException(s"Provider with ID '$provider' not found.")
    }
    
    if (prv.typeId != ResourceIds.CaasProvider)
      throw ResourceNotFoundException(s"Provider '$provider' is not a CaaS Provider")
    else extractKubeConfig(prv.properties) getOrElse {
      throw new RuntimeException(s"Provider configuration not found. This is a bug")
    }
  }
  
  /**
   * Get kube configuration from provider.properties. Decode if necessary.
   */
  private[services] def extractKubeConfig(props: Option[Map[String, String]]): Option[String] = {
    props flatMap { ps =>
      ps.get("data").map { config =>
        if (Ascii.isBase64(config)) Ascii.decode64(config) else config
      }
    }
  }  
  
}