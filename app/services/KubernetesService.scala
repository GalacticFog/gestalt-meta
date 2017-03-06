package services

import java.util.UUID

import com.galacticfog.gestalt.events.{AmqpClient, AmqpConnection, AmqpEndpoint, PolicyEvent}

//import play.api.{Logger => log}
//
//import play.api.Logger
import org.slf4j.LoggerFactory

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
import com.galacticfog.gestalt.meta.api.errors._
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


case class ContainerSecret(name: String, secret_type: String, data: Map[String, String])
object ContainerSecret {
  
  def fromResource(secret: GestaltResourceInstance) = {
    val parsed = for {
      ps <- Try(secret.properties getOrElse unprocessable("Unspecified properties: data, secret_type"))
      sd <- Try(ps.get("data") getOrElse unprocessable("Unspecified property: 'data'"))
      jd = Try(Json.parse(sd)) getOrElse unprocessable(s"Failed parsing 'data' property. found: $sd")
      data <- Js.parse[Map[String,String]](jd)
      tpe <- Try(ps.get("secret_type") getOrElse unprocessable("Unspecified property: 'secret_type'"))
    } yield (data, tpe)
    
    parsed map { case (data, stype) => ContainerSecret(secret.name, stype, data) }
  }
  
  def unprocessable(message: String) = throw UnprocessableEntityException(message)
}

/*
 * TODO: provider constructor argument appears to be unnecessary
 */
class KubernetesService(provider: UUID) extends CaasService with JsonInput with MetaControllerUtils {
  
  private[this] val log = LoggerFactory.getLogger(this.getClass)
  private[services] val DefaultNamespace = "default"
  
  def createSecret(context: ProviderContext, secret: GestaltResourceInstance)(
      implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    
    ContainerSecret.fromResource(secret) match {
      case Failure(e) => Future.failed(e)
      case Success(sec) =>
        for {
          k8s        <- initializeKube(context.provider.id, DefaultNamespace)
          namespace  <- getNamespace(k8s, context.environment.id, create = true)
          kube       <- initializeKube(context.provider.id, namespace.name)
          output     <- createKubeSecret(kube, secret.id, sec, namespace.name) map { _ => secret }
        } yield output
    }
  }
  
  def create(context: ProviderContext, container: GestaltResourceInstance)(
      implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {

    log.debug("create(...)")
    
    ContainerSpec.fromResourceInstance(container) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) =>
        for {
          k8s        <- initializeKube(context.provider.id, DefaultNamespace)
          namespace  <- getNamespace(k8s, context.environment.id, create = true)
          kube       <- initializeKube(context.provider.id, namespace.name)
          deployment <- createKubeDeployment(kube, container.id, spec, namespace.name) map { 
                          _ => setPostLaunchStatus(container) 
          }
        } yield deployment
    }
  }

  /**
   * Get the Kubernetes Namespace for the given Environment ID. Namespaces are named after the
   * Environment UUID. If a corresponding namespace is not found, it will be created if create is 'true'
   * 
   * @param rc RequestContext for communicating with Kubernetes
   * @param environment UUID of the Meta Environment you want a Namespace for
   * @param create when set to true, a new Namespace will be created if and existing one is not found
   */
  private def getNamespace(rc: RequestContext, environment: UUID, create: Boolean = false): Future[Namespace] = {
    log.debug("getNamespace(environment = {}, create = {}", environment, create)
    rc.get[Namespace](environment.toString) recoverWith { case _ =>
      if (create) {
        log.debug("Creating new Kubernetes namespace: {}", environment)
        rc create[Namespace] Namespace(metadata = ObjectMeta(name = environment.toString))
      } else {
        log.error("No namespace found for environment '{}' - create == false")
        throw UnprocessableEntityException(s"There is no Namespace corresponding with Environment '{}', environment")
      }
    }
  }
  
  private[services] def setPostLaunchStatus(container: GestaltResourceInstance): GestaltResourceInstance = {
    upsertProperties(container, "status" -> "LAUNCHED")    
  }

  /**
   * Create a Deployment in Kubernetes
   */
  private[services] def createKubeDeployment(k8s: RequestContext, containerId: UUID, spec: ContainerSpec, namespace: String) = {
    k8s.create[Deployment](mkdeployment(containerId, spec, namespace))
  }

  private[services] def createKubeSecret(k8s: RequestContext, secretId: UUID, spec: ContainerSecret, namespace: String) = {
    k8s.create[Secret](mksecret(secretId, spec, namespace))
  }
  
  /**
   * Lookup and return the Provider configured for the given Container.
   */
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
  
  /**
   * Parse and format the provider.id property from a container.properties map.
   */
  private def providerIdProperty(ps: Map[String, String]): Option[UUID] = {
    Js.find(Json.parse(ps("provider")).as[JsObject], "/id") map { id =>
      UUID.fromString(id.as[String])
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
  
  def destroyContainer(container: GestaltResourceInstance): Future[Unit] = {
    
    val provider = containerProvider(container)
    val depname  = deploymentName(container.name)
    /*
     * TODO: Change signature of deleteContainer to take a ProviderContext - providers 
     * must never user ResourceFactory directly.
     */
    val environment = {
      ResourceFactory.findParent(ResourceIds.Environment, container.id) map { _.id } getOrElse {
        throw new RuntimeException(s"Could not find Environment for container '${container.id}' in Meta.")
      }
    }
    
    for {
      kube        <- initializeKube(provider.id, environment.toString)
      deployment  <- kube.get[Deployment](depname)
      replicasets <- kube.list[ReplicaSetList]
      pods        <- kube.list[PodList]
      
      replicaToDelete = getByName[ReplicaSet, ReplicaSetList](replicasets, depname).get
      podsToDelete = listByName[Pod, PodList](pods, depname)
      
      _  <- kube.delete[Deployment](depname)
      _  <- kube.delete[ReplicaSet](replicaToDelete.name)
      dp <- Future(podsToDelete.map(pod => kube.delete[Pod](pod.name)).headOption.get)
    } yield dp  
  }
  
  def listByName[O <: ObjectResource, L <: KList[O]](objs: L, prefix: String): List[O] = {
    objs.items filter { _.name.startsWith(prefix) }
  }
  
  def getByName[O <: ObjectResource, L <: KList[O]](objs: L, prefix: String): Option[O] = {
    val found = listByName[O, L](objs, prefix)
    if (found.size > 1) throw new IllegalArgumentException(s"Too many matches.")
    else found.headOption
  }
  
  /**
   * Convert a ContainerSpec to a GestaltResourceInstance
   */
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
  
  private[services] def mksecret(id: UUID, secret: ContainerSecret, namespace: String = DefaultNamespace) = {
    val metadata = ObjectMeta(name = secret.name, namespace = namespace)
    val bytes = secret.data map { case (k,v) => (k, v.getBytes(Ascii.DEFAULT_CHARSET)) }
    Secret(metadata = metadata, data = bytes, `type` = secret.secret_type)
  }
  
  
  /**
   * Create a Kubernetes Deployment object in memory.
   * 
   * @param id UUID for the Meta Container. This will be used as a label on all of the.
   * @param containerSpec ContainerSpec with Container data
   */
  private[services] def mkdeployment(id: UUID, containerSpec: ContainerSpec, namespace: String = DefaultNamespace) = {
    // Container resource requirements.
    val requirements = skuber.Resource.Requirements(requests = Map(
        "cpu" -> containerSpec.cpus, 
        "memory" -> containerSpec.memory))
    
    // Container
    val container = skuber.Container(
        name = containerSpec.name, 
        image = containerSpec.image,
        resources = Some(requirements),
        env = mkEnvVars(containerSpec.env))

    // Pod Spec and Template        
    val podname = "pod-" + id.toString
    val labels = containerSpec.labels ++ Map("meta/container" -> id.toString)
    val podtemplatemeta = ObjectMeta(name = podname, labels = labels)
    val podtemplate = Pod.Template.Spec(
        metadata = podtemplatemeta, 
        spec = Some(Pod.Spec(containers = List(container), dnsPolicy = skuber.DNSPolicy.ClusterFirst)))

    // Deployment Spec
    val deployspec = Deployment.Spec(
        replicas = containerSpec.num_instances, 
        template = Some(podtemplate))
        
    // Deployment metadata
    val deploymentname = "deployment-" + containerSpec.name
    val objmeta = ObjectMeta(name = deploymentname, namespace = namespace)
            
    Deployment(metadata = objmeta, spec = Some(deployspec))    
  }
  
  private[services] def mkEnvVars(env: Map[String,String]): List[EnvVar] = {
    env.map { case (k,v) => EnvVar(k, EnvVar.StringValue(v)) }.toList
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
    
    log.debug("loadProviderConfiguration({})", provider.toString)
    
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
  
  def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }  
}