package services

import java.util.{TimeZone, UUID}

import org.slf4j.LoggerFactory

import scala.language.implicitConversions
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.Future
import scala.concurrent.duration._
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, ResourceIds}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.ContainerSpec
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
import com.galacticfog.gestalt.marathon.ContainerStats
import com.galacticfog.gestalt.meta.api.ContainerSpec.PortMapping
import org.joda.time.{DateTime, DateTimeZone}
import skuber.Container.Port

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

trait SkuberFactory {
  def initializeKube( provider: UUID, namespace: String )
                    ( implicit ec: ExecutionContext ): Future[RequestContext]
}

class DefaultSkuberFactory extends SkuberFactory {

  /**
    *
    */
  override def initializeKube( provider: UUID, namespace: String )
                             ( implicit ec: ExecutionContext ): Future[RequestContext] = for {
    config  <- loadProviderConfiguration(provider)
    context <- Future.fromTry(KubeConfig.initializeString(config, namespace = Some(namespace)))
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

    if (prv.typeId != ResourceIds.KubeProvider)
      throw ResourceNotFoundException(s"Provider '$provider' is not a Kubernetes Provider")
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

object KubernetesService {
  val META_CONTAINER_KEY = "meta/container"
}

class KubernetesService @Inject() ( skuberFactory: SkuberFactory )
  extends CaasService with JsonInput with MetaControllerUtils {

  import KubernetesService._
  
  private[this] val log = LoggerFactory.getLogger(this.getClass)
  private[services] val DefaultNamespace = "default"

  import ContainerSecret.unprocessable

  def createSecret(context: ProviderContext, secret: GestaltResourceInstance)(
      implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    
    ContainerSecret.fromResource(secret) match {
      case Failure(e) => Future.failed(e)
      case Success(sec) =>
        for {
          k8s        <- skuberFactory.initializeKube(context.provider.id, DefaultNamespace)
          namespace  <- getNamespace(k8s, context.environment.id, create = true)
          kube       <- skuberFactory.initializeKube(context.provider.id, namespace.name)
          output     <- createKubeSecret(kube, secret.id, sec, namespace.name) map { _ => secret }
        } yield output
    }
  }

  def create(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    log.debug("create(...)")
    ContainerSpec.fromResourceInstance(container) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) => for {
        k8s        <- skuberFactory.initializeKube(context.provider.id, DefaultNamespace)
        namespace  <- getNamespace(k8s, context.environment.id, create = true)
        kube       <- skuberFactory.initializeKube(context.provider.id, namespace.name)
        updatedContainerSpec <- createKubeDeployment(kube, container.id, spec, namespace.name)
      } yield upsertProperties(
        container,
        "external_id" -> s"deployment-${container.name}",
        "status" -> "LAUNCHED",
        "port_mappings" -> Json.toJson(updatedContainerSpec.port_mappings).toString()
      )
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

  private[this] implicit def toProtocol(proto: String): skuber.Protocol.Value = proto.toUpperCase match {
    case "TCP" => skuber.Protocol.TCP
    case "UDP" => skuber.Protocol.UDP
    case _ => unprocessable("port mapping must specify \"TCP\" or \"UDP\" as protocol")
  }

  /**
   * Create a Deployment in Kubernetes
   */
  private[services] def createKubeDeployment(k8s: RequestContext, containerId: UUID, spec: ContainerSpec, namespace: String): Future[ContainerSpec] = {
    val fDep = k8s.create[Deployment](mkdeployment(containerId, spec, namespace))
    val fUpdatedPMs = createKubeService(k8s, namespace, containerId, spec)
    for {
      dep <- fDep
      updatedPMs <- fUpdatedPMs
    } yield (spec.copy(
      port_mappings = updatedPMs
    ))
  }

  private[services] def createKubeService(k8s: RequestContext, namespace: String, containerId: UUID, container: ContainerSpec): Future[Seq[PortMapping]] = {
    val exposedPorts = container.port_mappings.filter(_.expose_endpoint.contains(true))
    val srvProto = exposedPorts.foldLeft[Service](
      Service(metadata=ObjectMeta(name=container.name,namespace=namespace))
        .withType(Service.Type.NodePort)
        .withSelector(
          META_CONTAINER_KEY -> containerId.toString
        )
        .addLabel(
          META_CONTAINER_KEY -> containerId.toString
        )
    ){
      case (svc,pm) => svc.exposeOnPort(Service.Port(
        name = pm.name.getOrElse(""),
        protocol = pm.protocol,
        port = pm.service_port.orElse(pm.container_port).getOrElse(unprocessable("port mapping must contain container_port ")),
        targetPort = Some(pm.container_port.getOrElse(unprocessable("port mapping must contain container_port"))),
        nodePort = pm.host_port.getOrElse(0)
      ))
    }
    if (srvProto.spec.exists(_.ports.size > 0)) {
      val svchost = s"${container.name}.${namespace}.svc.cluster.local"
      k8s.create[Service](srvProto) map { _ =>
        container.port_mappings.map {
          case pm if pm.expose_endpoint.contains(true) => pm.copy(
            service_address = Some(ContainerSpec.ServiceAddress(
              host = svchost,
              port = pm.service_port.orElse(pm.container_port).get,
              protocol = Some(pm.protocol)
            ))
          )
          case pm => pm.copy(service_address = None)
        }
      }
    } else {
      // return original port_mappings, clear out the service_address fields
      Future.successful(container.port_mappings.map(_.copy(service_address = None)))
    }
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

    val fKube = skuberFactory.initializeKube(provider.id, environment.toString)
    
    val fDplDel = for {
      kube        <- fKube
      deployment  <- kube.get[Deployment](depname)
      replicasets <- kube.list[ReplicaSetList]
      pods        <- kube.list[PodList]
      
      replicaToDelete = getByName[ReplicaSet, ReplicaSetList](replicasets, depname).get
      podsToDelete = listByName[Pod, PodList](pods, depname)

      _  <- kube.delete[Deployment](depname)
      _  <- kube.delete[ReplicaSet](replicaToDelete.name)
      dp <- Future.traverse(podsToDelete){pod =>
        log.debug(s"deleting Kubernetes Pod ${environment}/${pod.name}")
        kube.delete[Pod](pod.name)
      }
    } yield dp.headOption.getOrElse(())

    val fSrvDel = for {
      kube <- fKube
      srvs <- kube.list[ServiceList]
      dsrv <- Future.traverse(
        srvs.items.filter(_.metadata.labels.get(META_CONTAINER_KEY).contains(container.id.toString))
      ){srv =>
        log.debug(s"deleting Kubernetes Service ${environment}/${srv.name}")
        kube.delete[Service](srv.name)
      }
    } yield dsrv.headOption.getOrElse(())

    Future.sequence(Seq(fDplDel,fSrvDel)) map {_ =>
      log.debug(s"finished deleting Deployment, ReplicaSet, Pods and Service for container ${container.id}")
      ()
    }
  }

  def listByName[O <: ObjectResource, L <: KList[O]](objs: L, prefix: String): List[O] = {
    objs.items filter { _.name.startsWith(prefix) }
  }

  def listByLabel[O <: ObjectResource, L <: KList[O]](objs: L, label: (String, String)): List[O] = {
    objs.items filter { _.metadata.labels.exists(_ == label)}
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


  private[services] def mkportmappings(pms: Seq[PortMapping]): Seq[Port] = {
    pms.map(pm => skuber.Container.Port(
      containerPort = pm.container_port.getOrElse(unprocessable("port mapping must specify container_port")),
      protocol = pm.protocol,
      name = pm.name.getOrElse(
        if (pms.size > 1) unprocessable("multiple port mappings requires port names") else ""
      ),
      hostPort = pm.host_port
    ))
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
      env = mkEnvVars(containerSpec.env),
      ports = mkportmappings(containerSpec.port_mappings).toList
    )

    // Pod Spec and Template        
    val podname = "pod-" + id.toString
    val labels = containerSpec.labels ++ Map(META_CONTAINER_KEY -> id.toString)
    val podtemplatemeta = ObjectMeta(name = podname, labels = labels)
    val podtemplate = Pod.Template.Spec(
        metadata = podtemplatemeta, 
        spec = Some(Pod.Spec(containers = List(container), dnsPolicy = skuber.DNSPolicy.ClusterFirst)))

    // Deployment Spec
    val deployspec = Deployment.Spec(
        replicas = containerSpec.num_instances, 
        template = Some(podtemplate)
    )
        
    // Deployment metadata
    val deploymentname = "deployment-" + containerSpec.name
    val objmeta = ObjectMeta(name = deploymentname, namespace = namespace)
            
    Deployment(metadata = objmeta, spec = Some(deployspec))    
  }

  private[services] def mkEnvVars(env: Map[String,String]): List[EnvVar] = {
    env.map { case (k,v) => EnvVar(k, EnvVar.StringValue(v)) }.toList
  }

  private[services] def deploymentName(containerName: String) = "deployment-" + containerName

  def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
    val depname  = deploymentName(container.name)
    for {
      kube <- skuberFactory.initializeKube(context.providerId, context.environment.id.toString)
      maybeDepl <- kube.getOption[Deployment](depname)
      pods <- maybeDepl match {
        case None    => Future.successful(PodList())
        case Some(_) => kube.list[PodList]
      }
      thesePods = listByLabel[Pod,PodList](pods, META_CONTAINER_KEY -> container.id.toString)
      update = maybeDepl map (kubeDeplAndPodsToContainerStatus(_, thesePods))
    } yield update
  }

  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    for {
      kube <- skuberFactory.initializeKube(context.providerId, context.environment.id.toString)
      depls <- kube.list[DeploymentList]()
      allPods <- kube.list[PodList]()
      _ = log.debug(s"listInEnvironment returned ${depls.size} deployments and ${allPods.size} pods")
      stats = depls.flatMap (depl =>
        depl.metadata.labels.get(META_CONTAINER_KEY) map { id =>
          val thesePods = listByLabel[Pod,PodList](allPods, META_CONTAINER_KEY -> id)
          log.debug(s"deployment for container ${id} selected ${thesePods.size} pods")
          kubeDeplAndPodsToContainerStatus(depl, thesePods)
        }
      )
    } yield stats
  }

  private[this] def kubeDeplAndPodsToContainerStatus(depl: Deployment, pods: List[Pod]): ContainerStats = {
    // meta wants: SCALING, SUSPENDED, RUNNING, HEALTHY, UNHEALTHY
    // kube provides: waiting, running, terminated
    val containerStates = pods.flatMap(_.status.toSeq).flatMap(_.containerStatuses).flatMap(_.state.toSeq).map(_.id)
    val numRunning = containerStates.count(_ == "running")
    val numTarget  = depl.spec.map(_.replicas).getOrElse(numRunning) // TODO: don't really know what to do if this doesn't exist
    val specPorts = depl.spec.toSeq.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).flatMap(_.ports).map(_.containerPort)
    val status = if (numRunning != numTarget) "SCALING"
    else if (numRunning == 0 && 0 == numTarget) "SUSPENDED"
    else if (numRunning == numTarget) "RUNNING"
    else "UNKNOWN"
    val specImage = depl.spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).map(_.image).getOrElse("")
    val age = depl.metadata.creationTimestamp.map(
      zdt => new DateTime(
        zdt.toInstant().toEpochMilli(),
        DateTimeZone.forTimeZone(TimeZone.getTimeZone(zdt.getZone())))
    )
    val resources = depl.spec.flatMap(_.template).flatMap(_.spec).flatMap(_.containers.headOption).flatMap(_.resources)
    val taskStats = pods.map {
      pod =>
        ContainerStats.TaskStat(
          id = pod.name,
          host = pod.status.flatMap(_.hostIP).getOrElse(""),
          startedAt = pod.status.flatMap(_.startTime).map(_.toString),
          ipAddresses = pod.status.flatMap(_.podIP).map(ip => Seq(ContainerStats.TaskStat.IPAddress(
            ipAddress = ip,
            protocol = "IPv4"
          ))),
          ports = specPorts
        )
    }
    ContainerStats(
      id = depl.name,
      containerType = "DOCKER",
      status = status,
      cpus = resources.flatMap(_.limits.get("cpu")).map(_.value).map(_.toDouble).getOrElse(0),
      memory = resources.flatMap(_.limits.get("memory")).map(_.value).map(_.toDouble).getOrElse(0),
      image = specImage,
      age = age.getOrElse(DateTime.now()),
      numInstances = numTarget,
      tasksStaged = 0,
      tasksRunning = numRunning,
      tasksHealthy = 0,
      tasksUnhealthy = 0,
      taskStats = Some(taskStats)
    )
  }

}