package services

import java.util.{Base64, TimeZone, UUID}

import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.meta.api.ContainerSpec.{ExistingVolumeMountSpec, PortMapping, SecretDirMount, SecretEnvMount, SecretFileMount}
import com.galacticfog.gestalt.meta.api.ContainerStats.{ContainerStateStat, EventStat}
import com.galacticfog.gestalt.meta.api.VolumeSpec.ReadOnlyMany
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.{ContainerSpec, ContainerStats, SecretSpec, VolumeSpec}
import com.galacticfog.gestalt.util.Helpers.OptionWithDefaults
import com.galacticfog.gestalt.util.Helpers.StringCompare
import com.galacticfog.gestalt.util.Helpers.OptionDefaults._
import com.google.inject.Inject
import controllers.util._
import org.joda.time.{DateTime, DateTimeZone}
import play.api.Logger
import play.api.libs.json._
import services.util.CommandParser
import skuber.Container.Port
import skuber.LabelSelector.dsl._
import skuber.Volume.{GenericVolumeSource, HostPath}
import skuber._
import skuber.api.client._
import skuber.ext._
import skuber.json.ext.format._
import skuber.json.format._

import scala.util.Try
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{implicitConversions, postfixOps, reflectiveCalls}
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success}

object KubernetesService {
  val META_CONTAINER_KEY = "meta/container"
  val META_SECRET_KEY = "meta/secret"
  val META_VOLUME_KEY = "meta/volume"
  val META_ENVIRONMENT_KEY = "meta/environment"
  val META_WORKSPACE_KEY = "meta/workspace"
  val META_FQON_KEY = "meta/fqon"
  val META_PROVIDER_KEY = "meta/provider"

  val CPU_REQ_TYPE = "cpu-requirement-type"
  val MEM_REQ_TYPE = "memory-requirement-type"

  val REQ_TYPE_LIMIT = "limit"
  val REQ_TYPE_REQUEST = "request"

  val POD = "Pod"
  val DEPLOYMENT = "Deployment"
  val REPLICA_SET = "ReplicaSet"

  val DEFAULT_CPU_REQ = REQ_TYPE_REQUEST
  val DEFAULT_MEM_REQ = Seq(REQ_TYPE_LIMIT,REQ_TYPE_REQUEST).mkString(",")

  val HOST_VOLUME_WHITELIST = "host_volume_whitelist"
  val STORAGE_CLASSES = "storage_classes"

  def unprocessable(message: String) = throw UnprocessableEntityException(message)

  def isAllowedHostPath(provider: ResourceLike, hostPath: String): Boolean = {
    val allowedHostPaths = ContainerService.getProviderProperty[Seq[String]](provider, HOST_VOLUME_WHITELIST).getOrElse(Seq.empty)
    val hpParts = hostPath.stripSuffix("/").stripPrefix("/").split("/").scan("")(_ + "/" + _)
    hpParts.exists(part => allowedHostPaths.map(_.stripSuffix("/")).contains(part))
  }

  def isConfiguredStorageClass(provider: Instance, storageClass: String): Boolean = {
    val configuredStorageClasses = ContainerService.getProviderProperty[Seq[String]](provider, STORAGE_CLASSES).getOrElse(Seq.empty)
    configuredStorageClasses.contains(storageClass)
  }

  def mkPV( namespace: Namespace,
            metaResource: ResourceLike,
            spec: VolumeSpec,
            context: ProviderContext,
            source: Volume.PersistentSource ): PersistentVolume = {
    PersistentVolume(
      metadata = ObjectMeta(
        name = metaResource.name.substring(0, math.min(8,metaResource.name.length)) + "-" + metaResource.id.toString,
        namespace = "",
        labels = Map(
          META_VOLUME_KEY      -> metaResource.id.toString,
          META_ENVIRONMENT_KEY -> context.environmentId.toString,
          META_WORKSPACE_KEY   -> context.workspace.id.toString,
          META_FQON_KEY        -> context.fqon,
          META_PROVIDER_KEY    -> context.providerId.toString
        )
      ),
      spec = Some(PersistentVolume.Spec(
        capacity = Map(Resource.storage -> Resource.Quantity(s"${spec.size}Mi")),
        source = source,
        accessModes = List(spec.access_mode),
        claimRef = Some(skuber.ObjectReference(
          namespace = namespace.name,
          name = metaResource.name.toString
        ))
      ))
    )
  }

  def mkPVC( namespace: Namespace,
             metaResource: ResourceLike,
             spec: VolumeSpec,
             context: ProviderContext,
             maybeStorageClass: Option[String] = None): PersistentVolumeClaim = {
    PersistentVolumeClaim(
      metadata = ObjectMeta(
        name = metaResource.name,
        namespace = namespace.name,
        labels = Map(
          META_VOLUME_KEY      -> metaResource.id.toString,
          META_ENVIRONMENT_KEY -> context.environmentId.toString,
          META_WORKSPACE_KEY   -> context.workspace.id.toString,
          META_FQON_KEY        -> context.fqon,
          META_PROVIDER_KEY    -> context.providerId.toString
        )
      ),
      spec = Some(PersistentVolumeClaim.Spec(
        accessModes = List(spec.access_mode),
        resources = Some(Resource.Requirements(
          requests = Map(
            Resource.storage -> s"${spec.size}Mi"
          )
        )),
        storageClassName = maybeStorageClass
      ))
    )
  }

}

class KubernetesService @Inject() ( skuberFactory: SkuberFactory )
  extends CaasService with JsonInput with MetaControllerUtils {
  
  import KubernetesService._

  override private[this] val log = Logger(this.getClass)

  private[services] val DefaultNamespace = "default"

  def cleanly[T](provider: GestaltResourceInstance, namespace: String)(f: RequestContext => Future[T]): Future[T] = {
    skuberFactory.initializeKube(provider, namespace) flatMap { kube =>
      val fT = f(kube)
      fT.onComplete(_ => kube.close)
      fT
    }
  }

  def createSecret(context: ProviderContext, secret: GestaltResourceInstance, items: Seq[SecretSpec.Item])
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    SecretSpec.fromResourceInstance(secret) match {
      case _ if items.exists(_.value.isEmpty) => Future.failed(new BadRequestException("secret item was missing value"))
      case Failure(e) => Future.failed(e)
      case Success(spec) =>
        val specWithItems = spec.copy(
          items = items
        )
        for {
          namespace <- cleanly(context.provider, DefaultNamespace)( getNamespace(_, context, create = true) )
          output    <- cleanly(context.provider, namespace.name  )( createKubeSecret(_, secret.id, specWithItems, namespace.name, context) )
        } yield upsertProperties(
          secret,
          "items" -> Json.toJson(items.map(_.copy(value = None))).toString,
          "external_id" -> s"/namespaces/${namespace.name}/secrets/${secret.name}"
        )
    }
  }

  override def destroySecret(secret: ResourceLike): Future[Unit] = {
    val provider = ContainerService.containerProvider(secret)
    /*
     * TODO: Change signature of deleteSecret to take a ProviderContext - providers
     * must never user ResourceFactory directly.
     */
    val environment: String = {
      ResourceFactory.findParent(ResourceIds.Environment, secret.id).map(_.id.toString) orElse {
        val namespaceGetter = "/namespaces/([^/]+)/secrets/.*".r
        for {
          eid <- ContainerService.resourceExternalId(secret)
          ns <- eid match {
            case namespaceGetter(namespace) => Some(namespace)
            case _ => None
          }
        } yield ns
      } getOrElse {
        throw new RuntimeException(s"Could not find Environment for secret '${secret.id}' in Meta.")
      }
    }

    val targetLabel = META_SECRET_KEY -> secret.id.toString
    cleanly(provider, environment) { kube =>
      val fSecretDel = for {
        deps <- deleteAllWithLabel[Secret](kube, targetLabel)
      } yield deps.headOption.getOrElse({
        log.debug("deleted no Secrets")
        ()
      })

      fSecretDel map {_ =>
        log.debug(s"finished deleting Secrets for secret ${secret.id}")
        ()
      }
    }
  }

  def create(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    log.debug("create(...)")
    for {
      spec <- Future.fromTry(ContainerSpec.fromResourceInstance(container))
      namespace  <- cleanly(context.provider, DefaultNamespace)( getNamespace(_, context, create = true) )
      updatedContainerSpec <- cleanly(context.provider, namespace.name)( createDeploymentEtAl(_, container.id, spec, namespace.name, context) )
    } yield upsertProperties(
      container,
      "external_id" -> s"/namespaces/${namespace.name}/deployments/${container.name}",
      "status" -> "LAUNCHED",
      "port_mappings" -> Json.toJson(updatedContainerSpec.port_mappings).toString()
    )
  }

  def update(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    log.debug("update(...)")
    val nameGetter = "/namespaces/[^/]+/deployments/(.*)".r
    val previousName = for {
      eid <- ContainerService.resourceExternalId(container)
      prev <- eid match {
        case nameGetter(name) => Some(name)
        case _ => None
      }
    } yield prev
    if (previousName.exists(_ != container.name)) return Future.failed(new BadRequestException("renaming containers is not supported"))
    ContainerSpec.fromResourceInstance(container).map(
      c => c.copy(name = previousName getOrElse c.name)
    ) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) => for {
        namespace  <- cleanly(context.provider, DefaultNamespace)( getNamespace(_, context, create = true) )
        updatedContainerSpec <- cleanly(context.provider, namespace.name)( kube =>
            updateDeploymentEtAl(kube, container.id, spec, namespace.name, context)
        )
      } yield upsertProperties(
        container,
        "port_mappings" -> Json.toJson(updatedContainerSpec.port_mappings).toString()
      )
    }
  }

  /**
    * Get the Kubernetes Namespace for the given Environment ID.
    * By default, namespaces are named after the Environment UUID.
    * However, if existing resources in the Environment have `external_id` corresponding to a different namespace,
    * that takes precedence.
    *
    * If a corresponding namespace is not found, it will be created if create is 'true'
    *
    * @param rc RequestContext for communicating with Kubernetes
    * @param pc ProviderContext of the Meta Environment you want a Namespace for
    * @param create when set to true, a new Namespace will be created if and existing one is not found
    */
  private[services] def getNamespace(rc: RequestContext, pc: ProviderContext, create: Boolean = false): Future[Namespace] = {
    log.debug(s"getNamespace(environment = ${pc.environmentId}, create = ${create}")
    val namespaceGetter = "/namespaces/([^/]+)/.*/.*".r

    val namespaces = ResourceFactory.findChildren(pc.environmentId).filter(
      r => Set(ResourceIds.Container, ResourceIds.Secret, migrations.V13.VOLUME_TYPE_ID).contains(r.typeId) &&
        Try{ContainerService.containerProviderId(r)}.toOption.contains(pc.providerId)
    ).flatMap(ContainerService.resourceExternalId).collect {
      case namespaceGetter(namespace) => namespace
    } distinct

    val targetNamespace = namespaces match {
      case Seq() => pc.environmentId.toString
      case Seq(single) => single
      case _ => throw new BadRequestException(s"Environment '${pc.environmentId}' contains resources from multiple Kubernetes namespaces; new resources cannot be created until this is resolved.")
    }

    rc.getOption[Namespace](targetNamespace) flatMap {
      case Some(s) =>
        log.debug(s"Found Kubernetes namespace: ${s.name}")
        Future.successful(s)
      case None if create =>
        log.debug(s"Creating new Kubernetes namespace: ${targetNamespace}")
        rc.create(Namespace(metadata = ObjectMeta(
          name = targetNamespace,
          labels = Map(
            META_ENVIRONMENT_KEY -> pc.environmentId.toString,
            META_WORKSPACE_KEY -> pc.workspace.id.toString,
            META_FQON_KEY -> pc.fqon,
            META_PROVIDER_KEY -> pc.providerId.toString
          )
        )))
      case None if !create =>
        log.error(s"No namespace found for environment '${pc.environmentId}' - create == false")
        Future.failed(UnprocessableEntityException(s"There is no Namespace corresponding with Environment '${pc.environmentId}', environment"))
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

  def updateIngress(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec, context: ProviderContext): Future[Option[Ingress]] = {
    // start this async call early, we'll need it later
    val fExistingIngress = kube.getOption[Ingress](spec.name)
    /*
     * create new ingress spec
     * if empty, delete any existing ingress
     * if not empty, update or create
     */
    val maybeNewIng = mkIngressSpec(containerId, spec, namespace, context)
    fExistingIngress flatMap { maybeExistingIng =>
      (maybeExistingIng,maybeNewIng) match {
        case (None, None)            => Future.successful(None)
        case (Some(ing), None)         =>
          log.debug(s"updateIngress: deleting ${ing.name}")
          kube.delete[Ingress](ing.name) map (_ => None)
        case (None, Some(create))    =>
          log.debug(s"updateIngress: creating ${create.name}")
          kube.create[Ingress](create) map (Some(_))
        case (Some(_), Some(upd8)) =>
          log.debug(s"updateIngress: updating ${upd8.name}")
          kube.update[Ingress](upd8) map (Some(_))
      }
    }
  }

  case class ContainerServices( intSvc: Option[Service], extSvc: Option[Service], lbSvc: Option[Service] )

  def createIngress(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec, context: ProviderContext): Future[Option[Ingress]] = {
    mkIngressSpec(containerId, spec, namespace, context).fold { Future.successful(Option.empty[Ingress]) }
                                                 { kube.create[Ingress](_) map(Some(_)) }
  }

  private def getContainerServices(kube: RequestContext, containerId: UUID): Future[ContainerServices] = {
    kube.listSelected[ServiceList](LabelSelector(LabelSelector.IsEqualRequirement(
      META_CONTAINER_KEY, containerId.toString
    ))) map { svcList =>
      ContainerServices(
        svcList.items.filter(_.spec.exists(_._type == Service.Type.ClusterIP)).headOption,
        svcList.items.filter(_.spec.exists(_._type == Service.Type.NodePort)).headOption,
        svcList.items.filter(_.spec.exists(_._type == Service.Type.LoadBalancer)).headOption
      )
    }
  }

  def reconcileSvc(kube: RequestContext, plan: (Option[Service], Option[Service])): Future[Option[Service]] = {
    plan match {
      case (None, None)            => Future.successful(None)
      case (Some(cur), None)       =>
        log.debug(s"reconcileSvc: deleting ${cur.name}")
        kube.delete[Service](cur.name).map(_ => None)
      case (None, Some(create))    =>
        log.debug(s"reconcileSvc: creating ${create.name}")
        kube.create[Service](create) map Some.apply
      case (Some(cur), Some(upd8)) =>
        log.debug(s"reconcileSvc: updating ${upd8.name}")
        kube.update[Service](
        cur.spec.map(_.clusterIP).foldLeft[skuber.Service] (
          upd8.withResourceVersion(cur.resourceVersion)
        ) { (svc,clusterIP) => svc.withClusterIP(clusterIP) }
      ) map Some.apply
    }
  }

  def updateServices(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec, context: ProviderContext): Future[Seq[PortMapping]] = {

    // start this async call early, we'll need it later
    val fExistingSvcs = getContainerServices(kube, containerId)
    /*
     * create new service spec(s)
     * if empty, delete any service(s)
     * if not empty, update or create
     */
    val newServices = mkServiceSpecs(containerId, spec, namespace, context)
    for {
      existingSvcs <- fExistingSvcs
      upInt <- reconcileSvc(kube, existingSvcs.intSvc -> newServices.intSvc)
      upExt <- reconcileSvc(kube, existingSvcs.extSvc -> newServices.extSvc)
      upLb  <- reconcileSvc(kube, existingSvcs.lbSvc  -> newServices.lbSvc)
      updatedPMs = updateContainerSpecPortMappings(spec.port_mappings, namespace, ContainerServices(
        upInt,
        upExt,
        upLb
      ))
    } yield updatedPMs
  }

  def createServices(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec, context: ProviderContext): Future[Seq[PortMapping]] = {
    val svcs = mkServiceSpecs(containerId, spec, namespace, context)
    for {
      svcs <- Future.traverse(Seq(svcs.intSvc, svcs.extSvc, svcs.lbSvc)) {
        case Some(svc) => kube.create[Service](svc) map Some.apply
        case      None => Future.successful(None)
      }
      updatedPMs = updateContainerSpecPortMappings(spec.port_mappings, namespace, ContainerServices(
        svcs(0), svcs(1), svcs(2)
      ))
    } yield updatedPMs
  }

  /**
    * Create a Deployment with services in Kubernetes
    */
  private[services] def createDeploymentEtAl(kube: RequestContext, containerId: UUID, spec: ContainerSpec, namespace: String, context: ProviderContext): Future[ContainerSpec] = {
    log.debug("createDeploymentEtAl")
    val fDeployment = kube.create[Deployment](mkDeploymentSpec(kube, containerId, spec, context, namespace))
    fDeployment.onFailure{
      case e: Throwable => log.error(s"error creating Kubernetes Deployment for container ${containerId}; assuming that it was not created",e)
    }
    val fUpdatedPMsFromService = createServices(kube, namespace, containerId, spec, context) recover {
      case e: Throwable =>
        log.error(s"error creating Kubernetes Service for container ${containerId}; assuming that it was not created",e)
        spec.port_mappings
    }
    val fIngress = createIngress(kube, namespace, containerId, spec, context) recover {
      case e: Throwable =>
        log.error(s"error creating Kubernetes Ingress for container ${containerId}; assuming that it was not created",e)
        ()
    }
    for {
      dep <- fDeployment
      _   <- fIngress
      updatedPMs <- fUpdatedPMsFromService
    } yield spec.copy(
      port_mappings = updatedPMs
    )
  }

  /**
   * Update a Deployment in Kubernetes, updating/creating/deleting Services/Ingresses as needed
   */
  private[services] def updateDeploymentEtAl(kube: RequestContext, containerId: UUID, spec: ContainerSpec, namespace: String, context: ProviderContext): Future[ContainerSpec] = {
    val fDepl = kube.update[Deployment](mkDeploymentSpec(kube, containerId, spec, context, namespace))
    val fUpdatedPMsFromService = updateServices(kube, namespace, containerId, spec, context) recover {
      case e: Throwable =>
        log.error(s"error creating Kubernetes Service for container ${containerId}; assuming that it was not created",e)
        spec.port_mappings
    }
    val fIngress = updateIngress(kube, namespace, containerId, spec, context) recover {
      case e: Throwable =>
        log.error(s"error updating Kubernetes Ingress for container ${containerId}; assuming that is in an invalid state",e)
        ()
    }
    for {
      _ <- fDepl
      _ <- fIngress
      updatedPMs <- fUpdatedPMsFromService
    } yield spec.copy(
      port_mappings = updatedPMs
    )
  }

  private[services] def createKubeSecret(kube: RequestContext, secretId: UUID, spec: SecretSpec, namespace: String, context: ProviderContext): Future[Secret] = {
    kube.create[Secret](mkSecret(secretId, spec, namespace, context)) recoverWith { case e: K8SException =>
      Future.failed(new RuntimeException(s"Failed creating Secret '${spec.name}': " + e.status.message))
    }
  }

  def destroy(container: ResourceLike): Future[Unit] = {
    log.debug("destroy(_)")

    val provider = ContainerService.containerProvider(container)
    /*
     * TODO: Change signature of deleteContainer to take a ProviderContext - providers
     * must never use ResourceFactory directly.
     */
    val environment: String = {
      ResourceFactory.findParent(ResourceIds.Environment, container.id).map(_.id.toString) orElse {
        val namespaceGetter = "/namespaces/([^/]+)/deployments/.*".r
        for {
          eid <- ContainerService.resourceExternalId(container)
          ns <- eid match {
            case namespaceGetter(namespace) => Some(namespace)
            case _ => None
          }
        } yield ns
      } getOrElse {
        throw new RuntimeException(s"Could not find Environment for container '${container.id}' in Meta.")
      }
    }

    val targetLabel = META_CONTAINER_KEY -> container.id.toString
    cleanly(provider, environment) { kube =>
      val fDplDel = for {
        deps <- deleteAllWithLabel[Deployment](kube, targetLabel)
        rses <- deleteAllWithLabel[ReplicaSet](kube, targetLabel)
        pods <- deleteAllWithLabel[Pod](kube, targetLabel) recover {
          case e: K8SException =>
            log.warn(s"K8S error listing/deleting Pods associated with container ${container.id}")
            List(())
        }
      } yield pods.headOption.getOrElse({
        log.debug("deleted no Pods")
        ()
      })

      val fSrvDel = (for {
        svcs <- deleteAllWithLabel[Service](kube, targetLabel)
      } yield svcs.headOption.getOrElse({
        log.debug("deleted no Services")
        ()
      })) recover {
        case e: K8SException =>
          log.warn(s"K8S error listing/deleting Services associated with container ${container.id}")
          ()
      }

      val fIngDel = (for {
        ings <- deleteAllWithLabel[Ingress](kube, targetLabel)
      } yield ings.headOption.getOrElse({
        log.debug("deleted no Ingresses")
        ()
      })) recover {
        case e: K8SException =>
          log.warn(s"K8S error listing/deleting Ingresses associated with container ${container.id}")
          ()
      }

      Future.sequence(Seq(fDplDel,fSrvDel,fIngDel)) map {_ =>
        log.debug(s"finished deleting Deployments, ReplicaSets, Pods, Services and Ingresses for container ${container.id}")
        ()
      }
    }
  }

  def listByLabel[O <: ObjectResource, L <: ListResource[O]](objs: L, label: (String, String)): List[O] =
    objs filter { _.metadata.labels.get(label._1).contains(label._2) }

  def deleteAllWithLabel[O <: ObjectResource : TypeTag]( kube: RequestContext, label: (String, String) )
                                                       ( implicit fmtL: Format[ListResource[O]], fmtO: Format[O],
                                                                  rdo: skuber.ResourceDefinition[O],
                                                                  rd: skuber.ResourceDefinition[ListResource[O]] ) : Future[List[Unit]] = {
    val otype = typeOf[O]
    for {
      foundResources <- kube.listSelected[ListResource[O]](label._1 is label._2) map (_.items)
      _ = log.debug(s"found ${foundResources.size} ${otype} resources")
      deletes <- Future.traverse(foundResources){
        d =>
          log.info(s"deleting ${typeOf[O]} ${d.name} labeled with ${label}")
          kube.delete[O](d.name)
      }
    } yield deletes
  }

  private[services] def mkSecret(id: UUID, secret: SecretSpec, namespace: String, context: ProviderContext): Secret = {
    val metadata = ObjectMeta(
      name = secret.name,
      namespace = namespace,
      labels = Map(
        META_SECRET_KEY      -> id.toString,
        META_ENVIRONMENT_KEY -> context.environmentId.toString,
        META_WORKSPACE_KEY   -> context.workspace.id.toString,
        META_FQON_KEY        -> context.fqon,
        META_PROVIDER_KEY    -> context.providerId.toString
      )
    )
    val data = secret.items.map {
      case SecretSpec.Item(key, Some(value)) => key -> Base64.getDecoder.decode(value)
    }.toMap
    Secret(
      metadata = metadata,
      `type` = "Opaque",
      data = data
    )
  }

  private[services] def mkPortMappingsSpec(pms: Seq[PortMapping]): Seq[Port] = {
    pms.map(pm => skuber.Container.Port(
      containerPort = pm.container_port.getOrElse(unprocessable("port mapping must specify container_port")),
      protocol = pm.protocol,
      name = pm.name.getOrElse(
        if (pms.size > 1) unprocessable("multiple port mappings requires port names") else ""
      ),
      hostPort = pm.host_port
    ))
  }

  private[services] def mkIngressSpec(containerId: UUID, containerSpec: ContainerSpec, namespace: String, context: ProviderContext): Option[Ingress] = {
    val svcName = containerSpec.name

    val ingressPMs = for {
      pm <- containerSpec.port_mappings
      vhost <- pm.virtual_hosts.getOrElse(Seq.empty)
      cp <- pm.lb_port.orElse(pm.container_port)
      if pm.expose_endpoint.contains(true)
    } yield (vhost,cp)

    if (ingressPMs.isEmpty) None
    else Some(ingressPMs.foldLeft[Ingress](
      Ingress(metadata = ObjectMeta(
        name = svcName,
        namespace = namespace,
        labels = Map(
          META_CONTAINER_KEY -> containerId.toString,
          META_ENVIRONMENT_KEY -> context.environmentId.toString,
          META_WORKSPACE_KEY -> context.workspace.id.toString,
          META_FQON_KEY -> context.fqon,
          META_PROVIDER_KEY -> context.providerId.toString
        )
      ))
    ){
      case (acc, (vhost,port)) => acc.addHttpRule(
        vhost, Map("" -> s"$svcName:$port")
      )
    })
  }

  private[services] def updateContainerSpecPortMappings(pms: Seq[ContainerSpec.PortMapping], namespace: String, services: ContainerServices): Seq[PortMapping] = {
    def svcMatch(pm: ContainerSpec.PortMapping, sp: Service.Port)  = {
      pm.name.contains(sp.name) || (pm.lb_port.filter(_ != 0) orElse pm.container_port).contains(sp.port)
    }

    services.intSvc match {
      case None =>
        log.debug(s"updateContainerSpecPortMappings: No Service(ClusterIP)")
        pms.map(_.copy(
          service_address = None,
          lb_address = None
        ))
      case Some(cipSvc) =>
        val svcHost = s"${cipSvc.name}.${namespace}.svc.cluster.local"
        val cipPorts = cipSvc.spec.map(_.ports) getOrElse List.empty
        pms.map {
          pm => cipPorts.find( svcMatch(pm, _) ) match {
            case Some(sp) if pm.expose_endpoint.contains(true) =>
              log.debug(s"updateContainerSpecPortMappings: PortMapping ${pm} matched to Service.Port ${sp}")
              val nodePort = services.extSvc.flatMap(
                _.spec.flatMap(
                  _.ports.find( svcMatch(pm, _) )
                )
              ) map (_.nodePort)
              val lbAddress = for {
                lbSvc <- services.lbSvc
                spec <- lbSvc.spec
                pm <- spec.ports.find( svcMatch(pm, _) )
                lbStatus <- lbSvc.status.flatMap(_.loadBalancer).flatMap(_.ingress.headOption)
                addr <- lbStatus.hostName orElse lbStatus.ip
              } yield (addr, pm.port, "http")
              pm.copy(
                service_port = nodePort,
                service_address = Some(ContainerSpec.ServiceAddress(
                  host = svcHost,
                  port = sp.port,
                  protocol = Some(pm.protocol)
                )),
                lb_address = lbAddress.map(
                  a => ContainerSpec.ServiceAddress(
                    host = a._1,
                    port = a._2,
                    protocol = Some(a._3)
                  )
                ),
                lb_port = Some(sp.port),
                `type` = pm.`type` orElse(Some("internal"))
              )
            case _ =>
              log.debug(s"updateContainerSpecPortMappings: PortMapping ${pm} not matched")
              pm.copy(
                service_address = None,
                lb_address = None,
                lb_port = None,
                expose_endpoint = Some(false),
                `type` = None
              )
          }
        }
    }
  }

  private[services] def mkServiceSpecs(containerId: UUID, containerSpec: ContainerSpec, namespace: String, context: ProviderContext): ContainerServices = {

    // external and loadBalancer are always counted as a internal
    // likewise, loadBalancer is always counted as a external
    // therefore, we have internal superset external superset loadBalancer

    def mkSvc(svcName: String, serviceType: Service.Type.ServiceType, pms: Seq[ContainerSpec.PortMapping]): Option[Service] = {
      if (pms.isEmpty) {
        None
      } else {
        Some(pms.foldLeft[Service](
          Service(metadata = ObjectMeta(name = svcName, namespace = namespace))
            .withType(serviceType)
            .withSelector(
              META_CONTAINER_KEY -> containerId.toString
            )
            .addLabels(Map(
              META_CONTAINER_KEY -> containerId.toString,
              META_ENVIRONMENT_KEY -> context.environmentId.toString,
              META_WORKSPACE_KEY -> context.workspace.id.toString,
              META_FQON_KEY -> context.fqon,
              META_PROVIDER_KEY -> context.providerId.toString
            ))
        ) {
          case (svc, pm) =>
            val cp = pm.container_port.getOrElse(unprocessable("port mapping must contain container_port"))
            svc.exposeOnPort(Service.Port(
              name = pm.name.getOrElse(""),
              protocol = pm.protocol,
              port = pm.lb_port.filter(_ != 0).getOrElse(cp),
              targetPort = Some(Left(cp)),
              nodePort = pm.service_port.filter(_ => serviceType == Service.Type.NodePort).getOrElse(0)
            ))
        })
      }
    }

    val cipPMs = containerSpec.port_mappings.filter {
      pm => pm.expose_endpoint.contains(true)
    }
    val npPMs = containerSpec.port_mappings.filter {
      pm => pm.expose_endpoint.contains(true) && pm.`type`.exists(Set("external","loadBalancer").contains)
    }
    val lbPMs = containerSpec.port_mappings.filter {
      pm => pm.expose_endpoint.contains(true) && pm.`type`.contains("loadBalancer")
    }

    if (cipPMs.isEmpty) {
      ContainerServices(None,None,None)
    } else {
      ContainerServices(
        mkSvc(containerSpec.name,          Service.Type.ClusterIP,    cipPMs),
        mkSvc(containerSpec.name + "-ext", Service.Type.NodePort,     npPMs),
        mkSvc(containerSpec.name + "-lb",  Service.Type.LoadBalancer, lbPMs)
      )
    }
  }

  private[services] def mkKubernetesContainer(spec: ContainerSpec, provider: GestaltResourceInstance): skuber.Container = {
    val cpuRequest = ContainerService.getProviderProperty[String](provider, CPU_REQ_TYPE).getOrElse(DEFAULT_CPU_REQ).split(",")
    val memRequest = ContainerService.getProviderProperty[String](provider, MEM_REQ_TYPE).getOrElse(DEFAULT_MEM_REQ).split(",")

    val cpu: Resource.ResourceList = Map(skuber.Resource.cpu    -> f"${spec.cpus}%1.3f")
    val mem: Resource.ResourceList = Map(skuber.Resource.memory -> f"${spec.memory}%1.3fM")

    val cpuReq: Resource.ResourceList = if (cpuRequest.contains(REQ_TYPE_REQUEST)) cpu else Map()
    val cpuLim: Resource.ResourceList = if (cpuRequest.contains(REQ_TYPE_LIMIT))   cpu else Map()
    val memReq: Resource.ResourceList = if (memRequest.contains(REQ_TYPE_REQUEST)) mem else Map()
    val memLim: Resource.ResourceList = if (memRequest.contains(REQ_TYPE_LIMIT))   mem else Map()

    val requirements = skuber.Resource.Requirements(
      requests = cpuReq ++ memReq,
      limits = cpuLim ++ memLim
    )

    val commands = spec.cmd map CommandParser.translate

    val pullPolicy = {
      if (spec.force_pull) Container.PullPolicy.Always 
      else Container.PullPolicy.IfNotPresent
    }

    val environmentVars = List(
      skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
    ) ++ mkEnvVars(spec.env)
      
    skuber.Container(
      name      = spec.name,
      image     = spec.image,
      resources = Some(requirements),
      env       = environmentVars,
      ports     = mkPortMappingsSpec(spec.port_mappings).toList,
      imagePullPolicy = pullPolicy,
      args      = spec.args.getOrElse(Seq.empty).toList,
      command   = commands.getOrElse(Nil))    
  }

  /**
   * Create a Kubernetes Deployment object in memory.
   *
   * @param id UUID for the Meta Container. This will be used as a label on all of the created resourced for later indexing.
   * @param containerSpec ContainerSpec with Container data
   */
  private[services] def mkDeploymentSpec(kube: RequestContext, id: UUID, containerSpec: ContainerSpec, context: ProviderContext, namespace: String): Deployment = {

    val labels = containerSpec.labels ++ Map(
      META_CONTAINER_KEY -> id.toString,
      META_ENVIRONMENT_KEY -> context.environmentId.toString,
      META_WORKSPACE_KEY -> context.workspace.id.toString,
      META_FQON_KEY -> context.fqon,
      META_PROVIDER_KEY -> context.providerId.toString
    )


    val podTemplate = {

      /*
       * If there are any volumes in the Meta ContainerSpec, convert them to kube volumeMounts
       */
      val (storageVolumes,volMounts) = containerSpec.volumes.map({
        case ex: ExistingVolumeMountSpec =>
          val r = ResourceFactory.findById(migrations.V13.VOLUME_TYPE_ID, ex.volume_id).getOrElse(
            throw new BadRequestException("container spec had volume mount for non-existent volume")
          )
          val spec = VolumeSpec.fromResourceInstance(r).getOrElse(
            throw new InternalErrorException(s"could not parse referenced volume resource '${r.name}' as VolumeSpec")
          )
          val eid = spec.external_id.getOrElse(
            throw new InternalErrorException(s"Volume resource '${r.id}' did not have 'external_id'")
          )
          val eidExtracter = "/namespaces/([^/]+)/persistentvolumeclaims/(.*)".r
          val pvcName = eid match {
            case eidExtracter(_,name) /* TODO: need to check namespace */ => name
            case _ => throw new InternalErrorException(s"Volume resource '${r.id}' 'external_id' was not well-formed")
          }
          val readOnly = (spec.access_mode == ReadOnlyMany)
          val lbl = r.name // TODO: don't need this now: UUID.randomUUID().toString
          skuber.Volume(lbl, skuber.Volume.PersistentVolumeClaimRef(pvcName, readOnly)) -> Volume.Mount(lbl, ex.mount_path, readOnly)
        case _ =>
          throw new BadRequestException("container create/update only accepting volume mount to existing volumes; inline volume specs are not currently supported.")
      }).unzip


      /*
       * make sure the secrets all exist
       */
      val allEnvSecrets: Map[UUID, GestaltResourceInstance] = ResourceFactory.findChildrenOfType(ResourceIds.Secret, context.environmentId) map {
        res => res.id -> res
      } toMap

      containerSpec.secrets.foreach {
        secret =>
          allEnvSecrets.get(secret.secret_id) match {
            case None =>
              throw new UnprocessableEntityException(s"secret with ID '${secret.secret_id}' does not exist in environment '${context.environmentId}'")
            case Some(sec) if ContainerService.containerProviderId(sec) != context.providerId =>
              throw new UnprocessableEntityException(s"secret with ID '${secret.secret_id}' belongs to a different provider")
            case _ => ()
          }
      }

      if ( containerSpec.secrets.nonEmpty && containerSpec.secrets.map(_.path).distinct.size != containerSpec.secrets.size ) {
        throw new BadRequestException(s"secrets must have unique paths")
      }

      // Environment variable secrets
      val envSecrets = containerSpec.secrets.collect {
        case sem: SecretEnvMount => skuber.EnvVar(sem.path, EnvVar.SecretKeyRef(sem.secret_key, allEnvSecrets(sem.secret_id).name))
      }

      // Directory-mounted secrets: each needs a unique volume name
      val dirSecrets: Seq[(String, SecretDirMount)] = containerSpec.secrets.collect {
        case dir: SecretDirMount  => UUID.randomUUID.toString -> dir
      }
      val secDirMounts: Seq[Volume.Mount] = dirSecrets.map {
        case (secVolName,vsm) => Volume.Mount(secVolName,vsm.path, true)
      }
      val secretDirVolumes: Seq[Volume] = dirSecrets.collect {
        case (secretVolumeName, ContainerSpec.SecretDirMount(secret_id, path)) => Volume(secretVolumeName, Volume.Secret(allEnvSecrets(secret_id).name))
      }

      // verify it's a single health check and also resolve port_index
      val health_check = containerSpec.health_checks match {
        case Nil     => None
        case Seq(hc) => hc match {
          case checkWithPortIndex @ ContainerSpec.HealthCheck(_,_,_,_,_,_,_,Some(portIndex),None) if portIndex < 0 || portIndex >= containerSpec.port_mappings.size =>
            throw new UnprocessableEntityException(s"HealthCheck port_index '${portIndex}' was out of bounds for the provided port mappings array")
          case checkWithPortIndex @ ContainerSpec.HealthCheck(_,_,_,_,_,_,_,Some(portIndex),None) =>
            val pm = containerSpec.port_mappings(portIndex)
            pm.service_port orElse pm.container_port match {
              case None =>
                throw new UnprocessableEntityException(s"HealthCheck port_index '${portIndex}' referred to port mapping without either container_port or service_port.")
              case Some(indexedPort) =>
                Some(hc.copy(
                  port_index = None,
                  port = Some(indexedPort)
                ))
            }
          case otherCheck =>
            Some(otherCheck)
        }
        case _ => throw new UnprocessableEntityException("Kubernetes supports at most one health check/liveness probe.")
      }

      import ContainerSpec.HealthCheck._
      val livenessProbe = health_check map { hc =>
         hc match {
          case ContainerSpec.HealthCheck(protocol, maybePath, _, gracePeriod, _, timeoutSeconds, _, None, Some(portNumber)) if protocol.equalsIgnoreCase(HTTP) | protocol.equalsIgnoreCase(HTTPS) =>
            skuber.Probe(
              action = skuber.HTTPGetAction(
                port = Left(portNumber),
                host = "",
                path = maybePath getOrElse "",
                schema = protocol.toString.toUpperCase()
              ),
              initialDelaySeconds = gracePeriod,
              timeoutSeconds = timeoutSeconds
            )
          case ContainerSpec.HealthCheck(protocol, _, _, gracePeriod, _, timeoutSeconds, _, None, Some(portNumber)) if protocol.equalsIgnoreCase(TCP) =>
            skuber.Probe(
              action = skuber.TCPSocketAction(
                port = Left(portNumber)
              ),
              initialDelaySeconds = gracePeriod,
              timeoutSeconds = timeoutSeconds
            )
          case ContainerSpec.HealthCheck(protocol, _, Some(cmd), gracePeriod, _, timeoutSeconds, _, _, _) if protocol.equalsIgnoreCase(COMMAND) =>
            skuber.Probe(
              action = skuber.ExecAction(List("/bin/sh") ++ cmd.split(" ").toList),
              initialDelaySeconds = gracePeriod,
              timeoutSeconds = timeoutSeconds
            )
          case _ => throw new UnprocessableEntityException("Container health check was not well-formed")
        }
      }

      /*
       * SecretFileMount mountings have one path that has to be split into:
       *  - the Container's volumeMount.mountPath
       *  - the Pod Volume item path
       *
       * for a given container, all of the .volumeMounts[].mountPath must be unique
       * the uniqueness requirement means that we have to consider other SecretFileMount objects together

       * - combining them into one secret volume (and one mount at '/mnt/secrets/files') with both items, or
       * - keeping them as two volumes and two mounts with distinct mountPaths
       * the latter approach is possible in this case (e.g., mountPaths /mnt/secrets and /mnt/secrets/files), but will not be in general
       * for example, if the secret path had been /file-a and /file-b, there are no unique mount paths
       *
       * therefore, the approach here is to combine all sub-secret file mounts of a particular secret into one Volume, where
       * the mountPath for the Volume is calculated as the deepest merge path
       */
      def maxSharedRoot(pathParts: Seq[Seq[String]]): String = {
        @tailrec
        def _msr(pathParts: Seq[Seq[String]], accum: String): String = {
          val heads = pathParts.map(_.headOption)
          heads.head match {
            case Some(part) if (heads.forall(_.contains(part))) =>
              _msr(pathParts.map(_.tail), accum + "/" + part)
            case _ =>
              accum
          }
        }
        _msr(pathParts, "")
      }
      val fileSecretsBySecretId = containerSpec.secrets.collect {
        case file: SecretFileMount => file
      } groupBy {_.secret_id}
      val fileSecretVolumeNames: Map[UUID, String] = fileSecretsBySecretId.map{case (sid, _) => sid -> UUID.randomUUID().toString}
      // each secret becomes a Volume
      val fileSecretMountPaths = fileSecretsBySecretId mapValues {
        fileMounts =>
          val pathParts = fileMounts.map( _.path.stripPrefix("/").split("/").toSeq )
          pathParts match {
            case Seq(singleItem) =>
              "/" + singleItem.take(singleItem.size-1).mkString("/")
            case _ =>
              maxSharedRoot(pathParts)
          }

      }
      val secretFileMounts = fileSecretsBySecretId.keys.map {
        secretId => Volume.Mount(
          name = fileSecretVolumeNames(secretId),
          mountPath = fileSecretMountPaths(secretId),
          readOnly = true
        )
      }
      // each volume must be mounted
      val secretFileVolumes = fileSecretsBySecretId map {
        case (secretId, fileMounts) => Volume(
          name = fileSecretVolumeNames(secretId),
          source = Volume.Secret(
            secretName = allEnvSecrets(secretId).name,
            items = Some(fileMounts map {
              sfm => Volume.KeyToPath(
                key = sfm.secret_key,
                path = sfm.path.stripPrefix(fileSecretMountPaths(secretId)).stripPrefix("/")
              )
            } toList)
          )
        )
      }

      val container = mkKubernetesContainer(containerSpec, context.provider).copy(
        livenessProbe = livenessProbe
      )
      val affinity = ContainerService.getProviderProperty[Pod.Affinity](context.provider, "affinity")

      val baseSpec = Pod.Spec(
        affinity = affinity
      )
        .addContainer(container.copy(
          volumeMounts = (volMounts ++ secDirMounts ++ secretFileMounts).toList,
          env = container.env ++ envSecrets
        ))
        .withDnsPolicy(skuber.DNSPolicy.ClusterFirst)

      val specWithVols = (storageVolumes ++ secretDirVolumes ++ secretFileVolumes).foldLeft[Pod.Spec](baseSpec) { _.addVolume(_) }
      
      def DEFAULT_SECRET_NAME(idx: Int): String = "imagepullsecret-%d".format(idx)
      
      val finalSpec = specWithVols.copy(imagePullSecrets = List(
          LocalObjectReference(DEFAULT_SECRET_NAME(1)),
          LocalObjectReference(DEFAULT_SECRET_NAME(2)),
          LocalObjectReference(DEFAULT_SECRET_NAME(3)),
          LocalObjectReference(DEFAULT_SECRET_NAME(4)),
          LocalObjectReference(DEFAULT_SECRET_NAME(5)))
      )
      Pod.Template.Spec(spec = Some(finalSpec)).addLabels(labels)
    }

    Deployment(metadata = ObjectMeta(  
      name = containerSpec.name,
      namespace = namespace,
      labels = labels
    )).withTemplate(podTemplate)
      .withReplicas(containerSpec.num_instances)
      .withLabelSelector(LabelSelector(LabelSelector.IsEqualRequirement(
        META_CONTAINER_KEY, id.toString
      )))
  }

  private[services] def mkEnvVars(env: Map[String,String]): List[EnvVar] = {
    env.map { case (k,v) => EnvVar(k, EnvVar.StringValue(v)) }.toList
  }

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
    log.debug("find(_,_)")
    lazy val deplSplitter = "/namespaces/([^/]+)/deployments/(.*)".r
    ContainerService.resourceExternalId(container) match {
      case Some(deplSplitter(namespace,deploymentName)) =>
        cleanly(context.provider, namespace)( kube =>
          for {
            maybeDeployment <- kube.getOption[Deployment](deploymentName)
            allPods <- maybeDeployment match {
              case None    => Future.successful(Nil)
              case Some(_) => kube.list[PodList]() map (_.items)
            }
            allReplicaSets <- maybeDeployment match {
              case None    => Future.successful(Nil)
              case Some(_) => kube.list[ReplicaSetList]() map (_.items)
            }
            allEvents <- maybeDeployment match {
              case None    => Future.successful(Nil)
              case Some(_) => kube.list[EventList]() map (_.items)
            }
            allServices <- maybeDeployment match {
              case None    => Future.successful(None)
              case Some(_) => kube.list[ServiceList]().map {
                _.items.find(_.spec.exists(_._type == Service.Type.LoadBalancer))
              }
            }

            replicaSets = allReplicaSets.filter(_.metadata.ownerReferences.exists(owner =>
              (owner.kind ~== DEPLOYMENT) && owner.uid == maybeDeployment.get.metadata.uid))

            pods = allPods.filter(_.metadata.ownerReferences.exists(owner =>
              (owner.kind ~== REPLICA_SET) && replicaSets.exists(_.metadata.uid == owner.uid)))

            services = allServices.filter(service =>service.spec.isDefined &&
              (service.spec.get.selector.toSet diff maybeDeployment.get.metadata.labels.toSet).isEmpty)

            update = maybeDeployment map (kubeDeplAndPodsToContainerStatus(_, pods, services, allEvents))
          } yield update
        )
      case Some(externalId) => Future.failed(throw new RuntimeException(s"Invalid external_id: $externalId"))
      case None => Future.successful(None)
    }
  }

  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    import cats.syntax.traverse._
    import cats.instances.vector._
    import cats.instances.future._

    cleanly(context.provider, context.environment.id.toString) { kube0 =>

      kube0.getNamespaceNames flatMap { namespaces =>
        val prunedNamespaces = namespaces filter { namespace =>
          try {
            UUID.fromString(namespace)
            namespace == context.environment.id.toString
          } catch {
            case _: IllegalArgumentException => true
          }
        }
        val stats = Future.traverse(prunedNamespaces) { namespace =>
          val kube = kube0.usingNamespace(namespace)

          val envSelector = new LabelSelector(LabelSelector.IsEqualRequirement("meta/environment", context.environment.id.toString))

          val fDepls = kube.listSelected[DeploymentList](envSelector)
          // val fAllPods = kube.listSelected[PodList](envSelector)
          // val fAllServices = kube.listSelected[ServiceList](envSelector)
          val fAllEvents = kube.listSelected[EventList](envSelector)
          for {
            depls <- fDepls
            // allPods <- fAllPods
            // allServices <- fAllServices
            allEvents <- fAllEvents map (_.items)
            // _ = log.debug(s"listInEnvironment returned ${depls.size} deployments, ${allPods.size} pods, ${allEvents.size} events")
            _ = log.debug(s"${namespace}: listInEnvironment returned ${depls.size} deployments, ${allEvents.size} events")
            // stats = depls flatMap { depl =>
              // depl.metadata.labels.get(META_CONTAINER_KEY) map { id =>
              //   val thesePods = listByLabel[Pod,PodList](allPods, META_CONTAINER_KEY -> id)
              //   val maybeLbSvc = listByLabel[Service,ServiceList](allServices, META_CONTAINER_KEY -> id).find(_.spec.exists(_._type == Service.Type.LoadBalancer))
              //   log.debug(s"deployment for container ${id} selected ${thesePods.size} pods and ${maybeLbSvc.size} service")
              //   kubeDeplAndPodsToContainerStatus(depl, thesePods, maybeLbSvc, allEvents)
              // }
            // }
            deplsWithSelector = depls filter { depl => depl.spec.flatMap(_.selector).isDefined }
            stats <- deplsWithSelector.toVector traverse { depl =>
              val selector = depl.spec.flatMap(_.selector).get
              val fPods = kube.listSelected[PodList](selector)
              val fServices = kube.listSelected[ServiceList](selector)
              for(
                pods <- fPods;
                services <- fServices
              ) yield {
                val id = depl.metadata.labels.get(META_CONTAINER_KEY).getOrElse(s"name=${depl.name}")
                val lbs = services.find(_.spec.exists(_._type == Service.Type.LoadBalancer))
                log.debug(s"deployment for container ${id} selected ${pods.size} pods and ${lbs.size} service")
                kubeDeplAndPodsToContainerStatus(depl, pods, lbs, allEvents)
              }
            }
          } yield stats.toSeq
        }
        stats.map(_.flatten)
      }
    }
  }

  private[this] def kubeDeplAndPodsToContainerStatus(depl: Deployment,
                                                      pods: List[Pod],
                                                      maybeLbSvc: Option[Service],
                                                      events : List[Event] = Nil): ContainerStats = {
    // meta wants: SCALING, SUSPENDED, RUNNING, HEALTHY, UNHEALTHY
    // kube provides: waiting, running, terminated
    val containerStates = pods.flatMap(_.status.toSeq).flatMap(_.containerStatuses.headOption).flatMap(_.state)
    val podNames = pods.map(_.name)
    val numRunning = containerStates.count(_.isInstanceOf[skuber.Container.Running])
    val numTarget  = depl.spec.flatMap(_.replicas).getOrElse(numRunning) // TODO: don't really know what to do if this doesn't exist
    val lbAddress = maybeLbSvc.flatMap(_.status).flatMap(_.loadBalancer).flatMap(_.ingress.headOption).flatMap(
      i => i.hostName orElse i.ip
    )
    val status = if (numRunning != numTarget) "SCALING"
    else if (numRunning == 0 && 0 == numTarget) "SUSPENDED"
    else if (numRunning == numTarget) "RUNNING"
    else "UNKNOWN"
    val specImage = depl.getPodSpec.flatMap(_.containers.headOption).map(_.image).getOrElse("")
    val age = depl.metadata.creationTimestamp map skuberTimestampToJodaDateTime
    val hostPorts = (for {
      podspec <- depl.getPodSpec
      container <- podspec.containers.headOption
      hostPorts = container.ports.flatMap(_.hostPort)
    } yield hostPorts) getOrElse Seq.empty
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
          ports = hostPorts
        )
    }

    val podEventStats = events filter (podEvent => (podEvent.involvedObject.kind ~== POD) && podNames.contains(podEvent.involvedObject.name)) map eventStatFromPodEvent

    val containerStateStats = (pods flatMap containerStateStatFromPodContainerStates) ++ (pods flatMap containerStateStatFromPodConditions)

    ContainerStats(
      external_id = s"/namespaces/${depl.namespace}/deployments/${depl.name}",
      containerType = "DOCKER",
      status = status,
      cpus = (resources.flatMap(_.limits.get("cpu")) orElse resources.flatMap(_.requests.get("cpu")))
        .map(_.amount).map(_.toDouble).getOrElse(0),
      memory = (resources.flatMap(_.limits.get("memory")) orElse resources.flatMap(_.requests.get("memory")))
        .map(_.amount).map(_.toDouble / 1e6).getOrElse(0),
      image = specImage,
      age = age.getOrElse(DateTime.now()),
      numInstances = numTarget,
      tasksStaged = 0,
      tasksRunning = numRunning,
      tasksHealthy = 0,
      tasksUnhealthy = 0,
      taskStats = Some(taskStats),
      events = Some(podEventStats),
      states = Some(containerStateStats),
      lb_address = lbAddress
    )
  }

    def eventStatFromPodEvent(podEvent: skuber.Event): EventStat = {
      val eventAge = podEvent.metadata.creationTimestamp map skuberTimestampToJodaDateTime
      val (eventSourceComponent, eventSourceOption) = podEvent.source match {
        case None => (None, None)
        case Some(source) => (source.component, source.host)
      }
      EventStat(
        objectName = podEvent.involvedObject.name,
        objectType = POD,
        eventType = podEvent.`type`.getOrDefault(),
        reason = podEvent.reason.getOrDefault(),
        sourceComponent = eventSourceComponent.getOrDefault(),
        sourceHost = eventSourceOption.getOrDefault(),
        message = podEvent.message.getOrDefault(),
        age = eventAge.getOrElse(DateTime.now)
      )
  }

  def containerStateStatFromPodContainerStates(pod: skuber.Pod) : Option[ContainerStateStat] = {
    val maybeContainerState = pod.status.flatMap(_.containerStatuses.headOption).flatMap(_.state)
    maybeContainerState.map(containerState => {
      containerState match {
        case terminated: skuber.Container.Terminated => {
          val terminationDateTime = terminated.finishedAt map skuberTimestampToJodaDateTime
          ContainerStateStat(
            objectName = pod.name,
            objectType = POD,
            stateId = terminated.id,
            reason = terminated.reason,
            message = terminated.message,
            finishedAt = terminationDateTime,
            priority = 3
          )
        }
        case waiting: skuber.Container.Waiting => ContainerStateStat(
          objectName = pod.name,
          objectType = POD,
          stateId = waiting.id,
          reason = waiting.reason,
          priority = 2
        )
        case running: skuber.Container.Running => ContainerStateStat(
          objectName = pod.name,
          objectType = POD,
          stateId = running.id
        )
          //should never happen
        case _ => ContainerStateStat(
          objectName = pod.name,
          objectType = POD
        )
      }
    })
  }

  def containerStateStatFromPodConditions(pod: skuber.Pod) : Option[ContainerStateStat] = {
    pod.status flatMap (podStatus => {
      podStatus.conditions.find(_.status == "False").map(condition => {
        ContainerStateStat(
          objectName = pod.name,
          objectType = POD,
          stateId = podStatus.phase.map(_.toString.toLowerCase).getOrElse("unknown"),
          reason = condition.reason,
          message = condition.message,
          priority = 1
        )
      })
    })
  }

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = cleanly(context.provider, context.environmentId.toString) { kube =>
    for {
      extantDepl <- kube.getOption[Deployment](container.name) flatMap {
        case Some(depl) => Future.successful(depl)
        case None => Future.failed(new RuntimeException(
          s"could not locate associated Deployment in Kubernetes provider for container ${container.id}"
        ))
      }
      updatedDepl <- {
        val newDepl = extantDepl.withReplicas(numInstances)
        log.debug(s"updating deployment to scale: ${Json.toJson(newDepl)}")
        kube.update(newDepl)
      }
      updatedNumInstances = updatedDepl.spec.flatMap(_.replicas).getOrElse(
        throw new RuntimeException(s"updated Deployment for container ${container.id} did not have a spec, so that replica size could not be determined")
      )
    } yield upsertProperties(
      container,
      "num_instances" -> s"${updatedNumInstances}"
    )
  }

  def createVolumeResources(context: ProviderContext,
                            metaResource: ResourceLike,
                            maybePV: Option[(skuber.Namespace => skuber.PersistentVolume)],
                            pvc: skuber.Namespace => skuber.PersistentVolumeClaim): Future[PersistentVolumeClaim] = {
    for {
      namespace <- cleanly(context.provider, DefaultNamespace)( getNamespace(_, context, create = true) )
      pvc       <- cleanly(context.provider, namespace.name  ){ kube =>
        for {
          pv <- maybePV match {
            case Some(pv) =>
              kube.create[PersistentVolume](pv(namespace)) recoverWith { case e: K8SException =>
                Future.failed(new RuntimeException(s"Failed creating PersistentVolume for volume '${metaResource.name}': " + e.status.message))
              } map(Some(_))
            case None =>
              Future.successful(None)
          }
          pvc <- kube.create[PersistentVolumeClaim](pvc(namespace)) recoverWith { case e: K8SException =>
            Future.failed(new RuntimeException(s"Failed creating PersistentVolumeClaim for volume '${metaResource.name}': " + e.status.message))
          }
        } yield pvc
      }
    } yield pvc
  }

  override def createVolume(context: ProviderContext, metaResource: Instance)
                           (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    for {
      spec <- Future.fromTry(VolumeSpec.fromResourceInstance(metaResource))
      config <- Future.fromTry(Try(spec.parseConfig.get))
      v <- config match {
        case VolumeSpec.HostPathVolume(hostPath) if isAllowedHostPath(context.provider, hostPath) =>
          createVolumeResources(
            context,
            metaResource,
            Some(mkPV(_, metaResource, spec, context, HostPath(hostPath))),
            mkPVC(_, metaResource, spec, context)
          ) map { pvc =>
            upsertProperties(
              metaResource,
              "external_id" -> s"/namespaces/${pvc.namespace}/persistentvolumeclaims/${pvc.name}",
              "reclamation_policy" -> "delete_persistent_volume"
            )
          }
        case VolumeSpec.HostPathVolume(hostPath) =>
          Future.failed(new UnprocessableEntityException(s"host_path '$hostPath' is not in provider's white-list"))
        case VolumeSpec.PersistentVolume =>
          Future.failed(new BadRequestException("Kubernetes providers only support volumes of type 'external', 'dynamic' and 'host_path'"))
        case VolumeSpec.ExternalVolume(config) =>
          createVolumeResources(
            context,
            metaResource,
            Some(mkPV(_, metaResource, spec, context, GenericVolumeSource(config.toString))),
            mkPVC(_, metaResource, spec, context)
          ) map { pvc =>
            upsertProperties(
              metaResource,
              "external_id" -> s"/namespaces/${pvc.namespace}/persistentvolumeclaims/${pvc.name}",
              "reclamation_policy" -> "delete_persistent_volume"
            )
          }
        case VolumeSpec.DynamicVolume(storageClass) if isConfiguredStorageClass(context.provider, storageClass) =>
          createVolumeResources(
            context,
            metaResource,
            None,
            mkPVC(_, metaResource, spec, context, maybeStorageClass = Some(storageClass))
          ) map { pvc =>
            upsertProperties(
              metaResource,
              "external_id" -> s"/namespaces/${pvc.namespace}/persistentvolumeclaims/${pvc.name}"
            )
          }
        case VolumeSpec.DynamicVolume(storageClass) =>
          Future.failed(new UnprocessableEntityException(s"storage_class '$storageClass' is not in provider's white-list"))
      }
    } yield v
  }

  override def destroyVolume(volume: ResourceLike): Future[Unit] = {

    def doTheDelete: Future[Unit] = {
      val provider = ContainerService.containerProvider(volume)
      /*
       * TODO: Change signature of deleteVolume to take a ProviderContext - providers
       * must never user ResourceFactory directly.
       */
      val environment: String = {
        ResourceFactory.findParent(ResourceIds.Environment, volume.id).map(_.id.toString) orElse {
          val namespaceGetter = "/namespaces/([^/]+)/persistentvolumeclaims/.*".r
          for {
            eid <- ContainerService.resourceExternalId(volume)
            ns <- eid match {
              case namespaceGetter(namespace) => Some(namespace)
              case _ => None
            }
          } yield ns
        } getOrElse {
          throw new RuntimeException(s"Could not find Environment for volume '${volume.id}' in Meta.")
        }
      }

      val deletePVs = volume.properties.getOrElse(Map.empty).get("reclamation_policy").contains("delete_persistent_volume")

      val targetLabel = META_VOLUME_KEY -> volume.id.toString
      cleanly(provider, environment) { kube =>
        val fVolumeDels = for {
          pvcs <- deleteAllWithLabel[PersistentVolumeClaim](kube, targetLabel)
          _    <- if (deletePVs) deleteAllWithLabel[PersistentVolume](kube, targetLabel) else Future.successful(Seq.empty)
        } yield pvcs.headOption.getOrElse({
          log.debug("deleted no PersistentVolumeClaims")
          ()
        })

        fVolumeDels map {_ =>
          log.debug(s"finished deleting PersistentVolumes and PersistentVolumeClaims for volume ${volume.id}")
          ()
        }
      }
    }

    for {
      spec <- Future.fromTry(VolumeSpec.fromResourceInstance(volume))
      d <- spec.`type` match {
        case VolumeSpec.External | VolumeSpec.Dynamic | VolumeSpec.HostPath =>
          doTheDelete
        case other =>
          log.info(s"destroyVolume(): nothing to do for type ${other}")
          Future.successful(())
      }
    } yield d
  }

  override def updateVolume(context: ProviderContext, metaResource: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    log.warn("KubernetesService::updateVolume is currently a no-op and is not expected to be called")
    Future.successful(metaResource)
  }

  def skuberTimestampToJodaDateTime(timestamp: skuber.Timestamp): DateTime = {
    new DateTime(
      timestamp.toInstant().toEpochMilli(),
      DateTimeZone.forTimeZone(TimeZone.getTimeZone(timestamp.getZone())))
  }

}

