package services

import java.util.{Base64, TimeZone, UUID}

import org.slf4j.LoggerFactory
import services.util.CommandParser

import scala.language.implicitConversions
import scala.language.reflectiveCalls
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.Future
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, ResourceIds}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.{ContainerSpec, ContainerStats, SecretSpec}
import com.google.inject.Inject
import skuber._
import skuber.api.client._
import skuber.ext._
import skuber.json.format._
import skuber.json.ext.format._
import skuber.Container.Port
import skuber.LabelSelector.dsl._
import com.galacticfog.gestalt.caas.kube._
import controllers.util._
import com.galacticfog.gestalt.meta.api.ContainerSpec.{PortMapping, SecretDirMount, SecretEnvMount, SecretFileMount, SecretMount, VolumeSecretMount}
import org.joda.time.{DateTime, DateTimeZone}

import scala.concurrent.ExecutionContext
import scala.reflect.runtime.universe._
import scala.language.postfixOps
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.annotation.tailrec

object KubernetesService {
  val META_CONTAINER_KEY = "meta/container"
  val META_SECRET_KEY = "meta/secret"
  val META_ENVIRONMENT_KEY = "meta/environment"
  val META_WORKSPACE_KEY = "meta/workspace"
  val META_FQON_KEY = "meta/fqon"
  val META_PROVIDER_KEY = "meta/provider"

  val CPU_REQ_TYPE = "cpu-requirement-type"
  val MEM_REQ_TYPE = "memory-requirement-type"

  val REQ_TYPE_LIMIT = "limit"
  val REQ_TYPE_REQUEST = "request"

  val DEFAULT_CPU_REQ = REQ_TYPE_REQUEST
  val DEFAULT_MEM_REQ = Seq(REQ_TYPE_LIMIT,REQ_TYPE_REQUEST).mkString(",")

  def unprocessable(message: String) = throw UnprocessableEntityException(message)
}

class KubernetesService @Inject() ( skuberFactory: SkuberFactory )
  extends CaasService with JsonInput with MetaControllerUtils {
  
  import KubernetesService._

  private[this] val log = LoggerFactory.getLogger(this.getClass)
  private[services] val DefaultNamespace = "default"

  def cleanly[T](providerId: UUID, namespace: String)(f: RequestContext => Future[T]): Future[T] = {
    skuberFactory.initializeKube(providerId, namespace) flatMap { kube =>
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
          namespace <- cleanly(context.provider.id, DefaultNamespace)( getNamespace(_, context, create = true) )
          output    <- cleanly(context.provider.id, namespace.name  )( createKubeSecret(_, secret.id, specWithItems, namespace.name, context) )
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
    cleanly(provider.id, environment) { kube =>
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
      namespace  <- cleanly(context.provider.id, DefaultNamespace)( getNamespace(_, context, create = true) )
      updatedContainerSpec <- cleanly(context.provider.id, namespace.name)( createDeploymentEtAl(_, container.id, spec, namespace.name, context) )
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
        namespace  <- cleanly(context.provider.id, DefaultNamespace)( getNamespace(_, context, create = true) )
        updatedContainerSpec <- cleanly(context.provider.id, namespace.name)( kube =>
            updateDeploymentEtAl(kube, container.id, spec, namespace.name, context)
        )
      } yield upsertProperties(
        container,
        "port_mappings" -> Json.toJson(updatedContainerSpec.port_mappings).toString()
      )
    }
  }

  /**
   * Get the Kubernetes Namespace for the given Environment ID. Namespaces are named after the
   * Environment UUID. If a corresponding namespace is not found, it will be created if create is 'true'
   *
   * @param rc RequestContext for communicating with Kubernetes
   * @param pc ProviderContext of the Meta Environment you want a Namespace for
   * @param create when set to true, a new Namespace will be created if and existing one is not found
   */
  private[services] def getNamespace(rc: RequestContext, pc: ProviderContext, create: Boolean = false): Future[Namespace] = {
    log.debug("getNamespace(environment = {}, create = {}", pc.environmentId, create)
    rc.getOption[Namespace](pc.environmentId.toString) flatMap {
      case Some(s) =>
        log.debug("Found Kubernetes namespace: {}", pc.environmentId)
        Future.successful(s)
      case None if create =>
        log.debug("Creating new Kubernetes namespace: {}", pc.environmentId)
        rc create[Namespace] Namespace(metadata = ObjectMeta(
          name = pc.environmentId.toString,
          labels = Map(
            META_ENVIRONMENT_KEY -> pc.environmentId.toString,
            META_WORKSPACE_KEY -> pc.workspace.id.toString,
            META_FQON_KEY -> pc.fqon,
            META_PROVIDER_KEY -> pc.providerId.toString
          )
        ))
      case None if !create =>
        log.error("No namespace found for environment '{}' - create == false")
        Future.failed(UnprocessableEntityException(s"There is no Namespace corresponding with Environment '{}', environment"))
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
    kube.list[ServiceList](LabelSelector(LabelSelector.IsEqualRequirement(
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
    for {
    /*
     * This creates any volume claims that may not exist and tests each creation
     * for failure. If a failure is detected, processing stops and an exception is thrown.
     */
      _ <- reconcileVolumeClaims(kube, namespace, spec.volumes)
      fDeployment = kube.create[Deployment](mkDeploymentSpec(kube, containerId, spec, context, namespace))
      fUpdatedPMsFromService = createServices(kube, namespace, containerId, spec, context) recover {
        case e: Throwable =>
          log.error(s"error creating Kubernetes Service for container ${containerId}; assuming that it was not created",e)
          spec.port_mappings
      }
      fIngress = createIngress(kube, namespace, containerId, spec, context) recover {
        case e: Throwable =>
          log.error(s"error creating Kubernetes Ingress for container ${containerId}; assuming that it was not created",e)
          ()
      }
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
    for {
    /*
     * This creates any volume claims that may not exist and tests each creation
     * for failure. If a failure is detected, processing stops and an exception is thrown.
     */
      _ <- reconcileVolumeClaims(kube, namespace, spec.volumes)

      fDepl = kube.update[Deployment](mkDeploymentSpec(kube, containerId, spec, context, namespace))
      fUpdatedPMsFromService = updateServices(kube, namespace, containerId, spec, context) recover {
        case e: Throwable =>
          log.error(s"error creating Kubernetes Service for container ${containerId}; assuming that it was not created",e)
          spec.port_mappings
      }
      fIngress = updateIngress(kube, namespace, containerId, spec, context) recover {
        case e: Throwable =>
          log.error(s"error updating Kubernetes Ingress for container ${containerId}; assuming that is in an invalid state",e)
          ()
      }

      dep <- fDepl
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
    cleanly(provider.id, environment) { kube =>
      val fDplDel = for {
        deps <- deleteAllWithLabel[Deployment](kube, targetLabel)
        rses <- deleteAllWithLabel[ReplicaSet](kube, targetLabel)
        pods <- deleteAllWithLabel[Pod](kube, targetLabel) recover {
          case e: K8SException =>
            log.warn(s"K8S error listing/deleting Pods associated with container ${container.id}", e)
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
          log.warn(s"K8S error listing/deleting Services associated with container ${container.id}", e)
          ()
      }

      val fIngDel = (for {
        ings <- deleteAllWithLabel[Ingress](kube, targetLabel)
      } yield ings.headOption.getOrElse({
        log.debug("deleted no Ingresses")
        ()
      })) recover {
        case e: K8SException =>
          log.warn(s"K8S error listing/deleting Ingresses associated with container ${container.id}", e)
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
      foundResources <- kube.list[ListResource[O]](label._1 is label._2) map (_.items)
      _ = log.debug(s"found ${foundResources.size} ${otype} resources")
      deletes <- Future.traverse(foundResources){
        d =>
          log.info(s"deleting ${typeOf[O]} ${d.name} labeled with ${label}")
          kube.delete[O](d.name)
      }
    } yield deletes
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

    resourceWithDefaults(org, containerResourceInput, user, None)
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

    log.debug("svcExt: " + services.extSvc.toString)
    services.intSvc match {
      case None =>
        log.debug(s"updateContainerSpecPortMappings: No Service(ClusterIP)")
        pms.map(_.copy(service_address = None))
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
              log.debug(s"nodePort from svcExt for ${pm.name.get}: ${nodePort}")
              pm.copy(
                service_port = nodePort,
                service_address = Some(ContainerSpec.ServiceAddress(
                  host = svcHost,
                  port = sp.port,
                  protocol = Some(pm.protocol)
                )),
                lb_port = Some(sp.port),
                `type` = pm.`type` orElse(Some("clusterIP"))
              )
            case _ =>
              log.debug(s"updateContainerSpecPortMappings: PortMapping ${pm} not matched")
              pm.copy(
                service_address = None,
                lb_port = None,
                expose_endpoint = Some(false),
                `type` = None
              )
          }
        }
    }
  }

  private[services] def mkServiceSpecs(containerId: UUID, containerSpec: ContainerSpec, namespace: String, context: ProviderContext): ContainerServices = {

    // nodePort and loadBalancer are always counted as a clusterIp
    // likewise, loadBalancer is always counted as a nodePort
    // therefore, we have clusterIp superset nodePort superset loadBalancer

    def mkSvc(svcName: String, serviceType: String, pms: Seq[ContainerSpec.PortMapping]): Option[Service] = {
      if (pms.isEmpty) {
        None
      } else {
        Some(pms.foldLeft[Service](
          Service(metadata = ObjectMeta(name = svcName, namespace = namespace))
            .withType(
              serviceType match {
                case "nodePort"     => Service.Type.NodePort
                case "loadBalancer" => Service.Type.LoadBalancer
                case              _ => Service.Type.ClusterIP
              }
            )
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
              nodePort = pm.service_port.filter(_ => serviceType != "clusterIp").getOrElse(0)
            ))
        })
      }
    }

    val cipPMs = containerSpec.port_mappings.filter {
      pm => pm.expose_endpoint.contains(true)
    }
    val npPMs = containerSpec.port_mappings.filter {
      pm => pm.expose_endpoint.contains(true) && pm.`type`.exists(t => t == "nodePort" || t == "loadBalancer")
    }
    val lbPMs = containerSpec.port_mappings.filter {
      pm => pm.expose_endpoint.contains(true) && pm.`type`.exists(t => t == "loadBalancer")
    }

    if (cipPMs.isEmpty) {
      ContainerServices(None,None,None)
    } else {
      ContainerServices(
        mkSvc(containerSpec.name, "clusterIp", cipPMs),
        mkSvc(containerSpec.name + "-ext", "nodePort", npPMs),
        mkSvc(containerSpec.name + "-lb", "loadBalancer", lbPMs)
      )
    }
  }


  def reconcileVolumeClaims(kube: RequestContext, namespace: String, volumes: Seq[ContainerSpec.Volume]): Future[Seq[PersistentVolumeClaim]] = {
    for {
      pvcs <- kube.list[PersistentVolumeClaimList]
      volumeClaims = volumes flatMap {v => KubeVolume(v).asVolumeClaim(namespace)}
      out <- Future.sequence(volumeClaims map { pvc =>
        pvcs.items.find(_.name.equalsIgnoreCase(pvc.name)) match {
          case Some(matchingPvc) =>
            log.debug(s"Found existing volume claim with name '${matchingPvc.name}'")
            Future.successful(matchingPvc)
          case None =>
            log.info(s"Creating new Persistent Volume Claim '${pvc.name}'")
            kube.create[PersistentVolumeClaim](pvc) recoverWith {
              case e: K8SException =>
                unprocessable(s"Failed creating PVC for volume '${pvc.name}': " + e.status.message)
            }
        }
      })
    } yield out
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
          skuber.EnvVar("POD_IP", 
          skuber.EnvVar.FieldRef("status.podIP"))
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
      val volMounts: Seq[Volume.Mount] = containerSpec.volumes map { v => KubeVolume(v).asVolumeMount }

      /*
       * each kube volume needs a name, used for reference internal to the deployment, generate a uuid to use
       */

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
              throw new UnprocessableEntityException(s"secret with ID '${secret.secret_id}' does not exist in environment ${context.environmentId}'")
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

      val allowedHostPaths = ContainerService.getProviderProperty[Seq[String]](context.provider, "host_volume_whitelist").getOrElse(Seq.empty)

      val storageVolumes = containerSpec.volumes map { v =>
        val name = v.name getOrElse {
          unprocessable("Malformed volume specification. Must supply 'name'")
        }
        v match {
          case ContainerSpec.Volume(containerPath, Some(hostPath), None, maybeMode, Some(name)) =>
            val hpParts = hostPath.stripSuffix("/").stripPrefix("/").split("/").scan("")(_ + "/" + _)
            if (hpParts.exists(part => allowedHostPaths.map(_.stripSuffix("/")).contains(part))) {
              Volume(
                name = name,
                source = Volume.HostPath(hostPath)
              )
            } else {
              unprocessable("host_path is not in provider's white-list")
            }
          case ContainerSpec.Volume(containerPath, None, Some(persistentVolumeInfo), maybeMode, Some(name)) =>
            Volume(
              name = name,
              source = Volume.PersistentVolumeClaimRef(
                claimName = name,
                readOnly = KubeVolume.resolveAccessMode(maybeMode) == PersistentVolume.AccessMode.ReadOnlyMany
              )
            )
          case _ =>
            unprocessable("Could not resolve ContainerSpec.Volume to an appropriate volume mount.")
        }
      }

      val specWithVols = (storageVolumes ++ secretDirVolumes ++ secretFileVolumes).foldLeft[Pod.Spec](baseSpec) { _.addVolume(_) }
      Pod.Template.Spec(spec = Some(specWithVols)).addLabels(labels)
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
    val lbl = META_CONTAINER_KEY -> container.id.toString
    cleanly(context.providerId, context.environment.id.toString)( kube =>
      for {
        maybeDepl <- kube.list[DeploymentList](LabelSelector(LabelSelector.IsEqualRequirement(lbl._1, lbl._2))) map (_.items.headOption)
        pods <- maybeDepl match {
          case None    => Future.successful(Nil)
          case Some(_) => kube.list[PodList](LabelSelector(LabelSelector.IsEqualRequirement(lbl._1, lbl._2))) map (_.items)
        }
        maybeSvc <- maybeDepl match {
          case None    => Future.successful(None)
          case Some(_) => kube.list[ServiceList](LabelSelector(LabelSelector.IsEqualRequirement(lbl._1, lbl._2))) map (_.items.headOption)
        }
        update = maybeDepl map (kubeDeplAndPodsToContainerStatus(_, pods, maybeSvc))
      } yield update
    )
  }

  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = cleanly(context.providerId, context.environment.id.toString) { kube =>
    val fDepls = kube.list[DeploymentList]
    val fAllPods = kube.list[PodList]()
    val fAllServices = kube.list[ServiceList]
    for {
      depls <- fDepls
      allPods <- fAllPods
      allServices <- fAllServices
      _ = log.debug(s"listInEnvironment returned ${depls.size} deployments and ${allPods.size} pods")
      stats = depls.flatMap (depl =>
        depl.metadata.labels.get(META_CONTAINER_KEY) map { id =>
          val thesePods = listByLabel[Pod,PodList](allPods, META_CONTAINER_KEY -> id)
          val theseSvcs = listByLabel[Service,ServiceList](allServices, META_CONTAINER_KEY -> id)
          log.debug(s"deployment for container ${id} selected ${thesePods.size} pods and ${theseSvcs.size} services")
          val thisSvc = theseSvcs match {
            case Nil => None
            case head :: tail =>
              if (tail.nonEmpty) log.warn("found multiple services corresponding to container, will use only the first one")
              Some(head)
          }
          kubeDeplAndPodsToContainerStatus(depl, thesePods, thisSvc)
        }
      )
    } yield stats
  }

  private[this] def kubeDeplAndPodsToContainerStatus(depl: Deployment, pods: List[Pod], service: Option[Service]): ContainerStats = {
    // meta wants: SCALING, SUSPENDED, RUNNING, HEALTHY, UNHEALTHY
    // kube provides: waiting, running, terminated
    val containerStates = pods.flatMap(_.status.toSeq).flatMap(_.containerStatuses.headOption).flatMap(_.state)
    val numRunning = containerStates.count(_.isInstanceOf[skuber.Container.Running])
    val numTarget  = depl.spec.flatMap(_.replicas).getOrElse(numRunning) // TODO: don't really know what to do if this doesn't exist
    val status = if (numRunning != numTarget) "SCALING"
    else if (numRunning == 0 && 0 == numTarget) "SUSPENDED"
    else if (numRunning == numTarget) "RUNNING"
    else "UNKNOWN"
    val specImage = depl.getPodSpec.flatMap(_.containers.headOption).map(_.image).getOrElse("")
    val age = depl.metadata.creationTimestamp.map(
      zdt => new DateTime(
        zdt.toInstant().toEpochMilli(),
        DateTimeZone.forTimeZone(TimeZone.getTimeZone(zdt.getZone())))
    )
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
      taskStats = Some(taskStats)
    )
  }

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = cleanly(context.provider.id, context.environmentId.toString) { kube =>
    for {
      extantDepl <- kube.getOption[Deployment](container.name) flatMap {
        case Some(depl) => Future.successful(depl)
        case None => Future.failed(new RuntimeException(
          s"could not locate associated Deployment in Kubernetes provider for container ${container.id}"
        ))
      }
      updatedDepl <- {
        val newDepl = extantDepl.withReplicas(numInstances)
        log.debug("updating deployment to scale:\n" + Json.prettyPrint(Json.toJson(newDepl)))
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

}


