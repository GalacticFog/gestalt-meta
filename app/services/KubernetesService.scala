package services

import java.util.{TimeZone, UUID}

import org.slf4j.LoggerFactory
import services.util.CommandParser

import scala.language.implicitConversions
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.Future
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, ResourceIds}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.google.inject.Inject

import skuber._
import skuber.api.client._
import skuber.ext._
import skuber.json.format._
import skuber.json.ext.format._
import skuber.api.client.ObjKind
import skuber.Container.Port

import com.galacticfog.gestalt.caas.kube._
import controllers.util._
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.marathon.ContainerStats
import com.galacticfog.gestalt.meta.api.ContainerSpec.PortMapping
import org.joda.time.{DateTime, DateTimeZone}

import scala.concurrent.ExecutionContext
import scala.reflect.runtime.universe._
import scala.language.postfixOps

import play.api.libs.json._
import play.api.libs.functional.syntax._

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

  // TODO: skuber issue #38 is a formatting error around Ingress rules with empty path
  implicit val ingressBackendFmt: Format[Ingress.Backend] = Json.format[Ingress.Backend]
  // we'll override that formatter until then
  implicit val ingressPathFmt: Format[Ingress.Path] = (
    (JsPath \ "path").formatMaybeEmptyString() and
      (JsPath \ "backend").format[Ingress.Backend]
    ) (Ingress.Path.apply _, unlift(Ingress.Path.unapply))
  implicit val ingressHttpRuledFmt: Format[Ingress.HttpRule] = Json.format[Ingress.HttpRule]
  implicit val ingressRuleFmt: Format[Ingress.Rule] = Json.format[Ingress.Rule]
  implicit val ingressTLSFmt: Format[Ingress.TLS] = Json.format[Ingress.TLS]

  implicit val ingressSpecFormat: Format[Ingress.Spec] = (
    (JsPath \ "backend").formatNullable[Ingress.Backend] and
      (JsPath \ "rules").formatMaybeEmptyList[Ingress.Rule] and
      (JsPath \ "tls").formatMaybeEmptyList[Ingress.TLS]
    )(Ingress.Spec.apply _, unlift(Ingress.Spec.unapply))

  implicit val ingrlbingFormat: Format[Ingress.Status.LoadBalancer.Ingress] =
    Json.format[Ingress.Status.LoadBalancer.Ingress]

  implicit val ingrlbFormat: Format[Ingress.Status.LoadBalancer] =
    Json.format[Ingress.Status.LoadBalancer]

  implicit val ingressStatusFormat = Json.format[Ingress.Status]

  implicit lazy val ingressFormat: Format[Ingress] = (
    objFormat and
      (JsPath \ "spec").formatNullable[Ingress.Spec] and
      (JsPath \ "status").formatNullable[Ingress.Status]
    ) (Ingress.apply _, unlift(Ingress.unapply))

  implicit val ingressListFmt: Format[IngressList] = KListFormat[Ingress].apply(IngressList.apply _,unlift(IngressList.unapply))

  implicit val depSpecFmt: Format[Deployment.Spec] = (
    (JsPath \ "replicas").format[Int] and
      (JsPath \ "selector").formatNullableLabelSelector and
      (JsPath \ "template").formatNullable[Pod.Template.Spec] and
      (JsPath \ "strategy").formatNullable[Deployment.Strategy] and
      (JsPath \ "minReadySeconds").formatMaybeEmptyInt()
    )(Deployment.Spec.apply _, unlift(Deployment.Spec.unapply))

  implicit lazy val depFormat: Format[Deployment] = (
    objFormat and
      (JsPath \ "spec").formatNullable[Deployment.Spec] and
      (JsPath \ "status").formatNullable[Deployment.Status]
    ) (Deployment.apply _, unlift(Deployment.unapply))
  
  def createSecret(context: ProviderContext, secret: GestaltResourceInstance)(
      implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    
    ContainerSecret.fromResource(secret) match {
      case Failure(e) => Future.failed(e)
      case Success(sec) =>
        for {
          kube       <- skuberFactory.initializeKube(context.provider.id, DefaultNamespace)
          namespace  <- getNamespace(kube, context.environment.id, create = true)
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
        kube       <- skuberFactory.initializeKube(context.provider.id, DefaultNamespace)
        namespace  <- getNamespace(kube, context.environment.id, create = true)
        kube       <- skuberFactory.initializeKube(context.provider.id, namespace.name)
        updatedContainerSpec <- createDeploymentEtAl(kube, container.id, spec, namespace.name)
      } yield upsertProperties(
        container,
        "external_id" -> s"/namespaces/${namespace.name}/deployments/${container.name}",
        "status" -> "LAUNCHED",
        "port_mappings" -> Json.toJson(updatedContainerSpec.port_mappings).toString()
      )
    }
  }

  def update(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    log.debug("update(...)")
    val nameGetter = "/namespaces/[^/]+/deployments/(.*)".r
    val previousName = for {
      eid <- containerExternalId(container)
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
        kube       <- skuberFactory.initializeKube(context.provider.id, DefaultNamespace)
        namespace  <- getNamespace(kube, context.environment.id, create = true)
        kube       <- skuberFactory.initializeKube(context.provider.id, namespace.name)
        updatedContainerSpec <- updateDeploymentEtAl(kube, container.id, spec, namespace.name)
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

  def updateIngress(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec): Future[Option[Ingress]] = {
    // start this async call early, we'll need it later
    val fExistingIngress = kube.getOption[Ingress](spec.name)
    /*
     * create new ingress spec
     * if empty, delete any existing ingress
     * if not empty, update or create
     */
    val maybeNewIng = mkIngressSpec(containerId, spec, namespace)
    fExistingIngress flatMap { maybeExistingIng =>
      (maybeExistingIng,maybeNewIng) match {
        case (None, None)            => Future.successful(None)
        case (Some(_), None)         => kube.delete[Ingress](spec.name) map (_ => None)
        case (None, Some(create))    => kube.create[Ingress](create) map (Some(_))
        case (Some(_), Some(update)) => kube.update[Ingress](update) map (Some(_))
      }
    }
  }

  def createIngress(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec): Future[Option[Ingress]] = {
    mkIngressSpec(containerId, spec, namespace).fold { Future.successful(Option.empty[Ingress]) }
                                                 { kube.create[Ingress](_) map(Some(_)) }
  }

  def updateService(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec): Future[Seq[PortMapping]] = {
    // start this async call early, we'll need it later
    val fExistingSvc = kube.getOption[Service](spec.name)
    /*
     * create new service spec
     * if empty, delete any existing service
     * if not empty, update or create
     */
    val (maybeNewSvc, newPMs) = mkServiceSpec(containerId, spec, namespace)
    fExistingSvc flatMap { maybeExistingSvc =>
      (maybeExistingSvc, maybeNewSvc) match {
        case (None, None)            => Future.successful(())
        case (Some(_), None)         => kube.delete[Service](spec.name)
        case (None, Some(create))    => kube.create[Service](create)
        case (Some(_), Some(update)) => kube.update[Service](update)
      }
    } map (_ => newPMs)
  }

  def createService(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec): Future[Seq[PortMapping]] = {
    val (maybeService,newPMs) = mkServiceSpec(containerId, spec, namespace)
    maybeService match {
      case Some(svc) => kube.create[Service](svc) map { _ => newPMs }
      case      None => Future.successful(newPMs)
    }
  }

  /**
    * Create a Deployment with services in Kubernetes
    */
  private[services] def createDeploymentEtAl(kube: RequestContext, containerId: UUID, spec: ContainerSpec, namespace: String): Future[ContainerSpec] = {
    val fDeployment = kube.create[Deployment](mkDeploymentSpec(containerId, spec, namespace))
    val fUpdatedPMsFromService = createService(kube, namespace, containerId, spec) recover {
      case e: Throwable =>
        log.error(s"error creating Kubernetes Service for container ${containerId}; assuming that it was not created",e)
        spec.port_mappings
    }
    val fIngress = createIngress(kube, namespace, containerId, spec) recover {
      case e: Throwable =>
        log.error(s"error creating Kubernetes Ingress for container ${containerId}; assuming that it was not created",e)
        ()
    }
    for {
      dep <- fDeployment
      _ <- fIngress
      updatedPMs <- fUpdatedPMsFromService
    } yield spec.copy(
      port_mappings = updatedPMs
    )
  }

  /**
   * Update a Deployment in Kubernetes, updating/creating/deleting Services/Ingresses as needed
   */
  private[services] def updateDeploymentEtAl(kube: RequestContext, containerId: UUID, spec: ContainerSpec, namespace: String): Future[ContainerSpec] = {
    val fDepl = kube.update[Deployment](mkDeploymentSpec(containerId, spec, namespace))
    val fUpdatedPMsFromService = updateService(kube, namespace, containerId, spec) recover {
      case e: Throwable =>
        log.error(s"error creating Kubernetes Service for container ${containerId}; assuming that it was not created",e)
        spec.port_mappings
    }
    val fIngress = updateIngress(kube, namespace, containerId, spec) recover {
      case e: Throwable =>
        log.error(s"error updating Kubernetes Ingress for container ${containerId}; assuming that is in an invalid state",e)
        ()
    }
    for {
      dep <- fDepl
      _ <- fIngress
      updatedPMs <- fUpdatedPMsFromService
    } yield spec.copy(
      port_mappings = updatedPMs
    )
  }

  private[services] def createKubeSecret(kube: RequestContext, secretId: UUID, spec: ContainerSecret, namespace: String) = {
    kube.create[Secret](mkSecret(secretId, spec, namespace))
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
    * Lookup and return the external_id property
    */
  private def containerExternalId(container: GestaltResourceInstance): Option[String] = {
    for {
      props <- container.properties
      eid <- props.get("external_id")
    } yield eid
  }

  /**
   * Parse and format the provider.id property from a container.properties map.
   */
  private def providerIdProperty(ps: Map[String, String]): Option[UUID] = {
    Js.find(Json.parse(ps("provider")).as[JsObject], "/id") map { id =>
      UUID.fromString(id.as[String])
    }
  }



  def destroy(container: GestaltResourceInstance): Future[Unit] = {

    val provider = containerProvider(container)
    /*
     * TODO: Change signature of deleteContainer to take a ProviderContext - providers
     * must never user ResourceFactory directly.
     */
    val environment: String = {
      ResourceFactory.findParent(ResourceIds.Environment, container.id).map(_.id.toString) orElse {
        val namespaceGetter = "/namespaces/([^/]+)/deployments/.*".r
        for {
          eid <- containerExternalId(container)
          ns <- eid match {
            case namespaceGetter(namespace) => Some(namespace)
            case _ => None
          }
        } yield ns
      } getOrElse {
        throw new RuntimeException(s"Could not find Environment for container '${container.id}' in Meta.")
      }
    }

    val fKube = skuberFactory.initializeKube(provider.id, environment)

    val targetLabel = META_CONTAINER_KEY -> container.id.toString

    val fDplDel = for {
      kube        <- fKube
      deps <- deleteAllWithLabel[Deployment,DeploymentList](kube, targetLabel)
      rses <- deleteAllWithLabel[ReplicaSet,ReplicaSetList](kube, targetLabel)
      pods <- deleteAllWithLabel[Pod,PodList](kube, targetLabel)
    } yield pods.headOption.getOrElse({
      log.debug("deleted no Pods")
      ()
    })

    val fSrvDel = for {
      kube <- fKube
      svcs <- deleteAllWithLabel[Service,ServiceList](kube, targetLabel)
    } yield svcs.headOption.getOrElse({
      log.debug("deleted no Services")
      ()
    })

    val fIngDel = for {
      kube <- fKube
      ings <- deleteAllWithLabel[Ingress,IngressList](kube, targetLabel)
    } yield ings.headOption.getOrElse({
      log.debug("deleted no Ingresses")
      ()
    })

    Future.sequence(Seq(fDplDel,fSrvDel,fIngDel)) map {_ =>
      log.debug(s"finished deleting Deployments, ReplicaSets, Pods, Services and Ingresses for container ${container.id}")
      ()
    }
  }

  def listByName[O <: ObjectResource, L <: KList[O]](objs: L, prefix: String): List[O] = {
    objs filter { _.name.startsWith(prefix) }
  }

  def listByLabel[O <: ObjectResource, L <: KList[O]](objs: L, label: (String, String)): List[O] = {
    objs filter { _.metadata.labels.get(label._1).contains(label._2) }
  }

  def getByLabel[O <: ObjectResource, L <: KList[O]](objs: L, label: (String, String)): Option[O] = {
    objs find { _.metadata.labels.get(label._1).contains(label._2) }
  }

  def getByName[O <: ObjectResource, L <: KList[O]](objs: L, prefix: String): Option[O] = {
    val found = listByName[O, L](objs, prefix)
    if (found.size > 1) throw new IllegalArgumentException(s"Too many matches.")
    else found.headOption
  }

  def safeList[L <: KList[_]: TypeTag]( kube: RequestContext )(implicit fmt: Format[L], kind: ListKind[L]) : Future[L] = {
    // handle https://github.com/doriordan/skuber/issues/26
    kube.list[L]() recover {
      case e: Throwable =>
        log.debug(s"error listing Kubernetes resources: ${e.toString}, will assume empty list")
        // TODO: (cgbaker) this instantiation code did not work:
        //   typeTag[L].mirror.runtimeClass(typeOf[L]).newInstance().asInstanceOf[L]
        // I don't feel like investigating it right now, so hack for now
        typeOf[L] match {
          case t if t =:= typeOf[PodList] => PodList().asInstanceOf[L]
          case t if t =:= typeOf[DeploymentList] => DeploymentList().asInstanceOf[L]
          case t if t =:= typeOf[ServiceList] => ServiceList().asInstanceOf[L]
          case t if t =:= typeOf[IngressList] => IngressList().asInstanceOf[L]
        }

    }
  }

  def findByLabel[O <: ObjectResource : TypeTag, L <: KList[O] : TypeTag](kube: RequestContext, lbl: (String,String) )(implicit fmt: Format[L], kind: ListKind[L]) : Future[Option[O]] = {
    val otype = typeOf[O]
    safeList[L](kube) map {
      listByLabel[O,L](_, lbl) match {
        case Nil          => None
        case head :: tail =>
          if (tail.nonEmpty) log.warn(s"found multiple ${otype} resources tagged, will use the first one")
          Some(head)
      }
    }
  }

  // TODO (cgbaker): this is pretty unsavory... gotta be a way to get O out of L without having to explicitly template on it
  def deleteAllWithLabel[O <: ObjectResource : TypeTag, L <: skuber.KList[O] : TypeTag]( kube: RequestContext, label: (String, String) )
                                                                   ( implicit fmtL: Format[L], kindL: ListKind[L],
                                                                     fmtO: Format[O], kindO: ObjKind[O] ) : Future[List[Unit]] = {
    val otype = typeOf[O]
    for {
      allResources <- safeList[L](kube)
      _ = log.debug(s"found ${allResources.size} ${otype}")
      selectedResources = listByLabel[O,L](allResources, label)
      _ = log.debug(s"identified ${selectedResources.size} ${otype} to delete")
      deletes <- Future.traverse(selectedResources){
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

    withInputDefaults(org, containerResourceInput, user, None)
  }

  private[services] def mkSecret(id: UUID, secret: ContainerSecret, namespace: String = DefaultNamespace) = {
    val metadata = ObjectMeta(name = secret.name, namespace = namespace)
    val bytes = secret.data map { case (k,v) => (k, v.getBytes(Ascii.DEFAULT_CHARSET)) }
    Secret(metadata = metadata, data = bytes, `type` = secret.secret_type)
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

  private[services] def mkIngressSpec(containerId: UUID, containerSpec: ContainerSpec, namespace: String = DefaultNamespace): Option[Ingress] = {
    val svcName = containerSpec.name

    val ingressPMs = for {
      pm <- containerSpec.port_mappings
      vhost <- pm.virtual_hosts.getOrElse(Seq.empty) if pm.expose_endpoint.contains(true) && pm.service_port.orElse(pm.container_port).isDefined
    } yield (vhost,pm.service_port.getOrElse(pm.container_port.get))

    if (ingressPMs.isEmpty) None
    else Some(ingressPMs.foldLeft[Ingress](
      Ingress(metadata = ObjectMeta(
        name = svcName,
        namespace = namespace,
        labels = Map(META_CONTAINER_KEY -> containerId.toString)
      ))
    ){
      case (acc, (vhost,port)) => acc.addHttpRule(
        vhost, Map("" -> s"$svcName:$port")
      )
    })
  }

  private[services] def mkServiceSpec(containerId: UUID, containerSpec: ContainerSpec, namespace: String = DefaultNamespace): (Option[Service],Seq[PortMapping]) = {
    val svcName = containerSpec.name
    val exposedPorts = containerSpec.port_mappings.filter(_.expose_endpoint.contains(true))
    if (exposedPorts.isEmpty) {
      (None, containerSpec.port_mappings.map(_.copy(service_address = None)))
    } else {
      val srv = exposedPorts.foldLeft[Service](
        Service(metadata = ObjectMeta(name = svcName, namespace = namespace))
          .withType(Service.Type.NodePort)
          .withSelector(
            META_CONTAINER_KEY -> containerId.toString
          )
          .addLabel(
            META_CONTAINER_KEY -> containerId.toString
          )
      ) {
        case (svc, pm) => svc.exposeOnPort(Service.Port(
          name = pm.name.getOrElse(""),
          protocol = pm.protocol,
          port = pm.service_port.orElse(pm.container_port).getOrElse(unprocessable("port mapping must contain container_port ")),
          targetPort = Some(pm.container_port.getOrElse(unprocessable("port mapping must contain container_port"))),
          nodePort = pm.host_port.getOrElse(0)
        ))
      }
      val svchost = s"${svcName}.${namespace}.svc.cluster.local"
      val newPMs = containerSpec.port_mappings.map {
        case pm if pm.expose_endpoint.contains(true) => pm.copy(
          service_address = Some(ContainerSpec.ServiceAddress(
            host = svchost,
            port = pm.service_port.orElse(pm.container_port).get,
            protocol = Some(pm.protocol)
          ))
        )
        case pm => pm.copy(service_address = None)
      }
      (Some(srv),newPMs)
    }
  }


  /**
   * Create a Kubernetes Deployment object in memory.
   *
   * @param id UUID for the Meta Container. This will be used as a label on all of the created resourced for later indexing.
   * @param containerSpec ContainerSpec with Container data
   */
  private[services] def mkDeploymentSpec(id: UUID, containerSpec: ContainerSpec, namespace: String = DefaultNamespace): Deployment = {

    val labels = containerSpec.labels ++ Map(META_CONTAINER_KEY -> id.toString)

    val requirements = skuber.Resource.Requirements(requests = Map(
        skuber.Resource.cpu    -> containerSpec.cpus,
        skuber.Resource.memory -> containerSpec.memory
    ))

    val cmdArray = containerSpec.cmd map CommandParser.translate

    val container = skuber.Container(
      name = containerSpec.name,
      image = containerSpec.image,
      resources = Some(requirements),
      env = List(
        skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
      ) ++ mkEnvVars(containerSpec.env),
      ports = mkPortMappingsSpec(containerSpec.port_mappings).toList,
      imagePullPolicy = if (containerSpec.force_pull) Container.PullPolicy.Always else Container.PullPolicy.IfNotPresent,
      args = containerSpec.args.getOrElse(Seq.empty).toList,
      command = cmdArray.getOrElse(Nil)
    )

    val podTemplate = Pod.Template.Spec(
      spec = Some(Pod.Spec()
          .addContainer(container)
          .withDnsPolicy(skuber.DNSPolicy.ClusterFirst))
    ).addLabels( labels )

    Deployment(metadata = ObjectMeta(
      name = containerSpec.name,
      namespace = namespace,
      labels = labels
    )).withTemplate(podTemplate)
      .withReplicas(containerSpec.num_instances)
  }

  private[services] def mkEnvVars(env: Map[String,String]): List[EnvVar] = {
    env.map { case (k,v) => EnvVar(k, EnvVar.StringValue(v)) }.toList
  }

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
    val lbl = META_CONTAINER_KEY -> container.id.toString
    for {
      kube <- skuberFactory.initializeKube(context.providerId, context.environment.id.toString)
      maybeDepl <- findByLabel[Deployment,DeploymentList](kube, lbl)
      pods <- maybeDepl match {
        case None    => Future.successful(PodList())
        case Some(_) => safeList[PodList](kube)
      }
      maybeSvc <- maybeDepl match {
        case None    => Future.successful(None)
        case Some(_) => findByLabel[Service,ServiceList](kube, lbl)
      }
      thesePods = listByLabel[Pod,PodList](pods, lbl)
      update = maybeDepl map (kubeDeplAndPodsToContainerStatus(_, thesePods, maybeSvc))
    } yield update
  }

  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    for {
      kube <- skuberFactory.initializeKube(context.providerId, context.environment.id.toString)
      fDepls = safeList[DeploymentList](kube)
      fAllPods = safeList[PodList](kube)
      fAllServices = safeList[ServiceList](kube)
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
    val numTarget  = depl.spec.map(_.replicas).getOrElse(numRunning) // TODO: don't really know what to do if this doesn't exist
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
    val hostPorts = (for {
      svc <- service
      svcSpec <- svc.spec
      svcPorts = svcSpec.ports
      dspec <- depl.spec
      dtemp <- dspec.template
      pspec <- dtemp.spec
      cspec <- pspec.containers.headOption
      hostPorts = cspec.ports map {
        cp => svcPorts.find {
          sp => sp.name == cp.name || sp.targetPort.flatMap(_.left.toOption).contains(cp.containerPort)
        } map {_.nodePort} getOrElse 0
      }
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

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = {
    for {
      kube       <- skuberFactory.initializeKube(context.provider.id, context.environmentId.toString)
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
      updatedNumInstances = updatedDepl.spec.map(_.replicas).getOrElse(
        throw new RuntimeException(s"updated Deployment for container ${container.id} did not have a spec, so that replica size could not be determined")
      )
    } yield upsertProperties(
      container,
      "num_instances" -> s"${updatedNumInstances}"
    )
  }

}


