package services.kubernetes

import java.util.{TimeZone, UUID}

import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.meta.api.ContainerSpec.PortMapping
import com.galacticfog.gestalt.meta.api.ContainerStats.{ContainerStateStat, EventStat}
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.{ContainerSpec, ContainerStats, SecretSpec, VolumeSpec}
import com.galacticfog.gestalt.util.FutureFromTryST._
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.google.inject.Inject
import controllers.util._
import org.joda.time.{DateTime, DateTimeZone}
import cats.syntax.either._
import monocle.macros.GenLens
import play.api.Logger
import play.api.libs.json._
import services.{ProviderContext,CaasService}
import skuber.LabelSelector.dsl._
import skuber._
import skuber.api.client._
import skuber.ext._
import skuber.rbac._
import skuber.json.ext.format._
import skuber.json.rbac.format._
import skuber.json.format._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class KubernetesService @Inject() ( skuberFactory: SkuberFactory )
  extends CaasService with JsonInput with MetaControllerUtils {
  import scala.concurrent.ExecutionContext.Implicits.global
  
  import KubernetesConstants._

  val mks = new MkKubernetesSpec {}

  override private[this] val log = Logger(this.getClass)

  private[services] val DefaultNamespace = "default"

  def cleanly[T](provider: GestaltResourceInstance, namespace: String)(f: RequestContext => Future[T]): Future[T] = {
    skuberFactory.initializeKube(provider, namespace) flatMap { kube =>
      val fT = f(kube)
      fT.onComplete(_ => kube.close)
      fT
    }
  }

  private def getNamespaceForResource(resource: ResourceLike, resourceName: String): String = {
    // why not attempt to take the namespace from external_id first?

    val namespaceGetter = s"/namespaces/([^/]+)/${resourceName}/.*".r
    (for {
      eid <- ContainerService.resourceExternalId(resource)
      ns <- eid match {
        case namespaceGetter(namespace) => Some(namespace)
        case _ => None
      }
    } yield ns) orElse {
      ResourceFactory.findParent(ResourceIds.Environment, resource.id).map(_.id.toString)
    } getOrElse {
      throw new RuntimeException(s"Failed to obtain namespace for '${resource.id}' in Kubernetes: invalid external_id and missing parent environment.")
    }
  }

  def createSecret(context: ProviderContext, secret: GestaltResourceInstance, items: Seq[SecretSpec.Item])
                  (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {

    def createKubeSecret(kube: RequestContext, secretId: UUID, spec: SecretSpec, namespace: String, context: ProviderContext): Future[Secret] = {
      kube.create[Secret](mks.mkSecret(secretId, spec, namespace, context)) recoverWith { case e: K8SException =>
        Future.failed(new RuntimeException(s"Failed creating Secret '${spec.name}': " + e.status.message))
      }
    }

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
    // val environment: String = {
    //   ResourceFactory.findParent(ResourceIds.Environment, secret.id).map(_.id.toString) orElse {
    //     val namespaceGetter = "/namespaces/([^/]+)/secrets/.*".r
    //     for {
    //       eid <- ContainerService.resourceExternalId(secret)
    //       ns <- eid match {
    //         case namespaceGetter(namespace) => Some(namespace)
    //         case _ => None
    //       }
    //     } yield ns
    //   } getOrElse {
    //     throw new RuntimeException(s"Could not find Environment for secret '${secret.id}' in Meta.")
    //   }
    // }

    val namespace = getNamespaceForResource(secret, "secrets")

    val targetLabel = META_SECRET_KEY -> secret.id.toString
    cleanly(provider, namespace) { kube =>
      val fSecretDel = for {
        deps <- deleteAllWithLabel[Secret](kube, targetLabel)
      } yield deps.headOption.getOrElse({
        log.debug(s"$namespace: deleted no Secrets")
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
    // val namespaceGetter = "/namespaces/([^/]+)/.*/.*".r

    // val namespaces = (ResourceFactory.findChildren(pc.environmentId).filter(
    //   r => Set(ResourceIds.Container, ResourceIds.Secret, migrations.V13.VOLUME_TYPE_ID).contains(r.typeId) &&
    //     Try{ContainerService.containerProviderId(r)}.toOption.contains(pc.providerId)
    // ).flatMap(ContainerService.resourceExternalId).collect {
    //   case namespaceGetter(namespace) => namespace
    // }).distinct

    // val targetNamespace = namespaces match {
    //   case Seq() => pc.environmentId.toString
    //   case Seq(single) => single
    //   case _ => {
    //     log.error(s"Environment '${pc.environmentId}' contains resources from multiple Kubernetes namespaces; new resources cannot be created until this is resolved.")
    //     throw new BadRequestException(s"Environment '${pc.environmentId}' contains resources from multiple Kubernetes namespaces; new resources cannot be created until this is resolved.")
    //   }
    // }

    for(
      targetNamespace <- controllers.Environment.getDefaultNamespace(pc.environmentId, pc.providerId).liftTo[Future];
      namespaceOpt <- rc.getOption[Namespace](targetNamespace);
      namespace <- namespaceOpt match {
        case Some(s) => {
          log.debug(s"Found Kubernetes namespace: ${s.name}")
          Future.successful(s)
        }
        case None if create => {
          log.debug(s"Creating new Kubernetes namespace: ${targetNamespace}")
          for(
            namespace <- rc.create(Namespace(metadata = ObjectMeta(
              name = targetNamespace,
              labels = mks.mkLabels(pc)
            )));
            _ <- rc.create(new ClusterRoleBinding(
              metadata = ObjectMeta(
                name = s"${targetNamespace}-cluster-admin",
                labels = mks.mkLabels(pc)
              ),
              roleRef = Some(new RoleRef("rbac.authorization.k8s.io", "ClusterRole", "cluster-admin")),
              subjects = List(new Subject(None, "ServiceAccount", "default", Some(targetNamespace)))
              // the namespace param is an Option, but leaving it off in ClusterRoleBinding causes an exception – at least if created via kubectl
            ))
          ) yield namespace
        }
        case None if !create => {
          log.error(s"No namespace found for environment '${pc.environmentId}' - create == false")
          Future.failed(UnprocessableEntityException(s"There is no Namespace corresponding with Environment '${pc.environmentId}', environment"))
        }
      }
    ) yield namespace

    // targetNamespace flatMap {
    //   case Some(s) =>
    //     log.debug(s"Found Kubernetes namespace: ${s.name}")
    //     Future.successful(s)
    //   case None if create =>
    //     log.debug(s"Creating new Kubernetes namespace: ${targetNamespace}")
    //     for(
    //       namespace <- rc.create(Namespace(metadata = ObjectMeta(
    //         name = targetNamespace,
    //         labels = mkLabels(pc)
    //       )));
    //       _ <- rc.create(new ClusterRoleBinding(
    //         metadata = ObjectMeta(
    //           name = s"${targetNamespace}-cluster-admin",
    //           labels = mkLabels(pc)
    //         ),
    //         roleRef = Some(new RoleRef("rbac.authorization.k8s.io", "ClusterRole", "cluster-admin")),
    //         subjects = List(new Subject(None, "ServiceAccount", "default", Some(targetNamespace)))
    //         // the namespace param is an Option, but leaving it off in ClusterRoleBinding causes an exception – at least if created via kubectl
    //       ))
    //     ) yield namespace
    //   case None if !create =>
    //     log.error(s"No namespace found for environment '${pc.environmentId}' - create == false")
    //     Future.failed(UnprocessableEntityException(s"There is no Namespace corresponding with Environment '${pc.environmentId}', environment"))
    // }
  }

  // private def setPostLaunchStatus(container: GestaltResourceInstance): GestaltResourceInstance = {
  //   upsertProperties(container, "status" -> "LAUNCHED")
  // }

  def updateIngress(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec, context: ProviderContext): Future[Option[Ingress]] = {
    // start this async call early, we'll need it later
    val fExistingIngress = kube.getOption[Ingress](spec.name)
    /*
     * create new ingress spec
     * if empty, delete any existing ingress
     * if not empty, update or create
     */
    val maybeNewIng = mks.mkIngressSpec(containerId, spec, namespace, context)
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

  def createIngress(kube: RequestContext, namespace: String, containerId: UUID, spec: ContainerSpec, context: ProviderContext): Future[Option[Ingress]] = {
    mks.mkIngressSpec(containerId, spec, namespace, context).fold { Future.successful(Option.empty[Ingress]) }
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
    val newServices = mks.mkServiceSpecs(containerId, spec, namespace, context)
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
    val svcs = mks.mkServiceSpecs(containerId, spec, namespace, context)
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
    val fDeployment = kube.create[Deployment](mks.mkDeploymentSpec(containerId, spec, context, namespace))
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
    val fDepl = kube.update[Deployment](mks.mkDeploymentSpec(containerId, spec, context, namespace))
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

  def destroy(container: ResourceLike): Future[Unit] = {
    log.debug("destroy(_)")

    val provider = ContainerService.containerProvider(container)
    /*
     * TODO: Change signature of deleteContainer to take a ProviderContext - providers
     * must never use ResourceFactory directly.
     */
    // val environment: String = {
    //   ResourceFactory.findParent(ResourceIds.Environment, container.id).map(_.id.toString) orElse {
    //     val namespaceGetter = "/namespaces/([^/]+)/deployments/.*".r
    //     for {
    //       eid <- ContainerService.resourceExternalId(container)
    //       ns <- eid match {
    //         case namespaceGetter(namespace) => Some(namespace)
    //         case _ => None
    //       }
    //     } yield ns
    //   } getOrElse {
    //     throw new RuntimeException(s"Could not find Environment for container '${container.id}' in Meta.")
    //   }
    // }

    val namespace = getNamespaceForResource(container, "deployments")

    val targetLabel = META_CONTAINER_KEY -> container.id.toString
    cleanly(provider, namespace) { kube =>
      
      // val fDplDel = for {
      //   deps <- deleteAllWithLabel[Deployment](kube, targetLabel)
      //   rses <- deleteAllWithLabel[ReplicaSet](kube, targetLabel)
      //   pods <- deleteAllWithLabel[Pod](kube, targetLabel) recover {
      //     case e: K8SException =>
      //       log.warn(s"K8S error listing/deleting Pods associated with container ${container.id}")
      //       List(())
      //   }
      // } yield pods.headOption.getOrElse({
      //   log.debug(s"$namespace: deleted no Pods")
      //   ()
      // })

      val fDplDel = for(
        deployments <- kube.listSelected[ListResource[Deployment]](targetLabel._1 is targetLabel._2);
        _ = log.debug(s"deployments: ${deployments}");
        selectors = deployments map { depl =>
          depl.spec.flatMap(_.selector).get    // fair to think that every deployment has a selector
        };
        rss <- Future.traverse(selectors) { selector =>
          kube.listSelected[ListResource[ReplicaSet]](selector).map(_.items)
        };
        pods <- (Future.traverse(selectors) { selector =>
          kube.listSelected[PodList](selector).map(_.items)
        }) recover {
          case e: K8SException => {
            log.warn(s"K8S error listing/deleting Pods associated with container ${container.id}")
            List()
          }
        };
        _ <- Future.traverse(deployments.items) { depl =>
          kube.delete[Deployment](depl.name)
        };
        _ <- Future.traverse(rss.flatten) { replicaSet =>
          kube.delete[ReplicaSet](replicaSet.name)
        };
        _ <- Future.traverse(pods.flatten) { pod =>
          kube.delete[Pod](pod.name)
        }
      ) yield {
        if(pods.isEmpty) {
          log.debug(s"$namespace: deleted no Pods")
        }
        ()
      }

      val fSrvDel = (for {
        svcs <- deleteAllWithLabel[Service](kube, targetLabel)
      } yield svcs.headOption.getOrElse({
        log.debug(s"$namespace: deleted no Services")
        ()
      })) recover {
        case e: K8SException =>
          log.warn(s"K8S error listing/deleting Services associated with container ${container.id}")
          ()
      }

      val fIngDel = (for {
        ings <- deleteAllWithLabel[Ingress](kube, targetLabel)
      } yield ings.headOption.getOrElse({
        log.debug(s"$namespace: deleted no Ingresses")
        ()
      })) recover {
        case e: K8SException =>
          log.warn(s"K8S error listing/deleting Ingresses associated with container ${container.id}")
          ()
      }

      // destroy() signature ought to be changed
      val mounts = ContainerSpec.fromResourceInstance(container.asInstanceOf[GestaltResourceInstance]) map { spec =>
        spec.volumes
      } recoverWith { case e: Throwable =>
        e.printStackTrace()
        log.warn(s"Failed to deserialize container ${container.id}: ${e.getMessage}")
        Failure(e)
      } getOrElse {
        Seq()
      }

      val fVolumeResourceDel = Future.traverse(mounts) { 
        case ContainerSpec.ExistingVolumeMountSpec(_, volumeId) => {
          kube.listSelected[ListResource[PersistentVolumeClaim]](META_VOLUME_KEY is volumeId.toString) flatMap { resources =>
            if(resources.items.size == 0) {
              Future.fromTryST(ResourceFactory.hardDeleteResource(volumeId))
            }else {
              Future.successful(())
            }
          }
        }
        case _ => Future.successful(())
      }

      for(
        _ <- fDplDel;
        _ <- fSrvDel;
        _ <- fIngDel
      ) yield {
        log.debug(s"finished deleting Deployments, ReplicaSets, Pods, Services and Ingresses for container ${container.id}")
      }
    }
  }

  def deleteAllWithLabel[O <: ObjectResource]( kube: RequestContext, label: (String, String) )
                                                       ( implicit fmtL: Format[ListResource[O]], fmtO: Format[O],
                                                                  rdo: skuber.ResourceDefinition[O],
                                                                  rd: skuber.ResourceDefinition[ListResource[O]] ) : Future[List[Unit]] = {
    for {
      foundResources <- kube.listSelected[ListResource[O]](label._1 is label._2)
      _ = log.debug(s"found ${foundResources.items.size} ${rdo.spec.names.plural} labeled with ${label}")
      deletes <- Future.traverse(foundResources.items){
        d =>
          log.info(s"deleting ${rdo.spec.names.singular} ${d.name} labeled with ${label}")
          kube.delete[O](d.name)
      }
    } yield deletes
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
              (owner.kind == "Deployment") && owner.uid == maybeDeployment.get.metadata.uid))

            pods = allPods.filter(_.metadata.ownerReferences.exists(owner =>
              (owner.kind == "ReplicaSet") && replicaSets.exists(_.metadata.uid == owner.uid)))

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
            stats <- Future.traverse(deplsWithSelector) { depl =>
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

    val podEventStats = events filter (podEvent => (podEvent.involvedObject.kind == "Pod") && podNames.contains(podEvent.involvedObject.name)) map eventStatFromPodEvent

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
        objectType = "Pod",
        eventType = podEvent.`type`.getOrElse(""),
        reason = podEvent.reason.getOrElse(""),
        sourceComponent = eventSourceComponent.getOrElse(""),
        sourceHost = eventSourceOption.getOrElse(""),
        message = podEvent.message.getOrElse(""),
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
            objectType = "Pod",
            stateId = terminated.id,
            reason = terminated.reason,
            message = terminated.message,
            finishedAt = terminationDateTime,
            priority = 3
          )
        }
        case waiting: skuber.Container.Waiting => ContainerStateStat(
          objectName = pod.name,
          objectType = "Pod",
          stateId = waiting.id,
          reason = waiting.reason,
          priority = 2
        )
        case running: skuber.Container.Running => ContainerStateStat(
          objectName = pod.name,
          objectType = "Pod",
          stateId = running.id
        )
          //should never happen
        case _ => ContainerStateStat(
          objectName = pod.name,
          objectType = "Pod"
        )
      }
    })
  }

  def containerStateStatFromPodConditions(pod: skuber.Pod) : Option[ContainerStateStat] = {
    pod.status flatMap (podStatus => {
      podStatus.conditions.find(_.status == "False").map(condition => {
        ContainerStateStat(
          objectName = pod.name,
          objectType = "Pod",
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

  override def createVolume(context: ProviderContext, metaResource: Instance)
                           (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {

    import monocle.std.option._

    val pvcLabelsOptics = GenLens[skuber.PersistentVolumeClaim](_.metadata.labels)
    val pvLabelsOptics = GenLens[skuber.PersistentVolume](_.metadata.labels)

    val pvClaimRefNamespaceOptics = GenLens[skuber.PersistentVolume](_.spec) composePrism some composeLens
     GenLens[skuber.PersistentVolume.Spec](_.claimRef) composePrism some composeLens
     GenLens[skuber.ObjectReference](_.namespace)
    val pvcNamespaceOptics = GenLens[skuber.PersistentVolumeClaim](_.metadata.namespace)
    val pvSourceOptics = GenLens[skuber.PersistentVolume](_.spec) composePrism some composeLens
     GenLens[skuber.PersistentVolume.Spec](_.source)

    for(
      pvcpvSpecs <- mks.mkPVCandPV(context.provider, metaResource).liftTo[Future];
      pvcSpec = pvcLabelsOptics.modify(_ ++ mks.mkLabels(context))(pvcpvSpecs._1);
      pvSpecOpt = (some composeLens pvLabelsOptics).modify(_ ++ mks.mkLabels(context))(pvcpvSpecs._2);
      namespace <- cleanly(context.provider, DefaultNamespace) { kube =>
        getNamespace(kube, context, create = true)
      };
      pvc <- cleanly(context.provider, namespace.name) { kube =>
        for(
          _ <- pvSpecOpt map { pvSpec =>
            kube.create[PersistentVolume](pvClaimRefNamespaceOptics.set(namespace.name)(pvSpec)) recoverWith { case e: K8SException =>
              e.printStackTrace()
              Future.failed(new RuntimeException(s"Failed creating PersistentVolume for volume '${metaResource.name}': " + e.status.message))
            }
          } getOrElse(Future.successful(()));
          pvc <- kube.create[PersistentVolumeClaim](pvcNamespaceOptics.set(namespace.name)(pvcSpec)) recoverWith { case e: K8SException =>
            e.printStackTrace()
            Future.failed(new RuntimeException(s"Failed creating PersistentVolumeClaim for volume '${metaResource.name}': " + e.status.message))
          }
        ) yield pvc
      }
    ) yield {
      val externalId = Map(
        "external_id" -> s"/namespaces/${pvc.namespace}/persistentvolumeclaims/${pvc.name}"
      )
      val reclamationPolicy = (some composeOptional pvSourceOptics).getOption(pvSpecOpt) match {
        case Some(_: skuber.Volume.HostPath) | Some(_: skuber.Volume.GenericVolumeSource) => Map("reclamation_policy" -> "delete_persistent_volume")
        case _ => Map()
      }
      metaResource.copy(properties = Some((metaResource.properties getOrElse Map()) ++ externalId ++ reclamationPolicy))
    }
  }

  override def destroyVolume(volume: GestaltResourceInstance): Future[Unit] = {
    def doTheDelete: Future[Unit] = {
      val provider = ContainerService.containerProvider(volume)
      /*
       * TODO: Change signature of deleteVolume to take a ProviderContext - providers
       * must never user ResourceFactory directly.
       */
      // val environment: String = {
      //   ResourceFactory.findParent(ResourceIds.Environment, volume.id).map(_.id.toString) orElse {
      //     val namespaceGetter = "/namespaces/([^/]+)/persistentvolumeclaims/.*".r
      //     for {
      //       eid <- ContainerService.resourceExternalId(volume)
      //       ns <- eid match {
      //         case namespaceGetter(namespace) => Some(namespace)
      //         case _ => None
      //       }
      //     } yield ns
      //   } getOrElse {
      //     throw new RuntimeException(s"Could not find Environment for volume '${volume.id}' in Meta.")
      //   }
      // }

      val namespace = getNamespaceForResource(volume, "persistentvolumeclaims")

      val deletePVs = volume.properties.getOrElse(Map.empty).get("reclamation_policy").contains("delete_persistent_volume")

      val targetLabel = META_VOLUME_KEY -> volume.id.toString
      cleanly(provider, namespace) { kube =>
        val fVolumeDels = for {
          pvcs <- deleteAllWithLabel[PersistentVolumeClaim](kube, targetLabel)
          _    <- if (deletePVs) deleteAllWithLabel[PersistentVolume](kube, targetLabel) else Future.successful(Seq.empty)
        } yield pvcs.headOption.getOrElse({
          log.debug(s"$namespace: deleted no PersistentVolumeClaims")
          ()
        })

        fVolumeDels map {_ =>
          log.debug(s"finished deleting PersistentVolumes and PersistentVolumeClaims for volume ${volume.id}")
          ()
        }
      }
    }

    for {
      specType <- Future.fromTryST(VolumeSpec.fromResourceInstance(volume)) map { spec =>
        Some(spec.`type`)
      } recoverWith { case throwable =>
        log.warn(s"Failed to deserialize volume spec due to ${throwable}, proceeding")
        Future.successful(None)
      }
      d <- specType match {
        case Some(VolumeSpec.External | VolumeSpec.Dynamic | VolumeSpec.HostPath) =>
          doTheDelete
        case Some(other) =>
          log.info(s"destroyVolume(): nothing to do for type ${other}")
          Future.successful(())
        case None => Future.successful(())
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

