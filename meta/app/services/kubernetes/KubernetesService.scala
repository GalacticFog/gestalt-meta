package services.kubernetes

import java.util.{TimeZone, UUID}

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.meta.api.ContainerStats.{ContainerStateStat, EventStat}
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.{ContainerSpec, ContainerStats, SecretSpec, VolumeSpec}
import com.galacticfog.gestalt.util.FutureFromTryST._
import com.galacticfog.gestalt.util.EitherWithErrors.{futureStringApplicativeError => _, _}
import com.galacticfog.gestalt.util.Error
import com.galacticfog.gestalt.util.ResourceSerde
import com.google.inject.Inject
import controllers.util._
import org.joda.time.{DateTime, DateTimeZone}
import cats.syntax.either._
import cats.syntax.traverse._
import monocle.Traversal
import monocle.macros.GenLens
import monocle.macros.GenPrism
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
import scala.util.Failure

class KubernetesService @Inject() ( skuberFactory: SkuberFactory )
  extends CaasService with JsonInput with MetaControllerUtils {
  import scala.concurrent.ExecutionContext.Implicits.global
  
  import KubernetesConstants._

  val keb = new KubernetesEntityBuilder {}

  override private[this] val log = Logger(this.getClass)

  private[services] val DefaultNamespace = "default"

  private def cleanly[T](provider: GestaltResourceInstance, namespace: String)(f: RequestContext => Future[T]): Future[T] = {
    skuberFactory.initializeKube(provider, namespace) flatMap { kube =>
      val fT = f(kube)
      fT.onComplete(_ => kube.close)
      fT
    }
  }

  private def mkLabels(context: ProviderContext): Map[String,String] = {
    Map(
      META_ENVIRONMENT_KEY -> context.environmentId.toString,
      META_WORKSPACE_KEY -> context.workspace.id.toString,
      META_FQON_KEY -> context.fqon,
      META_PROVIDER_KEY -> context.providerId.toString
    )
  }

  private def getNamespaceForResource[T <: skuber.TypeMeta](resource: GestaltResourceInstance)(implicit rd: skuber.ResourceDefinition[T]): String = {
    val namespaceGetter = s"/namespaces/([^/]+)/${rd.spec.names.plural}/.*".r
    val externalId = ContainerService.resourceExternalId(resource)
    (for {
      eid <- externalId
      ns <- eid match {
        case namespaceGetter(namespace) => Some(namespace)
        case _ => None
      }
    } yield ns) orElse {
      log.warn(s"Failed to extract namespace from resource ${resource.id} with external_id=${externalId}; this is not meant to occur")
      ResourceFactory.findParent(ResourceIds.Environment, resource.id).map(_.id.toString)
    } getOrElse {
      throw new RuntimeException(s"Failed to obtain namespace for '${resource.id}' in Kubernetes: invalid external_id and missing parent environment.")
    }
  }

  def createSecret(context: ProviderContext, secret: GestaltResourceInstance, items: Seq[SecretSpec.Item])(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    import KubernetesProviderProperties.Implicits._

    // this will be gone sooner or later
    val secretWithName = secret.copy(
      properties=secret.properties map { props =>
        props ++ Map("name" -> secret.name)
      }
    )

    for(
      spec0 <- ResourceSerde.deserialize[SecretSpec,Error.UnprocessableEntity](secretWithName).liftTo[Future];
      spec = spec0.copy(items=items);
      providerProperties <- ResourceSerde.deserialize[KubernetesProviderProperties.Properties,Error.UnprocessableEntity](context.provider).liftTo[Future];
      namespace <- cleanly(context.provider, DefaultNamespace)( getNamespace(_, context, create = true) );
      k8sSecret0 <- keb.mkSecret(providerProperties, spec).liftTo[Future];
      k8sSecret = k8sSecret0.copy(
        metadata = skuber.ObjectMeta(
          name = secret.name,
          namespace = namespace.name,
          labels = Map(
            META_SECRET_KEY -> s"${secret.id}"
          ) ++ mkLabels(context)
        )
      );
      created <- cleanly(context.provider, namespace.name) { kube =>
        kube.create[Secret](k8sSecret) recoverWith { case e: K8SException =>
          e.printStackTrace()
          Future.failed(new RuntimeException(s"Failed creating Secret '${spec.name}': " + e.status.message))
        }
      }
    ) yield {
      val prunedItems = spec.items.map(_.copy(value=None))
      val extraProps = Map(
        "external_id" -> s"/namespaces/${created.namespace}/secrets/${created.name}",
        "status" -> "LAUNCHED",
        "items" -> Json.stringify(Json.toJson(prunedItems))
      )
      secret.copy(properties = Some((secret.properties getOrElse Map()) ++ extraProps))
    }
  }

  def destroySecret(secret: GestaltResourceInstance): Future[Unit] = {
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

    val namespace = getNamespaceForResource[skuber.Secret](secret)

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

  def create(context: ProviderContext, container: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    log.debug("create(...)")
    createDeployment(context, container)
  }

  def update(context: ProviderContext, container: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    log.debug("update(...)")
    updateDeployment(context, container)
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
              labels = mkLabels(pc)
            )));
            _ <- rc.create(new ClusterRoleBinding(
              metadata = ObjectMeta(
                name = s"${targetNamespace}-cluster-admin",
                labels = mkLabels(pc)
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

  def destroy(container: GestaltResourceInstance): Future[Unit] = {
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

    val namespace = getNamespaceForResource[skuber.ext.Deployment](container)

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

  private def deleteAllWithLabel[O <: ObjectResource]( kube: RequestContext, label: (String, String) )
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

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
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

  def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
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

  private def skuberTimestampToJodaDateTime(timestamp: skuber.Timestamp): DateTime = {
    new DateTime(
      timestamp.toInstant().toEpochMilli(),
      DateTimeZone.forTimeZone(TimeZone.getTimeZone(timestamp.getZone())))
  }

  private[this] def kubeDeplAndPodsToContainerStatus(depl: Deployment, pods: List[Pod], maybeLbSvc: Option[Service], events : List[Event] = Nil): ContainerStats = {
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

  private def eventStatFromPodEvent(podEvent: skuber.Event): EventStat = {
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

  private def containerStateStatFromPodContainerStates(pod: skuber.Pod) : Option[ContainerStateStat] = {
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

  private def containerStateStatFromPodConditions(pod: skuber.Pod) : Option[ContainerStateStat] = {
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

  def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = {
    // shouldn't this attempt to take namespace from external_id ?
    val namespace = getNamespaceForResource[skuber.ext.Deployment](container)

    cleanly(context.provider, namespace) { kube =>
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
  }

  def createVolume(context: ProviderContext, metaResource: Instance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    
    import KubernetesProviderProperties.Implicits._
    import monocle.std.option._
    import cats.instances.option._

    val pvClaimRefNamespaceOptics = GenLens[skuber.PersistentVolume.Spec](_.claimRef) composePrism some composeLens
     GenLens[skuber.ObjectReference](_.namespace)
    val pvSourceOptics = GenLens[skuber.PersistentVolume](_.spec) composePrism some composeLens
     GenLens[skuber.PersistentVolume.Spec](_.source)

    for(
      spec0 <- ResourceSerde.deserialize[VolumeSpec,Error.UnprocessableEntity](metaResource).liftTo[Future];
      spec = spec0.copy(name=metaResource.name);
      providerProperties <- ResourceSerde.deserialize[KubernetesProviderProperties.Properties,Error.UnprocessableEntity](context.provider).liftTo[Future];
      pvcSpec <- keb.mkPvcSpec(providerProperties, spec).liftTo[Future];
      pvSpecOpt <- keb.mkPvSpec(providerProperties, spec).liftTo[Future];
      labels = Map(
        META_VOLUME_KEY -> s"${metaResource.id}"
      ) ++ mkLabels(context);
      namespace <- cleanly(context.provider, DefaultNamespace) { kube =>
        getNamespace(kube, context, create = true)
      };
      pvc = skuber.PersistentVolumeClaim(
        metadata = skuber.ObjectMeta(
          name = spec.name,
          namespace = namespace.name,
          labels = labels
        ),
        spec = Some(pvcSpec)
      );
      pvOpt = pvSpecOpt map { pvSpec =>
        skuber.PersistentVolume(
          metadata = skuber.ObjectMeta(
            name = s"${spec.name.take(8)}-${metaResource.id}",
            namespace = namespace.name,
            labels = labels
          ),
          spec = Some(pvClaimRefNamespaceOptics.set(namespace.name)(pvSpec))
        )
      };
      createdPvc <- cleanly(context.provider, namespace.name) { kube =>
        for(
          _ <- (pvOpt map { pv =>
            kube.create[PersistentVolume](pv) recoverWith { case e: K8SException =>
              e.printStackTrace()
              Future.failed(new RuntimeException(s"Failed creating PersistentVolume for volume '${metaResource.name}': " + e.status.message))
            }
          }).sequence;
          createdPvc <- kube.create[PersistentVolumeClaim](pvc) recoverWith { case e: K8SException =>
            e.printStackTrace()
            Future.failed(new RuntimeException(s"Failed creating PersistentVolumeClaim for volume '${metaResource.name}': " + e.status.message))
          }
        ) yield createdPvc
      }
    ) yield {
      val externalId = Map(
        "external_id" -> s"/namespaces/${createdPvc.namespace}/persistentvolumeclaims/${createdPvc.name}"
      )
      val reclamationPolicy = (some composeOptional pvSourceOptics).getOption(pvOpt) match {
        case Some(_: skuber.Volume.HostPath) | Some(_: skuber.Volume.GenericVolumeSource) => Map("reclamation_policy" -> "delete_persistent_volume")
        case _ => Map()
      }
      metaResource.copy(properties = Some((metaResource.properties getOrElse Map()) ++ externalId ++ reclamationPolicy))
    }
  }

  def destroyVolume(volume: GestaltResourceInstance): Future[Unit] = {
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

      val namespace = getNamespaceForResource[skuber.PersistentVolumeClaim](volume)

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

  def updateVolume(context: ProviderContext, metaResource: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    log.warn("KubernetesService::updateVolume is currently a no-op and is not expected to be called")
    Future.successful(metaResource)
  }


  private def fillSecretsInPodTemplate(context: ProviderContext, specProperties: ContainerSpec, podTemplate: skuber.Pod.Template.Spec): EitherError[skuber.Pod.Template.Spec] = {
    import cats.instances.list._
    import cats.instances.either._
    import monocle.std.option._

    val podTemplateEnvSecretsOptics = GenLens[skuber.Pod.Template.Spec](_.spec) composePrism some composeLens
     GenLens[skuber.Pod.Spec](_.containers) composeTraversal Traversal.fromTraverse[List,skuber.Container] composeLens
     GenLens[skuber.Container](_.env) composeTraversal Traversal.fromTraverse[List,skuber.EnvVar] composeLens
     GenLens[skuber.EnvVar](_.value) composePrism GenPrism[skuber.EnvVar.Value,skuber.EnvVar.SecretKeyRef] composeLens
     GenLens[skuber.EnvVar.SecretKeyRef](_.name)

    val podTemplateVolumeSecretsOptics = GenLens[skuber.Pod.Template.Spec](_.spec) composePrism some composeLens
     GenLens[skuber.Pod.Spec](_.volumes) composeTraversal Traversal.fromTraverse[List,skuber.Volume] composeLens
     GenLens[skuber.Volume](_.source) composePrism GenPrism[skuber.Volume.Source,skuber.Volume.Secret] composeLens
     GenLens[skuber.Volume.Secret](_.secretName)
    
    val allSecretsInEnv = (ResourceFactory.findChildrenOfType(ResourceIds.Secret, context.environmentId) map { secret =>
      secret.id -> secret
    }).toMap

    for(
      secrets <- specProperties.secrets.toList traverse { secretMount =>
        val ee: EitherError[(String,SecretSpec)] = for(
          secretResource <- eitherFrom[Error.BadRequest].option(allSecretsInEnv.get(secretMount.secret_id),
           s"secret with ID '${secretMount.secret_id}' does not exist in environment '${context.environmentId}'");
          secretResourceWithName = secretResource.copy(     // it's unfortunate that this needs to be done
            properties=secretResource.properties map { props =>
              props ++ Map("name" -> secretResource.name)
            }
          );
          secretProperties <- ResourceSerde.deserialize[SecretSpec,Error.UnprocessableEntity](secretResourceWithName);
          _ <- if(secretProperties.provider.id == context.providerId) {
            Right(())
          }else {
            Left(Error.UnprocessableEntity(s"secret with ID '${secretMount.secret_id}' belongs to a different provider"))
          }
        ) yield s"!${secretMount.secret_id}" -> secretProperties
        ee
      }
    ) yield  {
      val secretsMapping = secrets.toMap
      val setSecretName = { secretId: String =>
        secretsMapping.get(secretId) match {
          case Some(s) => s.name
          case None => secretId
        }
      }
      val transformations = (podTemplateEnvSecretsOptics.modify(setSecretName) compose podTemplateVolumeSecretsOptics.modify(setSecretName))
      transformations(podTemplate)
    }
  }

  private def fillVolumesInPodTemplate(context: ProviderContext, specProperties: ContainerSpec, podTemplate: skuber.Pod.Template.Spec): EitherError[skuber.Pod.Template.Spec] = {
    import cats.instances.list._
    import cats.instances.either._
    import monocle.std.option._

    val podTemplateVolumeMountsOptics = GenLens[skuber.Pod.Template.Spec](_.spec) composePrism some composeLens
     GenLens[skuber.Pod.Spec](_.containers) composeTraversal Traversal.fromTraverse[List,skuber.Container] composeLens
     GenLens[skuber.Container](_.volumeMounts) composeTraversal Traversal.fromTraverse[List,skuber.Volume.Mount]

    val podTemplateVolumePVCRefOptics = GenLens[skuber.Pod.Template.Spec](_.spec) composePrism some composeLens
     GenLens[skuber.Pod.Spec](_.volumes) composeTraversal Traversal.fromTraverse[List,skuber.Volume] composeLens
     GenLens[skuber.Volume](_.source) composePrism GenPrism[skuber.Volume.Source,skuber.Volume.PersistentVolumeClaimRef]

    val mountedExistingVolumeIds = specProperties.volumes collect {
      case ContainerSpec.ExistingVolumeMountSpec(_, volumeId) => volumeId
    }

    for(
      volumes <- mountedExistingVolumeIds.toList traverse { volumeId =>
        val ee: EitherError[(String,(String,Boolean))] = for(
          volumeResource <- eitherFrom[Error.BadRequest].option(ResourceFactory.findById(migrations.V13.VOLUME_TYPE_ID, volumeId),
           "container spec had volume mount for non-existent volume");
          volumeResourceWithName = volumeResource.copy(     // it's unfortunate that this needs to be done
            properties=volumeResource.properties map { props =>
              props ++ Map("name" -> volumeResource.name)
            }
          );
          volumeProperties <- ResourceSerde.deserialize[VolumeSpec,Error.UnprocessableEntity](volumeResourceWithName);
          externalId <- eitherFrom[Error.UnprocessableEntity].option(volumeProperties.external_id, s"Volume resource '${volumeResource.id}' did not have 'external_id'");
          pvcName <- externalId.split("/") match {
            case Array("", "namespaces", _, "persistentvolumeclaims", name) => Right(name) /* TODO: need to check namespace */
            case _ => Left(Error.Default(s"Volume resource '${volumeResource.id}' 'external_id' was not well-formed"))
          }
        ) yield s"!${volumeResource.id}" -> (pvcName, volumeProperties.access_mode == VolumeSpec.ReadOnlyMany)
        ee
      }
    ) yield {
      val volumesMapping = volumes.toMap
      // inserting correct pvc names and read only flag
      val podTemplateWithPVCRefs = (podTemplateVolumePVCRefOptics modify { pvcRef: skuber.Volume.PersistentVolumeClaimRef =>
        volumesMapping.get(pvcRef.claimName) match {
          case Some((pvcName, readOnly)) => pvcRef.copy(claimName = pvcName, readOnly = readOnly)
          case None => pvcRef
        }
      })(podTemplate)
      (podTemplateVolumeMountsOptics modify { mount: skuber.Volume.Mount =>
        volumesMapping.get(s"!${mount.name.drop(10)}") match {     // chopping off "volume-ex-" from the beginning
          case Some((_, readOnly)) => mount.copy(readOnly = readOnly)
          case None => mount
        }
      })(podTemplateWithPVCRefs)
    }
  }

  type DeploymentSpecs = (skuber.ext.Deployment, Option[skuber.Service], Option[skuber.Service], Option[skuber.Service],
   Option[skuber.ext.Ingress])

  private def buildDeploymentSpecs(context: ProviderContext, metaResource: GestaltResourceInstance, specProperties: ContainerSpec): EitherError[DeploymentSpecs] = {
    import KubernetesProviderProperties.Implicits._

    for(
      providerResource <- eitherFrom[Error.NotFound].option(ResourceFactory.findById(context.providerId), s"Provider not found: ${context.providerId}");
      providerProperties <- ResourceSerde.deserialize[KubernetesProviderProperties.Properties,Error.UnprocessableEntity](providerResource);
      podTemplateOriginal <- keb.mkPodTemplate(providerProperties, specProperties);
      podTemplateWithSecrets <- fillSecretsInPodTemplate(context, specProperties, podTemplateOriginal);
      podTemplateWithVolumes <- fillVolumesInPodTemplate(context, specProperties, podTemplateWithSecrets);
      podTemplateWithLabels = podTemplateWithVolumes.addLabels(
        specProperties.labels ++ Map(
          KubernetesConstants.META_CONTAINER_KEY -> s"${metaResource.id}"
        ) ++ mkLabels(context)
      );
      clusterIpServiceSpec <- keb.mkClusterIpServiceSpec(providerProperties, specProperties);
      nodePortServiceSpec <- keb.mkNodePortServiceSpec(providerProperties, specProperties);
      loadBalancerServiceSpec <- keb.mkLoadBalancerServiceSpec(providerProperties, specProperties);
      ingressSpec <- keb.mkIngressSpec(providerProperties, specProperties)
    ) yield {
      val deployment = skuber.ext.Deployment(
        metadata = skuber.ObjectMeta(
          name = specProperties.name,
          namespace = "",
          labels = specProperties.labels ++ Map(
            KubernetesConstants.META_CONTAINER_KEY -> s"${metaResource.id}"
          ) ++ mkLabels(context)
        ),
        spec = Some(skuber.ext.Deployment.Spec(
          replicas = Some(specProperties.num_instances),
          selector = Some(skuber.LabelSelector(skuber.LabelSelector.IsEqualRequirement(
            KubernetesConstants.META_CONTAINER_KEY, s"${metaResource.id}"
          ))),
          template = Some(podTemplateWithLabels)
        ))
      )
      val mkService = { (name: String, spec: skuber.Service.Spec) =>
        skuber.Service(
          metadata = skuber.ObjectMeta(
            name = name,
            namespace = "",
            labels = Map(
              KubernetesConstants.META_CONTAINER_KEY -> s"${metaResource.id}"
            ) ++ mkLabels(context)
          ),
          spec = Some(spec.withSelector(Map(KubernetesConstants.META_CONTAINER_KEY -> s"${metaResource.id}")))
        )
      }
      val mkIngress = { spec: skuber.ext.Ingress.Spec =>
        skuber.ext.Ingress(
          metadata = skuber.ObjectMeta(
            name = specProperties.name,
            namespace = "",
            labels = Map(
              KubernetesConstants.META_CONTAINER_KEY -> s"${metaResource.id}"
            ) ++ mkLabels(context)
          ),
          spec = Some(spec)
        )
      }
      (deployment,
       clusterIpServiceSpec.map(mkService(specProperties.name, _)),
       nodePortServiceSpec.map(mkService(s"${specProperties.name}-ext", _)),
       loadBalancerServiceSpec.map(mkService(s"${specProperties.name}-lb", _)),
       ingressSpec.map(mkIngress(_))
      )
    }
  }

  private def updatePortMapping(pm: ContainerSpec.PortMapping, clusterIpServiceOpt: Option[skuber.Service],
   nodePortServiceOpt: Option[skuber.Service], loadBalancerServiceOpt: Option[skuber.Service]): ContainerSpec.PortMapping = {
    
    def svcMatch(pm: ContainerSpec.PortMapping, sp: Service.Port)  = {
      pm.name.contains(sp.name) || (pm.lb_port.filter(_ != 0) orElse pm.container_port).contains(sp.port)
    }

    clusterIpServiceOpt match {
      case None =>
        log.debug(s"updateContainerSpecPortMappings: No Service(ClusterIP)")
        pm.copy(
          service_address = None,
          lb_address = None
        )
      case Some(cipSvc) =>
        val svcHost = s"${cipSvc.name}.${cipSvc.namespace}.svc.cluster.local"
        val cipPorts = cipSvc.spec.map(_.ports) getOrElse List.empty
        cipPorts.find( svcMatch(pm, _) ) match {
          case Some(sp) if pm.expose_endpoint.contains(true) =>
            log.debug(s"updateContainerSpecPortMappings: PortMapping ${pm} matched to Service.Port ${sp}")
            val nodePort = nodePortServiceOpt.flatMap(
              _.spec.flatMap(
                _.ports.find( svcMatch(pm, _) )
              )
            ) map (_.nodePort)
            val lbAddress = for {
              lbSvc <- loadBalancerServiceOpt
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

  private def createDeployment(context: ProviderContext, metaResource: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    import cats.instances.option._

    for(
      specProperties0 <- ResourceSerde.deserialize[ContainerSpec,Error.UnprocessableEntity](metaResource).liftTo[Future];
      specProperties = specProperties0.copy(name=metaResource.name);    // this should be done somewhere else
      skuberSpecs <- buildDeploymentSpecs(context, metaResource, specProperties).liftTo[Future];
      (deploymentSpec, clusterIpServiceSpecOpt, nodePortServiceSpecOpt, loadBalancerServiceSpecOpt, ingressSpecOpt) = skuberSpecs;
      namespace <- cleanly(context.provider, DefaultNamespace) { kube =>
        getNamespace(kube, context, create = true)
      };

      // filling in namespace
      setServiceNamespace = GenLens[skuber.Service](_.metadata.namespace).set(namespace.name);
      setDeploymentNamespace = GenLens[skuber.ext.Deployment](_.metadata.namespace).set(namespace.name);
      setIngressNamespace = GenLens[skuber.ext.Ingress](_.metadata.namespace).set(namespace.name);

      updatedResource <- cleanly(context.provider, namespace.name) { kube =>
        val fDeployment = kube.create[skuber.ext.Deployment](setDeploymentNamespace(deploymentSpec)) recoverWith { case e: K8SException =>
          e.printStackTrace()
          Future.failed(new RuntimeException(s"Failed creating Kubernetes Deployment for container '${metaResource.name}': ${e.status.message}"))
        }
        val createService = { service: skuber.Service =>
          kube.create[skuber.Service](setServiceNamespace(service)) recoverWith { case e: K8SException =>
            e.printStackTrace()
            Future.failed(new RuntimeException(s"Failed creating Kubernetes Service ${service.name} for container '${metaResource.name}': ${e.status.message}"))
          }
        }
        val fClusterIpServiceOpt = clusterIpServiceSpecOpt.map(createService(_)).sequence
        val fNodePortServiceOpt = nodePortServiceSpecOpt.map(createService(_)).sequence
        val fLoadBalancerServiceOpt = loadBalancerServiceSpecOpt.map(createService(_)).sequence
        val fIngressOpt = (ingressSpecOpt map { ingress =>
          kube.create[skuber.ext.Ingress](setIngressNamespace(ingress)) recoverWith { case e: K8SException =>
            e.printStackTrace()
            Future.failed(new RuntimeException(s"Failed creating Kubernetes Ingress for container '${metaResource.name}': ${e.status.message}"))
          }
        }).sequence
        for(
          deployment <- fDeployment;
          clusterIpServiceOpt <- fClusterIpServiceOpt;
          nodePortServiceOpt <- fNodePortServiceOpt;
          loadBalancerServiceOpt <- fLoadBalancerServiceOpt;
          _ <- fIngressOpt
        ) yield {
          val updatedPortMappings: Seq[ContainerSpec.PortMapping] = specProperties.port_mappings map { pm =>
            updatePortMapping(pm, clusterIpServiceOpt, nodePortServiceOpt, loadBalancerServiceOpt)
          }
          val extraProps = Map(
            "external_id" -> s"/namespaces/${deployment.namespace}/deployments/${deployment.name}",
            "status" -> "LAUNCHED",
            "port_mappings" -> Json.stringify(Json.toJson(updatedPortMappings))
          )
          metaResource.copy(properties = Some((metaResource.properties getOrElse Map()) ++ extraProps))
        }
      }
    ) yield updatedResource
  }

  private def updateDeployment(context: ProviderContext, metaResource: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    import monocle.std.option._

    val nameGetter = "/namespaces/[^/]+/deployments/(.*)".r
    val previousName = for {
      eid <- ContainerService.resourceExternalId(metaResource)
      prev <- eid match {
        case nameGetter(name) => Some(name)
        case _ => None
      }
    } yield prev

    def reconcile[T <: skuber.ObjectResource](kube: RequestContext, pair: (Option[T], Option[T]))
     (implicit fmt: Format[T], rd: skuber.ResourceDefinition[T]): Future[Option[T]] = {
      pair match {
        case (None, Some(n)) => kube.create[T](n) map Option.apply
        case (Some(o), Some(n)) => kube.update[T](n) map Option.apply
        case (Some(o), None) => kube.delete[T](o.name) map { _ => None }
        case (None, None) => Future.successful(None)
      }
    }

    val namespace = getNamespaceForResource[skuber.ext.Deployment](metaResource)

    for(
      _ <- (if(previousName.isDefined && previousName != Some(metaResource.name)) {
        Left(Error.BadRequest("renaming containers is not supported"))
      }else {
        Right(())
      }).liftTo[Future];
      specProperties0 <- ResourceSerde.deserialize[ContainerSpec](metaResource).liftTo[Future];
      specProperties = specProperties0.copy(name=metaResource.name);    // this should be done somewhere else
      skuberSpecs <- buildDeploymentSpecs(context, metaResource, specProperties).liftTo[Future];
      (newDeployment, newClusterIpServiceOpt, newNodePortServiceOpt, newLoadBalancerServiceOpt, newIngressOpt) = skuberSpecs;
      updatedResource <- cleanly(context.provider, namespace) { kube =>
        // fetching old kubernetes entities for this container
        val fGetServices = kube.listSelected[skuber.ServiceList](META_CONTAINER_KEY is s"${metaResource.id}").map(_.items)
        val fGetIngressOpt = kube.getOption[skuber.ext.Ingress](specProperties.name)
        val serviceTypeOptics = GenLens[skuber.Service](_.spec) composePrism some composeLens GenLens[skuber.Service.Spec](_._type)
        for(
          services <- fGetServices;
          oldClusterIpServiceOpt = services.find(serviceTypeOptics.getOption(_) == Some(skuber.Service.Type.ClusterIP));
          oldNodePortServiceOpt = services.find(serviceTypeOptics.getOption(_) == Some(skuber.Service.Type.NodePort));
          oldLoadBalancerServiceOpt = services.find(serviceTypeOptics.getOption(_) == Some(skuber.Service.Type.LoadBalancer));
          oldIngressOpt <- fGetIngressOpt;

          // setting cluster ip and resource version to new Service spec – otherwise kubernetes will disallow updating the service
          withClusterIp = { (o0: Option[skuber.Service], n0: Option[skuber.Service]) =>
            val clusterIpOptics = GenLens[skuber.Service](_.spec) composePrism some composeLens GenLens[skuber.Service.Spec](_.clusterIP)
            (o0, n0) match {
              case(Some(o), Some(n)) => {
                val n1 = n.withClusterIP(clusterIpOptics.getOption(o).getOrElse("")).withResourceVersion(o.resourceVersion)
                (o0, Some(n1))
              }
              case _ => (o0, n0)
            }
          };
          // filling in namespace
          setServiceOptNamespace = (some composeLens GenLens[skuber.Service](_.metadata.namespace)).set(namespace);
          setDeploymentNamespace = GenLens[skuber.ext.Deployment](_.metadata.namespace).set(namespace);
          setIngressOptNamespace = (some composeLens GenLens[skuber.ext.Ingress](_.metadata.namespace)).set(namespace);

          fClusterIpServiceOpt = reconcile(kube, withClusterIp(oldClusterIpServiceOpt, setServiceOptNamespace(newClusterIpServiceOpt)));
          fNodePortServiceOpt = reconcile(kube, withClusterIp(oldNodePortServiceOpt, setServiceOptNamespace(newNodePortServiceOpt)));
          fLoadBalancerServiceOpt = reconcile(kube, withClusterIp(oldLoadBalancerServiceOpt, setServiceOptNamespace(newLoadBalancerServiceOpt)));
          fIngressOpt = reconcile(kube, (oldIngressOpt, setIngressOptNamespace(newIngressOpt)));
          fUpdateDeployment = kube.update[skuber.ext.Deployment](setDeploymentNamespace(newDeployment));
          clusterIpServiceOpt <- fClusterIpServiceOpt;
          nodePortServiceOpt <- fNodePortServiceOpt;
          loadBalancerServiceOpt <- fLoadBalancerServiceOpt;
          _ <- fIngressOpt;
          deployment <- fUpdateDeployment
        ) yield {
          val updatedPortMappings: Seq[ContainerSpec.PortMapping] = specProperties.port_mappings map { pm =>
            updatePortMapping(pm, clusterIpServiceOpt, nodePortServiceOpt, loadBalancerServiceOpt)
          }
          val extraProps = Map(
            "external_id" -> s"/namespaces/${deployment.namespace}/deployments/${deployment.name}",
            "status" -> "LAUNCHED",
            "port_mappings" -> Json.stringify(Json.toJson(updatedPortMappings))
          )
          metaResource.copy(properties = Some((metaResource.properties getOrElse Map()) ++ extraProps))
        }
      }
    ) yield updatedResource
  }

  def createJob(context: ProviderContext, metaResource: Instance) (implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    import KubernetesProviderProperties.Implicits._
    import skuber.json.batch.format._
    import monocle.std.option._

    val podTemplateRestartPolicy = GenLens[skuber.Pod.Template.Spec](_.spec) composePrism some composeLens
     GenLens[skuber.Pod.Spec](_.restartPolicy)

    val eitherJobSpec: EitherError[skuber.batch.Job] = for(
      specProperties0 <- ResourceSerde.deserialize[ContainerSpec,Error.UnprocessableEntity](metaResource);
      specProperties = specProperties0.copy(name=metaResource.name);    // this should be done somewhere else
      providerResource <- eitherFrom[Error.NotFound].option(ResourceFactory.findById(context.providerId), s"Provider not found: ${context.providerId}");
      providerProperties <- ResourceSerde.deserialize[KubernetesProviderProperties.Properties,Error.UnprocessableEntity](providerResource);
      _ <- if(specProperties.num_instances > 1) {
        Left(Error.BadRequest("Setting num_replicas > 1 is not allowed for jobs"))
      }else {
        Right(())
      };
      podTemplateOriginal <- keb.mkPodTemplate(providerProperties, specProperties);
      // restart policy can be OnFailure or Never for Jobs. Using Never here because it makes most sense for laser use case
      // deployments have restart policy Always at all times
      podTemplateWithRestartPolicy = podTemplateRestartPolicy.set(skuber.RestartPolicy.Never)(podTemplateOriginal);
      podTemplateWithSecrets <- fillSecretsInPodTemplate(context, specProperties, podTemplateWithRestartPolicy);
      podTemplateWithVolumes <- fillVolumesInPodTemplate(context, specProperties, podTemplateWithSecrets)
    ) yield {
      skuber.batch.Job(
        metadata = skuber.ObjectMeta(
          name = specProperties.name,
          namespace = "",
          labels = Map(
            KubernetesConstants.META_JOB_KEY -> s"${metaResource.id}"
          ) ++ mkLabels(context)
        ),
        spec = Some(skuber.batch.Job.Spec(
          template = Some(podTemplateWithVolumes)
        ))
      )
    }

    val jobNamespaceOptics = GenLens[skuber.batch.Job](_.metadata.namespace)

    for(
      jobSpec <- eitherJobSpec.liftTo[Future];
      namespace <- cleanly(context.provider, DefaultNamespace) { kube =>
        getNamespace(kube, context, create = true)
      };
      job <- cleanly(context.provider, namespace.name) { kube =>
        kube.create[skuber.batch.Job](jobNamespaceOptics.set(namespace.name)(jobSpec)) recoverWith { case e: K8SException =>
          e.printStackTrace()
          Future.failed(new RuntimeException(s"Failed creating Job '${metaResource.name}': ${e.status.message}"))
        }
      }
    ) yield {
      val extraProps = Map(
        "external_id" -> s"/namespaces/${job.ns}/jobs/${job.name}",
        "status" -> "LAUNCHED"
      )
      metaResource.copy(properties = Some((metaResource.properties getOrElse Map()) ++ extraProps))
    }
  }
  
  def destroyJob(job: GestaltResourceInstance): Future[Unit] = {
    import skuber.json.batch.format._
    
    val provider = ContainerService.containerProvider(job)

    val namespace = getNamespaceForResource[skuber.batch.Job](job)

    cleanly(provider, namespace) { kube =>
      for(
        jobs <- kube.listSelected[ListResource[skuber.batch.Job]](META_JOB_KEY is s"${job.id}");
        selectors = jobs map { job =>
          job.spec.flatMap(_.selector).get
        };
        pods <- (Future.traverse(selectors) { selector =>
          kube.listSelected[PodList](selector).map(_.items)
        }) recover {
          case e: K8SException => {
            log.warn(s"K8S error listing/deleting Pods associated with job ${job.id}")
            List()
          }
        };
        _ <- Future.traverse(jobs.items) { job =>
          kube.delete[skuber.batch.Job](job.name)
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
    }
  }

}

