package services
import java.nio.ByteBuffer
import java.security.MessageDigest
import java.util.UUID
import javax.inject.Inject

import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.{ContainerSpec, ContainerStats, SecretSpec => MetaSecretSpec}
import com.galacticfog.gestalt.meta.api.ContainerSpec.{PortMapping, ServiceAddress}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.spotify.docker.client.exceptions.ServiceNotFoundException
import com.spotify.docker.client.messages.swarm._
import com.spotify.docker.client.{DefaultDockerClient, DockerClient, messages => docker}
import controllers.util.ContainerService
import org.joda.time.DateTime
import org.slf4j.LoggerFactory
import play.api.libs.json.Json
import services.util.CommandParser

import collection.JavaConverters._
import scala.annotation.meta
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object DockerService {
  val META_CONTAINER_KEY = "gestalt-meta/container"
  val META_ENVIRONMENT_KEY = "gestalt-meta/environment"
  val META_WORKSPACE_KEY = "gestalt-meta/workspace"
  val META_FQON_KEY = "gestalt-meta/fqon"
  val META_PROVIDER_KEY = "gestalt-meta/provider"

  val stagedStates = Set(
    TaskStatus.TASK_STATE_ACCEPTED,
    TaskStatus.TASK_STATE_ALLOCATED,
    TaskStatus.TASK_STATE_ASSIGNED,
    TaskStatus.TASK_STATE_PENDING,
    TaskStatus.TASK_STATE_PREPARING,
    TaskStatus.TASK_STATE_STARTING
  )

  type DockerClient = com.spotify.docker.client.DockerClient

  def containerShortName(envId: UUID, resourceName: String): String = {
    // docker service/container name must be <= 63 characters
    // envId is our namespace and container name is unique in that namespace, so we'll use those
    // 32-chars for the UUID (after removing dashes) leaves 31
    // one for the dash separator (wasteful, but nice looking) leaves 30
    // we'll hash the name, then hash the hash down to 8 characters
    // that allows us 30-8==22 characters of the original name for human readability
    val shortName = if (resourceName.length <= 30) resourceName else {
      val chk = ByteBuffer.wrap(MessageDigest.getInstance("MD5").digest(resourceName.getBytes))
      // keep 8 chars == 4 bytes; MD5 is 128-bits/16-bytes; so, need to halve twice
      val red = (chk.getInt(0) ^ chk.getInt(1)) ^ (chk.getInt(2) ^ chk.getInt(3))
      val shortChk = Integer.toHexString(int2Integer(red))
      resourceName.substring(0,22) + shortChk
    }
    envId.toString.replace("-","") + "-" + shortName
  }

  def getExternalId(container: GestaltResourceInstance): Option[String] = {
    ContainerService.containerExternalId(container) orElse {
      for {
        envId <- ResourceFactory.findParent(ResourceIds.Environment, container.id).map(_.id)
      } yield containerShortName(envId, container.name)
    }
  }
}

trait DockerClientFactory {
  def getDockerClient(providerId: UUID): Try[DockerClient]
}

class DefaultDockerClientFactory @Inject() () extends DockerClientFactory {
  override def getDockerClient(providerId: UUID): Try[DockerClient] = {
    for {
      provider <- Try{ContainerService.caasProvider(providerId)}
      url <- ContainerService.getProviderProperty[String](provider, "url") match {
        case Some(c) => Success(c)
        case None => Failure(new RuntimeException("provider 'properties.config' missing or not parsable"))
      }
      client <- Try{DefaultDockerClient.builder().uri(url).build()}
    } yield client
  }
}

class DockerService @Inject() ( dockerClientFactory: DockerClientFactory ) extends CaasService {

  import DockerService._

  private[this] val log = LoggerFactory.getLogger(this.getClass)

  def cleanly[T](providerId: UUID)(f: DockerClient => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    dockerClientFactory.getDockerClient(providerId) match {
      case Failure(e) =>
        log.warn("failed to instantiate docker client",e)
        Future.failed(e)
      case Success(client) =>
        val fT = f(client)
        fT.onComplete(_ => client.close())
        fT
    }
  }

  private[services] def mkServiceSpec(containerResourceId: UUID, externalId: String, containerSpec: ContainerSpec, providerId: UUID, fqon: String, workspaceId: UUID, environmentId: UUID): Try[docker.swarm.ServiceSpec] = {
    val allLabels = containerSpec.labels ++ Map[String,String](
      META_CONTAINER_KEY -> containerResourceId.toString,
      META_ENVIRONMENT_KEY -> environmentId.toString,
      META_FQON_KEY -> fqon,
      META_WORKSPACE_KEY -> workspaceId.toString,
      META_PROVIDER_KEY -> providerId.toString
    )

    /** for port mappings, docker has to modes for publishing a service:
      * vip mode (use the routing mesh): each swarm node will publish the service on the same (service) port
      *    we will allow use service_port semantics for this, and allow use to optionally specify the service port
      *    this is equivalent to Kube Service of type NodePort (what we do in the Kube adapter)
      * host mode: only swarm modes running a container will publish the port
      *    we will NOT support this right now; later on, we can support it, perhaps according to host_port:
      *    host_port match {
      *      case None => use vip mode
      *      case Some(0) => use host mode with random host port
      *      case Some(p) => use host mode with user-specified host port
      *    }
      */

    val maybeCmdArray = containerSpec.cmd map { cmdStr => Seq("/bin/sh", "-c", cmdStr) } map (_.asJava)
    val env = containerSpec.env.map({
      case (k, v) => (k + "=" + v)
    }).toSeq

    Try {
      docker.swarm.ServiceSpec.builder()
        .name(externalId)
        .mode(ServiceMode.withReplicas(containerSpec.num_instances))
        .taskTemplate(docker.swarm.TaskSpec.builder()
//          .resources(ResourceRequirements.builder().reservations(Resources.builder()
//              .nanoCpus( (containerSpec.cpus * 1e9).toLong )
//              .memoryBytes( (containerSpec.memory * 1024.0 * 1024.0).toLong )
//              .build()
//          ).build())
          .containerSpec(
            docker.swarm.ContainerSpec.builder()
              .image(containerSpec.image)
              .args(containerSpec.args.map(_.asJava).getOrElse(null))
              .command(maybeCmdArray.getOrElse(null))
              .env(env:_*)
              .build()
          )
          .build()
        )
        .networks(containerSpec.network
          .map(name => Seq(NetworkAttachmentConfig.builder().target(name).build()))
          .getOrElse(Seq.empty):_*
        )
        .endpointSpec(
          EndpointSpec.builder()
            .mode(EndpointSpec.Mode.RESOLUTION_MODE_VIP)
            .ports(
              containerSpec.port_mappings.collect({
                case PortMapping(protocol, Some(containerPort), _, maybeServicePort, Some(portName), _, Some(true), _, _) =>
                  PortConfig.builder()
                    .name(portName)
                    .protocol(protocol)
                    .publishMode(PortConfig.PortConfigPublishMode.INGRESS)
                    .targetPort(containerPort)
                    .publishedPort(maybeServicePort.map(int2Integer).getOrElse(null))
                    .build()
              }).asJava
            )
            .build()
        )
        .labels(allLabels.asJava)
        .build()
    }
  }

  private[services] def createService( docker: DockerClient,
                                       containerId: UUID,
                                       externalId: String,
                                       spec: ContainerSpec,
                                       providerId: UUID,
                                       fqon: String,
                                       workspaceId: UUID,
                                       environmentId: UUID )
                                     ( implicit ec: ExecutionContext ): Future[ContainerSpec] = {
    for {
      config <- Future.fromTry(mkServiceSpec(containerId, externalId, spec, providerId, fqon, workspaceId, environmentId))
      response <- Future{docker.createService(config)}
      newPortMappings = spec.port_mappings map {
        case pm @ PortMapping(proto, Some(cp), _, _, _, _, Some(true), _, maybeVHosts) =>
          pm.copy(service_address = Some(ServiceAddress(
            host = externalId,
            port = cp,
            protocol = Some(proto),
            virtual_hosts = maybeVHosts
          )))
        case pm => pm.copy(
          service_address = None
        )
      }
    } yield spec.copy(
      port_mappings = newPortMappings
    )
  }

  override def create(context: ProviderContext,
                      container: GestaltResourceInstance )
                     ( implicit ec: ExecutionContext ): Future[GestaltResourceInstance] = {
    log.debug("DockerService::create(...)")
    ContainerSpec.fromResourceInstance(container) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) =>
        val externalId = containerShortName(context.environmentId, spec.name)
        cleanly(context.providerId)(
          createService(_, container.id, externalId, spec, context.providerId, context.fqon, context.workspace.id, context.environmentId)
        ) map { updatedContainerSpec =>
          upsertProperties(
            container,
            "external_id" -> externalId,
            "status" -> "LAUNCHED",
            "port_mappings" -> Json.toJson(updatedContainerSpec.port_mappings).toString()
          )
        }
    }
  }

  override def destroy(container: GestaltResourceInstance): Future[Unit] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext
    log.debug("DockerService::create(...)")
    val provider = ContainerService.containerProvider(container)
    val externalId = getExternalId(container) getOrElse(throw new BadRequestException("Could not determine 'external_id' for container or find parent to reconstruct the 'external_id'. Container resource may be corrupt."))
    cleanly(provider.id) ( docker => Future(docker.removeService(externalId)))
  }


  override def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext
    getExternalId(container) match {
      case None => Future.successful(None)
      case Some(extId) =>
        cleanly(context.providerId) { docker =>
          val fSvc = Future(docker.inspectService(extId))
          // val fTasks = Future(docker.listTasks(Task.Criteria.builder().serviceName(extId).build()))
          for {
            maybeSvc <- fSvc.map(Some(_)).recover {case _: ServiceNotFoundException => None}
            _ = log.info(s"found service: ${maybeSvc}")
            // javaTasks <- fTasks
            // tasks = javaTasks.asScala
            // _ = log.info(s"found ${tasks.size} tasks")
            stats = maybeSvc map { svc =>
              ContainerStats(
                external_id = svc.spec().name(),
                containerType = "DOCKER",
                status = "RUNNING",
                cpus = Try{svc.spec().taskTemplate().resources().limits().nanoCpus().toDouble / 1e9}.getOrElse[Double](0),
                memory = Try{svc.spec().taskTemplate().resources().limits().memoryBytes().toDouble / (1024*1024)}.getOrElse[Double](0),
                image = Try{svc.spec().taskTemplate().containerSpec().image()}.getOrElse(""),
                age = new DateTime(svc.createdAt()),
                numInstances = Try{svc.spec().mode().replicated().replicas().toInt}.getOrElse[Int](0),
                tasksStaged = 0, // tasks.count( t => stagedStates.contains(t.status().state()) ),
                tasksRunning = 0, // tasks.count( _.status().state() == TaskStatus.TASK_STATE_RUNNING ),
                tasksHealthy = 0,
                tasksUnhealthy = 0,
                taskStats = None /* Some(tasks.map(
                  t => ContainerStats.TaskStat(
                    id = t.id(),
                    host = "",
                    ipAddresses = Some(t.networkAttachments().asScala.flatMap(_.addresses().asScala).map(
                      a => ContainerStats.TaskStat.IPAddress(
                        ipAddress = a,
                        protocol = "tcp"
                    ))),
                    ports = Seq.empty,
                    startedAt = Some(t.createdAt().toString)
                  )
                )) */
              )
            }
          } yield stats
        }
    }
  }


  override def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]] = {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext
    log.debug("DockerService::listInEnvironment(...)")

    cleanly(context.providerId) (
      docker => {
        val fSvcs = Future(docker.listServices(Service.Criteria.builder().addLabel(META_ENVIRONMENT_KEY, context.environmentId.toString).build()).asScala)
        //        val fTasks = Future(docker.listTasks(Task.Criteria.builder().label(META_ENVIRONMENT_KEY + "=" + context.environmentId.toString).build()).asScala)
        val fTasks = Future(docker.listTasks())
        for {
          svcs <- fSvcs
          _ = log.info(s"found ${svcs.size} services in environment")
          // tasks <- fTasks
          // _ = log.info(s"found ${tasks.size} tasks in environment")
          stats = svcs.map { svc =>
            // val svcTasks = tasks.asScala.filter(_.serviceId() == svc.id())
            ContainerStats(
              external_id = svc.spec().name(),
              containerType = "DOCKER",
              status = "RUNNING",
              cpus = Try {
                svc.spec().taskTemplate().resources().limits().nanoCpus().toDouble / 1e9
              }.getOrElse[Double](0),
              memory = Try {
                svc.spec().taskTemplate().resources().limits().memoryBytes().toDouble / (1024 * 1024)
              }.getOrElse[Double](0),
              image = svc.spec().taskTemplate().containerSpec().image(),
              age = new DateTime(svc.createdAt()),
              numInstances = Try { svc.spec().mode().replicated().replicas().toInt }.getOrElse[Int](0),
              tasksStaged = 0, // svcTasks.count( t => stagedStates.contains(t.status().state()) ),
              tasksRunning = 0, // svcTasks.count( _.status().state() == TaskStatus.TASK_STATE_RUNNING ),
              tasksHealthy = 0,
              tasksUnhealthy = 0,
              taskStats = None
            )
          }
        } yield stats
      }
    )
  }

  override def update(context: ProviderContext, container: GestaltResourceInstance)(implicit ec: ExecutionContext): Future[GestaltResourceInstance] = ???

  override def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance] = {
    ???
//    import play.api.libs.concurrent.Execution.Implicits.defaultContext
//    log.debug("DockerService::scale(...)")
//    val provider = ContainerService.containerProvider(container)
//    val externalId = ContainerService.containerExternalId(container) orElse {
//      for {
//        envId <- ResourceFactory.findParent(ResourceIds.Environment, container.id).map(_.id.toString)
//      } yield envId + "-" + container.name
//    } getOrElse(throw new BadRequestException("Could not determine 'external_id' for container. Container resource may be corrupt."))
//    for {
//      docker    <- Future.fromTry(dockerClientFactory.getDockerClient(provider.id))
//      extantSvc <- getServiceOption(docker, externalId) flatMap {
//        case Some(svc) => Future.successful(svc)
//        case None => Future.failed(new RuntimeException(
//          s"could not locate associated Service in Docker Swarm provider for container ${container.id}"
//        ))
//      }
//      updatedSvc <- Future{
//        val newSvc = extantSvc.spec().mode().replicated().replicas()
//      }
//    } yield ()
  }

  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  private[this] def getServiceOption(client: DockerClient, serviceName: String)(implicit ec: ExecutionContext): Future[Option[docker.swarm.Service]] = {
    Future{client.inspectService(serviceName)} map (Some(_)) recover {
      case snf: ServiceNotFoundException => None
    }
  }

  override def createSecret(context: ProviderContext, metaResource: Instance, items: Seq[MetaSecretSpec.Item])
                           (implicit ec: ExecutionContext): Future[Instance] = Future.failed(
    new BadRequestException("Docker Swarm CaaS provider does not support secrets")
  )

  override def destroySecret(secret: Instance): Future[Unit] = Future.failed(
    new BadRequestException("DCOS CaaS provider does not support secrets")
  )
}

