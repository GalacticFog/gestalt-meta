package services.kubernetes

import java.util.{Base64, UUID}

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.ContainerSpec.{ExistingVolumeMountSpec, InlineVolumeMountSpec, PortMapping, SecretDirMount, SecretEnvMount, SecretFileMount}
import com.galacticfog.gestalt.meta.api.VolumeSpec.ReadOnlyMany
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec, VolumeSpec}
import com.galacticfog.gestalt.util.ResourceSerde
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.Error
import controllers.util._
import services.util.CommandParser
import services.ProviderContext
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.either._
import skuber.Container.Port
import skuber._
import skuber.ext._
import skuber.json.format._

import scala.annotation.tailrec

case class ContainerServices( intSvc: Option[Service], extSvc: Option[Service], lbSvc: Option[Service] )

/**
Transforms meta entities into kubernetes entities
*/
trait MkKubernetesSpec {
  import KubernetesConstants._

  import KubernetesProviderProperties.Implicits._

  def mkPVCandPV(provider: GestaltResourceInstance, resource: GestaltResourceInstance): EitherError[(PersistentVolumeClaim, Option[PersistentVolume])] = {

    def isAllowedHostPath(providerProperties: KubernetesProviderProperties.Properties, hostPath: String): Boolean = {
      // val allowedHostPaths = ContainerService.getProviderProperty[Seq[String]](provider, HOST_VOLUME_WHITELIST).getOrElse(Seq.empty)
      val hpParts = hostPath.stripSuffix("/").stripPrefix("/").split("/").scan("")(_ + "/" + _)
      hpParts.exists(part => providerProperties.config.host_volume_whitelist.map(_.stripSuffix("/")).contains(part))
    }

    def isConfiguredStorageClass(providerProperties: KubernetesProviderProperties.Properties, storageClass: String): Boolean = {
      // val configuredStorageClasses = ContainerService.getProviderProperty[Seq[String]](provider, STORAGE_CLASSES).getOrElse(Seq.empty)
      providerProperties.config.storage_classes.contains(storageClass)
    }

    for(
      spec <- ResourceSerde.deserialize[VolumeSpec,Error.UnprocessableEntity](resource);
      providerProperties <- ResourceSerde.deserialize[KubernetesProviderProperties.Properties,Error.UnprocessableEntity](provider);
      config <- eitherFromJsResult(spec.parseConfig);
      props <- config match {
        case VolumeSpec.HostPathVolume(hostPath) if isAllowedHostPath(providerProperties, hostPath) => {
          Right((Some(skuber.Volume.HostPath(hostPath)), None))
        }
        case VolumeSpec.HostPathVolume(hostPath) => Left(Error.UnprocessableEntity(s"host_path '$hostPath' is not in provider's white-list"))
        case VolumeSpec.PersistentVolume => Left(Error.BadRequest("Kubernetes providers only support volumes of type 'external', 'dynamic' and 'host_path'"))
        case VolumeSpec.ExternalVolume(config) => {
          Right((Some(skuber.Volume.GenericVolumeSource(config.toString)), None))
        }
        case VolumeSpec.DynamicVolume(storageClass) if isConfiguredStorageClass(providerProperties, storageClass) => {
          Right((None, Some(storageClass)))
        }
        case VolumeSpec.DynamicVolume(storageClass) => Left(Error.UnprocessableEntity(s"storage_class '$storageClass' is not in provider's white-list"))
      }
    ) yield {
      val (sourceOpt, storageClassOpt) = props
      val pvOpt = sourceOpt map { source =>
        PersistentVolume(
          metadata = ObjectMeta(
            name = s"${resource.name.take(8)}-${resource.id}",
            namespace = "",
            labels = Map(
              META_VOLUME_KEY -> s"${resource.id}"
            )
          ),
          spec = Some(PersistentVolume.Spec(
            capacity = Map(Resource.storage -> Resource.Quantity(s"${spec.size}Mi")),
            source = source,
            accessModes = List(spec.access_mode),
            claimRef = Some(skuber.ObjectReference(
              namespace = "__SET_ME__",
              name = resource.name
            ))
          ))
        )
      }
      val pvc = PersistentVolumeClaim(
        metadata = ObjectMeta(
          name = resource.name,
          namespace = "__SET_ME__",
          labels = Map(
            META_VOLUME_KEY -> s"${resource.id}"
          )
        ),
        spec = Some(PersistentVolumeClaim.Spec(
          accessModes = List(spec.access_mode),
          resources = Some(Resource.Requirements(
            requests = Map(
              Resource.storage -> s"${spec.size}Mi"
            )
          )),
          storageClassName = storageClassOpt
        ))
      )
      (pvc, pvOpt)
    }
  }

  private def mkTransportProtocol(protocol: String): EitherError[skuber.Protocol.Protocol] = {
    protocol.toUpperCase match {
      case "TCP" => Right(skuber.Protocol.TCP)
      case "UDP" => Right(skuber.Protocol.UDP)
      case _ => Left(Error.UnprocessableEntity("port mapping must specify \"TCP\" or \"UDP\" as protocol"))
    }
  }

  def mkPodTemplate(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[Pod.Template.Spec] = {
    // I want to 1) keep method signatures simple / short and uniform and 2) keep db access out of the code that merely transforms one data structure into another
    // so filling in namespace and data from other meta resources (including meta/*** labels) needs to happen somewhere else

    for(
      // specProperties <- ResourceSerde.deserialize[ContainerSpec,Error.UnprocessableEntity](resource);
      // providerProperties <- ResourceSerde.deserialize[KubernetesProviderProperties.Properties,Error.UnprocessableEntity](provider);
      portMappings <- specProperties.port_mappings.toList traverse { pm =>
        val ee: EitherError[skuber.Container.Port] = for(
          containerPort <- eitherFrom[Error.UnprocessableEntity].option(pm.container_port, "port mapping must specify container_port");
          protocol <- mkTransportProtocol(pm.protocol);
          _ <- if(specProperties.port_mappings.size > 1 && pm.name.isEmpty) {
            Left(Error.UnprocessableEntity("multiple port mappings requires port names"))
          }else {
            Right(())
          }
        ) yield {
          skuber.Container.Port(
            containerPort = containerPort,
            protocol = protocol,
            name = pm.name.getOrElse(""),
            hostPort = pm.host_port
          )
        }
        ee
      };
      _ <- if(specProperties.health_checks.size > 1) {
        Left(Error.UnprocessableEntity("Kubernetes supports at most one health check/liveness probe."))
      }else {
        Right(())
      };
      probes <- specProperties.health_checks.toList traverse { hc =>
        val ee: EitherError[skuber.Probe] = for(
          hcc <- if(hc.port_index.isEmpty) {
            Right(hc)
          }else {
            val portIndex = hc.port_index.get
            val portOptOpt = specProperties.port_mappings.lift(portIndex) map { pm =>
              pm.service_port orElse pm.container_port
            }
            portOptOpt match {
              case None => Left(Error.UnprocessableEntity(s"HealthCheck port_index '${portIndex}' was out of bounds for the provided port mappings array"))
              case Some(None) => Left(Error.UnprocessableEntity(s"HealthCheck port_index '${portIndex}' referred to port mapping without either container_port or service_port."))
              case Some(Some(port)) => {
                Right(hc.copy(
                  port_index = None,
                  port = Some(port)
                ))
              }
            }
          };
          probe <- hcc.copy(protocol = hcc.protocol.toUpperCase()) match {
            // max_consecutive_failures and interval_seconds are not used; not sure if there is a good reason for this
            // see https://kubernetes.io/docs/tasks/configure-pod-container/configure-liveness-readiness-probes/#define-a-liveness-command
            case ContainerSpec.HealthCheck("HTTP" | "HTTPS", maybePath, _, gracePeriod, _, timeoutSeconds, _, None, Some(portNumber)) => {
              Right(skuber.Probe(
                action = skuber.HTTPGetAction(
                  port = Left(portNumber),
                  host = "",
                  path = maybePath getOrElse "",
                  schema = hcc.protocol.toString.toUpperCase()
                ),
                initialDelaySeconds = gracePeriod,
                timeoutSeconds = timeoutSeconds
              ))
            }
            case ContainerSpec.HealthCheck("TCP", _, _, gracePeriod, _, timeoutSeconds, _, None, Some(portNumber)) => {
              Right(skuber.Probe(
                action = skuber.TCPSocketAction(
                  port = Left(portNumber)
                ),
                initialDelaySeconds = gracePeriod,
                timeoutSeconds = timeoutSeconds
              ))
            }
            case ContainerSpec.HealthCheck("COMMAND", _, Some(cmd), gracePeriod, _, timeoutSeconds, _, _, _) => {
              Right(skuber.Probe(
                action = skuber.ExecAction(List("/bin/sh") ++ cmd.split(" ").toList),
                initialDelaySeconds = gracePeriod,
                timeoutSeconds = timeoutSeconds
              ))
            }
            case _ => Left(Error.UnprocessableEntity("Container health check was not well-formed"))
          }
        ) yield probe
        ee
      };
      volumesAndMounts <- specProperties.volumes.toList traverse { mount =>
        val ee: EitherError[(skuber.Volume, skuber.Volume.Mount)] = mount match {
          case ex: ExistingVolumeMountSpec => {
            val label = s"volume-ex-${ex.volume_id}"    // format of this label is important
            Right(skuber.Volume(label, skuber.Volume.PersistentVolumeClaimRef(s"!${ex.volume_id}")) -> Volume.Mount(label, ex.mount_path))
          }
          case in: InlineVolumeMountSpec => {
            import controllers.util.JsonInput
            val ji = new JsonInput {}

            val resource = ji.inputToInstance(UUID.fromString("00000000-0000-0000-0000-000000000000"), in.volume_resource)
            for(
              volumeProperties <- ResourceSerde.deserialize[VolumeSpec,Error.UnprocessableEntity](resource);
              label = s"volume-in-${volumeProperties.name}";    // format of this label is important
              vm <- volumeProperties.`type` match {
                case VolumeSpec.EmptyDir => Right(skuber.Volume(label, skuber.Volume.EmptyDir()) -> Volume.Mount(label, in.mount_path))
                case any => Left(Error.UnprocessableEntity(s"volume type $any is currently not supported for InlineVolumeMountSpec"))
              }
            ) yield vm
          }
        }
        ee
      };
      (storageVolumes, storageMounts) = volumesAndMounts.unzip;
      _ <- if(specProperties.secrets.nonEmpty && specProperties.secrets.map(_.path).distinct.size != specProperties.secrets.size) {
        Left(Error.UnprocessableEntity("secrets must have unique paths"))
      }else {
        Right(())
      };
      cpuConstraints = (providerProperties.config.cpu_requirement_type map { r =>
        r -> Map[String,Resource.Quantity](skuber.Resource.cpu -> f"${specProperties.cpus}%1.3f")
      }).toMap;
      memoryConstraints = (providerProperties.config.memory_requirement_type map { r =>
        r -> Map[String,Resource.Quantity](skuber.Resource.memory -> f"${specProperties.memory}%1.3fM")
      }).toMap;
      resourceRequirements = skuber.Resource.Requirements(
        requests = (cpuConstraints.getOrElse(KubernetesProviderProperties.Request, Map()) ++ memoryConstraints.getOrElse(KubernetesProviderProperties.Request, Map())),
        limits = (cpuConstraints.getOrElse(KubernetesProviderProperties.Limit, Map()) ++ memoryConstraints.getOrElse(KubernetesProviderProperties.Limit, Map()))
      );
      pullPolicy = if(specProperties.force_pull) {
        Container.PullPolicy.Always
      } else {
        Container.PullPolicy.IfNotPresent
      };
      commands = (specProperties.cmd map CommandParser.translate).getOrElse(Nil);
      envSecrets = specProperties.secrets.collect {
        case sem: ContainerSpec.SecretEnvMount =>
         skuber.EnvVar(sem.path, EnvVar.SecretKeyRef(sem.secret_key, s"!${sem.secret_id}"))     // substitute with actual secret name
      };
      environmentVars = List(
        skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
      ) ++ specProperties.env.map { case (k,v) => EnvVar(k, EnvVar.StringValue(v)) }
    ) yield {
      // Directory-mounted secrets: each needs a unique volume name
      val dirSecrets: Seq[(String, SecretDirMount)] = specProperties.secrets.zipWithIndex.collect {
        // case dir: SecretDirMount  => UUID.randomUUID.toString -> dir
        case (dir: SecretDirMount, i)  => s"dir-$i" -> dir
      }
      val secDirMounts: Seq[Volume.Mount] = dirSecrets.map {
        case (secVolName,vsm) => Volume.Mount(secVolName,vsm.path, true)
      }
      val secretDirVolumes: Seq[Volume] = dirSecrets.collect {
        // case (secretVolumeName, ContainerSpec.SecretDirMount(secret_id, path)) => Volume(secretVolumeName, Volume.Secret(allEnvSecrets(secret_id).name))
        case (secretVolumeName, ContainerSpec.SecretDirMount(secret_id, path)) =>
         Volume(secretVolumeName, Volume.Secret(s"!${secret_id}"))     // substitute with actual secret name
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
      val fileSecretsBySecretId = specProperties.secrets.collect {
        case file: SecretFileMount => file
      } groupBy {_.secret_id}
      val fileSecretVolumeNames: Map[UUID, String] = fileSecretsBySecretId.zipWithIndex map { case ((sid, _), i) =>
        // sid -> UUID.randomUUID().toString
        sid -> s"file-$i"
      }
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
            // secretName = allEnvSecrets(secretId).name,
            secretName = s"!${secretId}",    // substitute with actual secret name
            items = Some(fileMounts map {
              sfm => Volume.KeyToPath(
                key = sfm.secret_key,
                path = sfm.path.stripPrefix(fileSecretMountPaths(secretId)).stripPrefix("/")
              )
            } toList)
          )
        )
      }

      val container = skuber.Container(
        name = specProperties.name,
        image = specProperties.image,
        resources = Some(resourceRequirements),
        env = environmentVars ++ envSecrets,
        ports = portMappings,
        imagePullPolicy = pullPolicy,
        args = specProperties.args.getOrElse(Seq.empty).toList,
        command = commands,
        livenessProbe = probes.headOption,
        volumeMounts = (storageMounts ++ secDirMounts ++ secretFileMounts).toList
      )

      val pod = Pod.Spec(
        containers = List(container),
        affinity = providerProperties.affinity,
        volumes = (storageVolumes ++ secretDirVolumes ++ secretFileVolumes).toList,
        dnsPolicy = skuber.DNSPolicy.ClusterFirst
      )
      Pod.Template.Spec(spec = Some(pod))
    }
  }

  private def mkPort(pm: ContainerSpec.PortMapping): EitherError[Service.Port] = {
    for(
      cp <- eitherFrom[Error.UnprocessableEntity].option(pm.container_port, "port mapping must contain container_port");
      protocol <- mkTransportProtocol(pm.protocol)
    ) yield {
      Service.Port(
        name = pm.name.getOrElse(""),
        protocol = protocol,
        port = pm.lb_port.filter(_ != 0).getOrElse(cp),
        targetPort = Some(Left(cp)),
        nodePort = pm.service_port.getOrElse(0)
      )
    }
  }

  def mkClusterIpServiceSpec(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[Option[Service.Spec]] = {
    for(
      ports <- specProperties.port_mappings.filter(_.expose_endpoint == Some(true))
       .map(_.copy(service_port = None)).toList.traverse(mkPort)
    ) yield {
      if(ports.size > 0) {
        Some(Service.Spec(
          ports = ports,
          _type = Service.Type.ClusterIP
        ))
      }else {
        None
      }
    }
  }

  def mkNodePortServiceSpec(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[Option[Service.Spec]] = {
    for(
      ports <- specProperties.port_mappings.filter(pm => pm.expose_endpoint == Some(true) &&
       (pm.`type` == Some("external") || pm.`type` == Some("loadBalancer"))).toList.traverse(mkPort)
    ) yield {
      if(ports.size > 0) {
        Some(Service.Spec(
          ports = ports,
          _type = Service.Type.NodePort
        ))
      }else {
        None
      }
    }
  }

  def mkLoadBalancerServiceSpec(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[Option[Service.Spec]] = {
    for(
      ports <- specProperties.port_mappings.filter(pm => pm.expose_endpoint == Some(true) && pm.`type` == Some("loadBalancer"))
       .map(_.copy(service_port = None)).toList.traverse(mkPort)
    ) yield {
      if(ports.size > 0) {
        Some(Service.Spec(
          ports = ports,
          _type = Service.Type.LoadBalancer
        ))
      }else {
        None
      }
    }
  }

  // def mkIngressSpec(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[Option[Ingress.Spec]] = {
  // }

  def mkLabels(context: ProviderContext): Map[String,String] = {
    Map(
      META_ENVIRONMENT_KEY -> context.environmentId.toString,
      META_WORKSPACE_KEY -> context.workspace.id.toString,
      META_FQON_KEY -> context.fqon,
      META_PROVIDER_KEY -> context.providerId.toString
    )
  }

  def stringToProtocol(proto: String): skuber.Protocol.Value = proto.toUpperCase match {
    case "TCP" => skuber.Protocol.TCP
    case "UDP" => skuber.Protocol.UDP
    case _ => throw new UnprocessableEntityException("port mapping must specify \"TCP\" or \"UDP\" as protocol")
  }

  def mkSecret(id: UUID, secret: SecretSpec, namespace: String, context: ProviderContext): Secret = {
    val metadata = ObjectMeta(
      name = secret.name,
      namespace = namespace,
      labels = Map(
        META_SECRET_KEY      -> id.toString
      ) ++ mkLabels(context)
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

  def mkIngressSpec(containerId: UUID, containerSpec: ContainerSpec, namespace: String, context: ProviderContext): Option[Ingress] = {
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
          META_CONTAINER_KEY -> containerId.toString
        ) ++ mkLabels(context)
      ))
    ){
      case (acc, (vhost,port)) => acc.addHttpRule(
        vhost, Map("" -> s"$svcName:$port")
      )
    })
  }

  def mkServiceSpecs(containerId: UUID, containerSpec: ContainerSpec, namespace: String, context: ProviderContext): ContainerServices = {

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
              META_CONTAINER_KEY -> containerId.toString
            ) ++ mkLabels(context))
        ) {
          case (svc, pm) =>
            val cp = pm.container_port.getOrElse(throw new UnprocessableEntityException("port mapping must contain container_port"))
            svc.exposeOnPort(Service.Port(
              name = pm.name.getOrElse(""),
              protocol = stringToProtocol(pm.protocol),
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

  def mkPortMappingsSpec(pms: Seq[PortMapping]): Seq[Port] = {
    pms.map(pm => skuber.Container.Port(
      containerPort = pm.container_port.getOrElse(throw new UnprocessableEntityException("port mapping must specify container_port")),
      protocol = stringToProtocol(pm.protocol),
      name = pm.name.getOrElse(
        if (pms.size > 1) throw new UnprocessableEntityException("multiple port mappings requires port names") else ""
      ),
      hostPort = pm.host_port
    ))
  }

  def mkKubernetesContainer(spec: ContainerSpec, provider: GestaltResourceInstance): skuber.Container = {
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
    ) ++ spec.env.map { case (k,v) => EnvVar(k, EnvVar.StringValue(v)) }.toList
      
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
  def mkDeploymentSpec(id: UUID, containerSpec: ContainerSpec, context: ProviderContext, namespace: String): Deployment = {

    val labels = containerSpec.labels ++ Map(
      META_CONTAINER_KEY -> id.toString
    ) ++ mkLabels(context)


    val podTemplate = {

      /*
       * If there are any volumes in the Meta ContainerSpec, convert them to kube volumeMounts
       */
      val (storageVolumes,volMounts) = containerSpec.volumes.map({
        case ex: ExistingVolumeMountSpec =>
          val r = ResourceFactory.findById(migrations.V13.VOLUME_TYPE_ID, ex.volume_id).getOrElse(
            throw BadRequestException("container spec had volume mount for non-existent volume")
          )
          val spec = VolumeSpec.fromResourceInstance(r).getOrElse(
            throw InternalErrorException(s"could not parse referenced volume resource '${r.name}' as VolumeSpec")
          )
          val eid = spec.external_id.getOrElse(
            throw InternalErrorException(s"Volume resource '${r.id}' did not have 'external_id'")
          )
          val eidExtracter = "/namespaces/([^/]+)/persistentvolumeclaims/(.*)".r
          val pvcName = eid match {
            case eidExtracter(_,name) /* TODO: need to check namespace */ => name
            case _ => throw InternalErrorException(s"Volume resource '${r.id}' 'external_id' was not well-formed")
          }
          val readOnly = (spec.access_mode == ReadOnlyMany)
          val lbl = r.name // TODO: don't need this now: UUID.randomUUID().toString
          skuber.Volume(lbl, skuber.Volume.PersistentVolumeClaimRef(pvcName, readOnly)) -> Volume.Mount(lbl, ex.mount_path, readOnly)
        case in: InlineVolumeMountSpec => //TODO: improve this implementation
          val name = in.volume_resource.name
          val mountPath = in.mount_path
          val volumeType = for {
            properties <- in.volume_resource.properties
            volumeTypeString <- properties.get("type")
            volumeTypeValue <- VolumeSpec.Type.fromString(volumeTypeString.as[String]).toOption
          } yield volumeTypeValue

          volumeType match {
            case Some(VolumeSpec.EmptyDir) => skuber.Volume(name, skuber.Volume.EmptyDir()) -> Volume.Mount(name, mountPath)
            case Some(any) => throw BadRequestException(s"volume type $any is currently not supported for InlineVolumeMountSpec")
            case None => throw BadRequestException(s"volume type is not defined for InlineVolumeMountSpec")
          }

        case _ =>
          throw BadRequestException("unsupported type of VolumeMountSpec")
      }).unzip


      /*
       * make sure the secrets all exist
       */
      val allEnvSecrets: Map[UUID, GestaltResourceInstance] = (ResourceFactory.findChildrenOfType(ResourceIds.Secret, context.environmentId) map {
        res => res.id -> res
      }).toMap

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
}