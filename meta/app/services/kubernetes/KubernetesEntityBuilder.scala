package services.kubernetes

import java.util.{Base64, UUID}
import scala.annotation.tailrec
import scala.util.Try
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.list._
import cats.instances.either._
import com.galacticfog.gestalt.meta.api.{ContainerSpec, SecretSpec, VolumeSpec}
import com.galacticfog.gestalt.util.ResourceSerde
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.Error
import services.util.CommandParser

/**
Transforms meta entities into kubernetes entities:
(KubernetesProviderProperties.Properties, META_SPEC) => EitherError[SKUBER_SPEC]
*/
trait KubernetesEntityBuilder {
  import skuber.json.format._

  private def mkTransportProtocol(protocol: String): EitherError[skuber.Protocol.Protocol] = {
    protocol.toUpperCase match {
      case "TCP" => Right(skuber.Protocol.TCP)
      case "UDP" => Right(skuber.Protocol.UDP)
      case _ => Left(Error.UnprocessableEntity("port mapping must specify \"TCP\" or \"UDP\" as protocol"))
    }
  }

  def mkPodTemplate(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[skuber.Pod.Template.Spec] = {
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
          case ex: ContainerSpec.ExistingVolumeMountSpec => {
            val label = s"volume-ex-${ex.volume_id}"    // format of this label is important
            Right(skuber.Volume(label, skuber.Volume.PersistentVolumeClaimRef(s"!${ex.volume_id}")) -> skuber.Volume.Mount(label, ex.mount_path))
          }
          case in: ContainerSpec.InlineVolumeMountSpec => {
            import controllers.util.JsonInput
            val ji = new JsonInput {}

            for(
              resource <- eitherFromTry(Try(ji.inputToInstance(UUID.fromString("00000000-0000-0000-0000-000000000000"), in.volume_resource)));
              volumeProperties <- ResourceSerde.deserialize[VolumeSpec,Error.UnprocessableEntity](resource);
              label = s"volume-in-${volumeProperties.name}";    // format of this label is important
              vm <- volumeProperties.`type` match {
                case VolumeSpec.EmptyDir => Right(skuber.Volume(label, skuber.Volume.EmptyDir()) -> skuber.Volume.Mount(label, in.mount_path))
                case any => Left(Error.UnprocessableEntity(s"volume type $any is currently not supported for InlineVolumeMountSpec"))
              }
            ) yield vm
          }
        }
        ee
      };
      (storageVolumes, storageMounts) = volumesAndMounts.unzip;
      _ <- if(specProperties.secrets.nonEmpty && specProperties.secrets.map(_.path).distinct.size != specProperties.secrets.size) {
        Left(Error.BadRequest("secrets must have unique paths"))
      }else {
        Right(())
      };
      cpuConstraints = (providerProperties.config.cpu_requirement_type map { r =>
        r -> Map[String,skuber.Resource.Quantity](skuber.Resource.cpu -> f"${specProperties.cpus}%1.3f")
      }).toMap;
      memoryConstraints = (providerProperties.config.memory_requirement_type map { r =>
        r -> Map[String,skuber.Resource.Quantity](skuber.Resource.memory -> f"${specProperties.memory}%1.3fM")
      }).toMap;
      // https://kubernetes.io/docs/tasks/manage-gpus/scheduling-gpus/
      gpuConstraints <- if(specProperties.gpu_support.enabled) {
        val knownGpuVendors = Seq("nvidia", "amd")
        if(knownGpuVendors.contains(specProperties.gpu_support.`type`)) {
          Right(Map[String,skuber.Resource.Quantity](s"${specProperties.gpu_support.`type`}.com/gpu" -> s"${specProperties.gpu_support.count}"))
        }else {
          Left(Error.BadRequest(s"""Kubernetes provider supports the following gpu vendors / types: ${knownGpuVendors.mkString(", ")}"""))
        }
      }else {
        Right(Map.empty[String,skuber.Resource.Quantity])
      };
      nodeSelector = if(gpuConstraints.isEmpty) {
        Map.empty[String,String]
      }else {
        providerProperties.config.gpu_default_node_selector
      };
      resourceRequirements = skuber.Resource.Requirements(
        requests = (cpuConstraints.getOrElse(KubernetesProviderProperties.Request, Map.empty)
         ++ memoryConstraints.getOrElse(KubernetesProviderProperties.Request, Map.empty)),
        limits = (cpuConstraints.getOrElse(KubernetesProviderProperties.Limit, Map.empty)
         ++ memoryConstraints.getOrElse(KubernetesProviderProperties.Limit, Map.empty)) ++ gpuConstraints
      );
      pullPolicy = if(specProperties.force_pull) {
        skuber.Container.PullPolicy.Always
      } else {
        skuber.Container.PullPolicy.IfNotPresent
      };
      commands = (specProperties.cmd map CommandParser.translate).getOrElse(Nil);
      envSecrets = specProperties.secrets.collect {
        case sem: ContainerSpec.SecretEnvMount =>
         skuber.EnvVar(sem.path, skuber.EnvVar.SecretKeyRef(sem.secret_key, s"!${sem.secret_id}"))     // substitute with actual secret name
      };
      environmentVars = List(
        skuber.EnvVar("POD_IP", skuber.EnvVar.FieldRef("status.podIP"))
      ) ++ specProperties.env.map { case (k,v) => skuber.EnvVar(k, skuber.EnvVar.StringValue(v)) }
    ) yield {
      // Directory-mounted secrets: each needs a unique volume name
      val dirSecrets: Seq[(String, ContainerSpec.SecretDirMount)] = specProperties.secrets.zipWithIndex.collect {
        // case dir: SecretDirMount  => UUID.randomUUID.toString -> dir
        case (dir: ContainerSpec.SecretDirMount, i)  => s"dir-$i" -> dir
      }
      val secDirMounts: Seq[skuber.Volume.Mount] = dirSecrets.map {
        case (secVolName,vsm) => skuber.Volume.Mount(secVolName,vsm.path, true)
      }
      val secretDirVolumes: Seq[skuber.Volume] = dirSecrets.collect {
        // case (secretVolumeName, ContainerSpec.SecretDirMount(secret_id, path)) => Volume(secretVolumeName, Volume.Secret(allEnvSecrets(secret_id).name))
        case (secretVolumeName, ContainerSpec.SecretDirMount(secret_id, path)) =>
         skuber.Volume(secretVolumeName, skuber.Volume.Secret(s"!${secret_id}"))     // substitute with actual secret name
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
        case file: ContainerSpec.SecretFileMount => file
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
        secretId => skuber.Volume.Mount(
          name = fileSecretVolumeNames(secretId),
          mountPath = fileSecretMountPaths(secretId),
          readOnly = true
        )
      }
      // each volume must be mounted
      val secretFileVolumes = fileSecretsBySecretId map {
        case (secretId, fileMounts) => skuber.Volume(
          name = fileSecretVolumeNames(secretId),
          source = skuber.Volume.Secret(
            // secretName = allEnvSecrets(secretId).name,
            secretName = s"!${secretId}",    // substitute with actual secret name
            items = Some(fileMounts map {
              sfm => skuber.Volume.KeyToPath(
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

      def DEFAULT_SECRET_NAME(idx: Int): String = "imagepullsecret-%d".format(idx)

      val pod = skuber.Pod.Spec(
        containers = List(container),
        affinity = providerProperties.config.affinity,
        volumes = (storageVolumes ++ secretDirVolumes ++ secretFileVolumes).toList,
        dnsPolicy = skuber.DNSPolicy.ClusterFirst,
        imagePullSecrets = List(
          skuber.LocalObjectReference(DEFAULT_SECRET_NAME(1)),
          skuber.LocalObjectReference(DEFAULT_SECRET_NAME(2)),
          skuber.LocalObjectReference(DEFAULT_SECRET_NAME(3)),
          skuber.LocalObjectReference(DEFAULT_SECRET_NAME(4)),
          skuber.LocalObjectReference(DEFAULT_SECRET_NAME(5))
        ),
        nodeSelector = nodeSelector
      )
      skuber.Pod.Template.Spec(spec = Some(pod))
    }
  }

  private def mkPort(pm: ContainerSpec.PortMapping): EitherError[skuber.Service.Port] = {
    for(
      cp <- eitherFrom[Error.UnprocessableEntity].option(pm.container_port, "port mapping must contain container_port");
      protocol <- mkTransportProtocol(pm.protocol)
    ) yield {
      skuber.Service.Port(
        name = pm.name.getOrElse(""),
        protocol = protocol,
        port = pm.lb_port.filter(_ != 0).getOrElse(cp),
        targetPort = Some(Left(cp)),
        nodePort = pm.service_port.getOrElse(0)
      )
    }
  }

  def mkClusterIpServiceSpec(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[Option[skuber.Service.Spec]] = {
    for(
      ports <- specProperties.port_mappings.filter(_.expose_endpoint == Some(true))
       .map(_.copy(service_port = None)).toList.traverse(mkPort)
    ) yield {
      if(ports.size > 0) {
        Some(skuber.Service.Spec(
          ports = ports,
          _type = skuber.Service.Type.ClusterIP
        ))
      }else {
        None
      }
    }
  }

  def mkNodePortServiceSpec(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[Option[skuber.Service.Spec]] = {
    for(
      ports <- specProperties.port_mappings.filter(pm => pm.expose_endpoint == Some(true) &&
       (pm.`type` == Some("external") || pm.`type` == Some("loadBalancer"))).toList.traverse(mkPort)
    ) yield {
      if(ports.size > 0) {
        Some(skuber.Service.Spec(
          ports = ports,
          _type = skuber.Service.Type.NodePort
        ))
      }else {
        None
      }
    }
  }

  def mkLoadBalancerServiceSpec(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[Option[skuber.Service.Spec]] = {
    for(
      _ <- if(specProperties.port_mappings.filter(_.lb_ip.isDefined).size > 1) {
        Left(Error.BadRequest("Please set no more than one lb_ip per container"))
      }else {
        Right(())
      };
      ports <- specProperties.port_mappings.filter(pm => pm.expose_endpoint == Some(true) && pm.`type` == Some("loadBalancer"))
       .map(_.copy(service_port = None)).toList.traverse(mkPort)
    ) yield {
      if(ports.size > 0) {
        Some(skuber.Service.Spec(
          ports = ports,
          _type = skuber.Service.Type.LoadBalancer,
          loadBalancerIP = specProperties.port_mappings.find(_.lb_ip.isDefined).flatMap(_.lb_ip).getOrElse("")
        ))
      }else {
        None
      }
    }
  }

  def mkIngressSpec(providerProperties: KubernetesProviderProperties.Properties, specProperties: ContainerSpec): EitherError[Option[skuber.ext.Ingress.Spec]] = {
    val ingressRules = for(
      pm <- specProperties.port_mappings.toList if pm.expose_endpoint == Some(true);
      vhost <- pm.virtual_hosts.getOrElse(Seq.empty);
      port <- pm.lb_port.orElse(pm.container_port)
    ) yield {
      val postfix = if(pm.lb_ip.isDefined) {
        "lb"
      }else {
        "ext"
      }
      skuber.ext.Ingress.Rule(vhost, skuber.ext.Ingress.HttpRule(
        List(skuber.ext.Ingress.Path("", skuber.ext.Ingress.Backend(s"${specProperties.name}-${postfix}", port)))
      ))
    }

    Right(if(ingressRules.size > 0) {
      Some(skuber.ext.Ingress.Spec(rules = ingressRules))
    }else {
      None
    })
  }

  def mkPvSpec(providerProperties: KubernetesProviderProperties.Properties, spec: VolumeSpec): EitherError[Option[skuber.PersistentVolume.Spec]] = {
    def isAllowedHostPath(hostPath: String): Boolean = {
      // val allowedHostPaths = ContainerService.getProviderProperty[Seq[String]](provider, HOST_VOLUME_WHITELIST).getOrElse(Seq.empty)
      val hpParts = hostPath.stripSuffix("/").stripPrefix("/").split("/").scan("")(_ + "/" + _)
      hpParts.exists(part => providerProperties.config.host_volume_whitelist.map(_.stripSuffix("/")).contains(part))
    }

    for(
      // this ought to be done by Reads[VolumesSpec]
      config <- eitherFromJsResult(spec.parseConfig);
      sourceOpt <- config match {
        case VolumeSpec.HostPathVolume(hostPath) if isAllowedHostPath(hostPath) => {
          Right(Some(skuber.Volume.HostPath(hostPath)))
        }
        case VolumeSpec.HostPathVolume(hostPath) => Left(Error.UnprocessableEntity(s"host_path '$hostPath' is not in provider's white-list"))
        case VolumeSpec.PersistentVolume => Left(Error.BadRequest("Kubernetes providers only support volumes of type 'external', 'dynamic' and 'host_path'"))
        case VolumeSpec.ExternalVolume(config) => Right(Some(skuber.Volume.GenericVolumeSource(config.toString)))
        case VolumeSpec.DynamicVolume(_) => Right(None)
      }
    ) yield {
      sourceOpt map { source =>
        skuber.PersistentVolume.Spec(
          capacity = Map(skuber.Resource.storage -> skuber.Resource.Quantity(s"${spec.size}Mi")),
          source = source,
          accessModes = List(spec.access_mode),
          claimRef = Some(skuber.ObjectReference(
            namespace = "__SET_ME__",
            name = spec.name
          ))
        )
      }
    }
  }

  def mkPvcSpec(providerProperties: KubernetesProviderProperties.Properties, spec: VolumeSpec): EitherError[skuber.PersistentVolumeClaim.Spec] = {
    def isConfiguredStorageClass(storageClass: String): Boolean = {
      // val configuredStorageClasses = ContainerService.getProviderProperty[Seq[String]](provider, STORAGE_CLASSES).getOrElse(Seq.empty)
      providerProperties.config.storage_classes.contains(storageClass)
    }

    for(
      // this ought to be done by Reads[VolumesSpec]
      config <- eitherFromJsResult(spec.parseConfig);
      storageClassOpt <- config match {
        case VolumeSpec.DynamicVolume(storageClass) if isConfiguredStorageClass(storageClass) => {
          Right(Some(storageClass))
        }
        case VolumeSpec.DynamicVolume(storageClass) => Left(Error.UnprocessableEntity(s"storage_class '$storageClass' is not in provider's white-list"))
        case _ => Right(None)
      }
    ) yield {
      skuber.PersistentVolumeClaim.Spec(
        accessModes = List(spec.access_mode),
        resources = Some(skuber.Resource.Requirements(
          requests = Map(
            skuber.Resource.storage -> s"${spec.size}Mi"
          )
        )),
        storageClassName = storageClassOpt
      )
    }
  }

  def mkSecret(providerProperties: KubernetesProviderProperties.Properties, specProperties: SecretSpec): EitherError[skuber.Secret] = {
    for(
      data <- specProperties.items.toList traverse {
        case SecretSpec.Item(key, maybeValue) => {
          val ee: EitherError[(String,Array[Byte])] = for(
            value <- eitherFrom[Error.BadRequest].option(maybeValue, "secret item was missing value");
            decodedValue <- eitherFromTry(Try(Base64.getDecoder.decode(value)))
          ) yield (key, decodedValue)
          ee
        }
      }
    ) yield {
      skuber.Secret(
        metadata = skuber.ObjectMeta(),
        `type` = "Opaque",
        data = data.toMap
      )
    }
  }
}