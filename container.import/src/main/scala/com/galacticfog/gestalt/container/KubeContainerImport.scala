package com.galacticfog.gestalt.container

import java.util.UUID
import java.io.{PrintWriter, Writer}

import akka.actor.{ActorSystem, Props, ActorRef}
import akka.stream.ActorMaterializer
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import play.api.libs.json._
import play.api.Configuration
import skuber.api.client.RequestContext
import com.galacticfog.gestalt.integrations.kubernetes.{KubeToken, KubeTokenActor}

import scala.concurrent.duration._
import scala.concurrent.{Await,Future}
import scala.util.{Failure,Success,Try}


class KubeContainerImport extends ContainerImport {
  import scala.concurrent.ExecutionContext.Implicits.global
  import skuber.json.format._
  import skuber.json.ext.format._

  val log = LoggerFactory.getLogger(this.getClass)

  val logWriter = new PrintWriter(new Writer {
    override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
      log.error(cbuf.subSequence(off, off+len).toString)
    }
    override def flush(): Unit = ()
    override def close(): Unit = ()
  })

  def run(payloadStr: String, ctxStr: String): String = {
    val payload = Try {Json.parse(payloadStr).as[JsObject]} getOrElse Json.obj()
    log.debug(s"parsed payload is: '${payload}'")
    val ctx = Json.parse(ctxStr).as[JsObject]
    log.debug(s"parsed context is: '${ctx}'")

    val supportedActions = Seq("container.import", "secret.import", "volume.import")

    val resp = (payload \ "action").asOpt[String] match {
      case Some(action) if supportedActions.contains(action) =>
        log.debug("performing container.import")
        Try {
          doImport(payload, ctx)
        } match {
          case Success(j) => j
          case Failure(err) =>
            err.printStackTrace(logWriter)
            Json.obj(
              "actionFailed" -> s"caught exception: ${err.getMessage}"
            )
        }
      case Some(other) =>
        log.debug(s"action '${other}' not supported")
        Json.obj(
          "actionFailed" -> s"action '$other' not supported"
        )
      case None =>
        log.debug("payload was bad")
        Json.obj(
          "actionFailed" -> "payload was not properly formatted"
        )
    }
    resp.toString
  }

  protected def initializeKube(provider: JsObject, namespace: String): Future[RequestContext] = {
    import scala.collection.JavaConverters._

    val cl = this.getClass.getClassLoader
    val config = ConfigFactory.load(this.getClass.getClassLoader)
    implicit val system = ActorSystem("my-system", Some(config), Some(cl), Some(global))

    val kubeTokenActor = system.actorOf(Props(new KubeTokenActor(new Configuration(config))), "kubeTokenActor")

    val kubeToken = new KubeToken {
      val actor: ActorRef = kubeTokenActor

      val whitelistedCmdPaths: Set[String] = Try(config.getStringList("skuberFactory.execWhiteList").asScala).toOption.getOrElse(List.empty[String]).toSet
      val timeout: FiniteDuration = Try(config.getLong("skuberFactory.execAuthTimeoutInSeconds")).toOption.getOrElse(30l) .seconds
    }

    implicit val materializer = ActorMaterializer()
    
    // val kubeconfig = if (Ascii.isBase64(rawKubeconfig)) Ascii.decode64(rawKubeconfig) else rawKubeconfig
    // val c = KubeConfig.parseYaml(kubeconfig, Map.empty).setCurrentNamespace(namespace)
    // skuber.api.client.init(c, ConfigFactory.load(this.getClass.getClassLoader))

    val providerId = (provider \ "id").as[UUID]
    val providerKubeconfig = (provider \ "properties" \ "data").asOpt[String]
    kubeToken.mkKubeconfig(providerId, providerKubeconfig, namespace, Some(config))
  }

  private def doImport(payload: JsObject, ctx: JsObject): JsValue = {
    val provider = (payload \ "provider").asOpt[JsObject] getOrElse(throw new RuntimeException("payload did not have '.provider'"))
    log.debug("got provider config")
    // val providerConfig = (provider \ "properties" \ "config").asOpt[JsObject] getOrElse(throw new RuntimeException("provider did not have '.properties.config'"))

    val action = (payload \ "action").as[String]
    val resource = (payload \ "resource").as[JsObject]
    val inputProps = (resource \ "properties").asOpt[JsObject].getOrElse(Json.obj())
    val importTarget = (resource \ "properties" \ "external_id").as[String]
    
    val uuid = (resource \ "id").as[String]
    val envId = (payload \ "context" \ "environment" \ "id").as[String]

    val contextLabels = Json.obj(
      "meta/environment" -> (payload \ "context" \ "environment" \ "id").as[String],
      "meta/workspace" -> (payload \ "context" \ "workspace" \ "id").as[String],
      "meta/fqon" -> "",    // what should I put here?
      "meta/provider" -> (payload \ "provider" \ "id").as[String]
    )

    def checkLabels(obj: skuber.ObjectResource, uuidLabel: String): Future[Unit] = {
      val uuid = obj.metadata.labels.get(s"meta/${uuidLabel}")
      val environment = obj.metadata.labels.get("meta/environment")
      if(uuid.isEmpty && environment.isEmpty) {
        Future.successful(())
      }else {
        Future.failed(new RuntimeException(s"Resource is already managed by gestalt: envId=${environment}; id=${uuid}"))
      }
    }

    def setLabels[T <: skuber.ObjectResource](kube: RequestContext, obj: T, uuidLabel: String)(implicit rd: skuber.ResourceDefinition[T], fmt: Format[T]): Future[T] = {
      val labels = Json.obj(
        "metadata" -> Json.obj(
          "labels" -> (Json.obj(
            s"meta/${uuidLabel}" -> uuid
          ) ++ contextLabels)
        )
      )
      kube.jsonMergePatch(obj, labels.toString)
    }

    def getMetaId(obj: skuber.ObjectResource, envId: String, uuidLabel: String): UUID = {
      val idInMeta = obj.metadata.labels.get(s"meta/${uuidLabel}").map(UUID.fromString(_)) getOrElse {
        throw new RuntimeException(s"Resource ${obj.name} in namespace ${obj.ns} has not been imported to gestalt")
      }
      val objEnvId = obj.metadata.labels.get("meta/environment").getOrElse("")
      if(envId != objEnvId) {
        throw new RuntimeException(s"Resource ${obj.name} (env. id=${objEnvId}) and resource being imported (env. id=${envId}) must belong to the same gestalt environment")
      }
      idInMeta
    }

    val fResp = (action, importTarget.split("/", 5)) match {
      case ("container.import", Array("", "namespaces", namespaceValue, "deployments", deplNameValue)) => {
        log.debug(s"namespace: $namespaceValue, deployment: $deplNameValue")

        for {
          kube <- initializeKube(provider, namespaceValue)
          depl <- kube.getInNamespace[skuber.ext.Deployment](deplNameValue, namespaceValue)
          _ <- checkLabels(depl, "container")    // so that failed imports don't set labels
          containerSpec <- depl.getPodSpec.map(_.containers) match {
            case Some(List(single)) => Future.successful(single)
            case None => Future.failed(new RuntimeException("Kubernetes deployment did not have a Pod spec"))
            case Some(_) => Future.failed(new RuntimeException("Kubernetes container.import currently only supports Deployments with a single container spec"))
          }
          volumesSpec = depl.getPodSpec.get.volumes
          portMappings = containerSpec.ports map { kp =>
            Json.obj(
              "container_port" -> kp.containerPort,
              "service_port" -> kp.hostPort,
              "name" -> kp.name,
              "virtual_hosts" -> Seq.empty[String],
              "protocol" -> (kp.protocol match {
                case skuber.Protocol.TCP => "tcp"
                case skuber.Protocol.UDP => "udp"
              }),
              "expose_endpoint" -> false // ...for now
               // "service_port" -> (pm \ "servicePort").asOpt[Int],
               // "lb_port" -> vipHostPort.map(_._2),
               // "expose_endpoint" -> vipHostPort.isDefined,
               // "service_address" -> vipHostPort.map {
               //   case (host, port) => Json.obj(
               //     "protocol" -> (pm \ "protocol").asOpt[String],
               //     "host" -> host,
               //     "port" -> port
               //   )
               // }
            )
          }
          cpuLimits = for(
            r <- containerSpec.resources;
            cpu <- r.limits.get(skuber.Resource.cpu)
          ) yield cpu.amount.toDouble
          cpuRequests = for(
            r <- containerSpec.resources;
            cpu <- r.requests.get(skuber.Resource.cpu)
          ) yield cpu.amount.toDouble
          memLimits = for(
            r <- containerSpec.resources;
            memory <- r.limits.get(skuber.Resource.memory)
          ) yield (memory.amount / 1.0e6).toDouble
          memRequests = for(
            r <- containerSpec.resources;
            memory <- r.requests.get(skuber.Resource.memory)
          ) yield (memory.amount / 1.0e6).toDouble
          // secrets
          secretsAsEnvVars <- Future.traverse(containerSpec.env collect {
            case skuber.EnvVar(name, skuber.EnvVar.SecretKeyRef(secretKey, secretName)) => {
              (name, secretKey, secretName)
            }
          }) { case(name, secretKey, secretName) =>
            kube.getInNamespace[skuber.Secret](secretName, namespaceValue) map { secret =>
              Json.toJson(ContainerSpec.SecretEnvMount(getMetaId(secret, envId, "secret"), name, secretKey))
            }
          }
          secretVolumes = (volumesSpec collect {
            case skuber.Volume(name, skuber.Volume.Secret(secretName, _, _, _)) => (name, secretName)
          }).toMap
          secretsAsVolumes <- Future.traverse(containerSpec.volumeMounts collect { 
            case skuber.Volume.Mount(name, mountPath, readOnly, subPath, mountPropagation) if secretVolumes.contains(name) => {
              (secretVolumes(name), mountPath)
            }
          }) { case(secretName, mountPath) =>
            kube.getInNamespace[skuber.Secret](secretName, namespaceValue) map { secret =>
              Json.toJson(ContainerSpec.SecretDirMount(getMetaId(secret, envId, "secret"), mountPath))
            }
          }
          // volumes
          mounts = (containerSpec.volumeMounts map {
            case skuber.Volume.Mount(name, mountPath, _, _, _) => (name, mountPath)
          }).toMap
          inlineVolumes = volumesSpec collect {
            case skuber.Volume(name, skuber.Volume.HostPath(path, _)) => {
              Json.obj(
                "mount_path" -> mounts(name),
                "volume_resource" -> Json.obj(
                  "name" -> s"${deplNameValue}-${name}",
                  // I have no other option but to hardcode resource id here
                  "resource_type" -> "24361db7-0fc0-4be5-9973-d88d5602956f", // "Gestalt::Resource::Volume",
                  "resource_state" -> "Gestalt::Resource::State::Active",
                  "properties" -> Json.obj(
                    "type" -> "host_path",
                    // "config" -> Json.obj(),
                    // "reclamation_policy" -> "",
                    "mount_path" -> path
                  )
                )
              )
            }
          }
          pvcs <- Future.traverse(volumesSpec collect {
            case skuber.Volume(name, skuber.Volume.PersistentVolumeClaimRef(claimName, _)) => (name, claimName)
          }) { case(name, claimName) =>
            kube.getInNamespace[skuber.PersistentVolumeClaim](claimName, namespaceValue) map { pvc =>
              // Json.toJson(ContainerSpec.ExistingVolumeMountSpec(mounts(claimName), getMetaId(pvc, envId)))
              // ^^ doesn't get serialized
              Json.obj(
                "mount_path" -> mounts(name),
                "volume_id" -> getMetaId(pvc, envId, "volume")
              )
            }
          }
          importProps = Json.obj(
            "container_type" -> "DOCKER",
            "image" -> containerSpec.image,
            "force_pull" -> (containerSpec.imagePullPolicy == skuber.Container.PullPolicy.Always),
            "cpus" -> (cpuLimits orElse cpuRequests).getOrElse[Double](0.0),
            "memory" -> (memLimits orElse memRequests).getOrElse[Double](0.0),
            "volumes" -> (pvcs ++ inlineVolumes),
            "labels" -> depl.metadata.labels,
            "env" -> Json.toJson(containerSpec.env.collect({
              case skuber.EnvVar(name, skuber.EnvVar.StringValue(value)) => name -> value
            }).toMap),
            "num_instances" -> depl.spec.flatMap(_.replicas).getOrElse[Int](0),
            "port_mappings" -> portMappings,
            "secrets" -> (secretsAsEnvVars ++ secretsAsVolumes)
          ) ++ JsObject(
            Seq(
              Option(containerSpec.command).filter(_.nonEmpty).map(cmds => "cmd" -> JsString(cmds.mkString(" "))),
              Option(containerSpec.args).filter(_.nonEmpty).map(args => "args" -> Json.toJson(args))
            ).flatten
          )
          _ <- setLabels(kube, depl, "container")
        } yield resource ++ Json.obj(
          "properties" -> (inputProps ++ importProps)
        )
      }
      case ("secret.import", Array("", "namespaces", namespaceValue, "secrets", secretName)) => {
        log.debug(s"namespace: $namespaceValue, secret: $secretName")
        
        for {
          kube <- initializeKube(provider, namespaceValue)
          secret <- kube.getInNamespace[skuber.Secret](secretName, namespaceValue)
          _ <- checkLabels(secret, "secret")    // so that failed imports don't set labels
          importProps = Json.obj(
            // "name" -> secret.name,
            "items" -> (secret.data map { case(key, value) =>
              Json.obj("key" -> key, "value" -> new String(value, "UTF-8"))
            })
          )
          _ <- setLabels(kube, secret, "secret")
        } yield resource ++ Json.obj(
          "properties" -> (inputProps ++ importProps)
        )
      }
      case ("volume.import", Array("", "namespaces", namespaceValue, "persistentvolumeclaims", pvcName)) => {
        log.debug(s"namespace: $namespaceValue, pvc: $pvcName")
        
        for {
          kube <- initializeKube(provider, namespaceValue)
          pvc <- kube.getInNamespace[skuber.PersistentVolumeClaim](pvcName, namespaceValue)
          _ <- checkLabels(pvc, "volume")    // so that failed imports don't set labels
          pvcSpec = pvc.spec.get
          _ = log.debug(s"getting pv ${pvcSpec.volumeName}")
          pv <- kube.getInNamespace[skuber.PersistentVolume](pvcSpec.volumeName, namespaceValue)
          _ = log.debug(s"got pv ${pvcSpec.volumeName}")
          pvSpec = pv.spec.get
          pvType = pvSpec.source match {
            // not sure if this is the correct correspondence
            case _: skuber.Volume.AWSElasticBlockStore => "external"
            case _: skuber.Volume.GCEPersistentDisk => "persistent"
            case _: skuber.Volume.Glusterfs => "external"
            case _: skuber.Volume.HostPath => "host_path"
            case _: skuber.Volume.ISCSI => "external"
            case _: skuber.Volume.NFS => "external"
            case _: skuber.Volume.RBD => "external"
            case _: skuber.Volume.GenericVolumeSource => "external"
          }
          size = pvSpec.capacity.get("storage").map(_.amount.toInt / 1024 / 1024).getOrElse(0)
          accessMode: String = (pvcSpec.accessModes.headOption orElse pvSpec.accessModes.headOption).map(_.toString).getOrElse("")
          importProps = Json.obj(
            "type" -> pvType,
            "config" -> Json.obj(),     // what should I put in here?
            "size" -> size,
            "access_mode" -> accessMode
          )
          _ <- setLabels(kube, pvc, "volume")
        } yield resource ++ Json.obj(
          "properties" -> (inputProps ++ importProps)
        )
      }
      case _ => throw BadRequestException(s"Invalid combination of action and External ID: (`${action}`, `${importTarget}`)")
    }

    Await.result(fResp, 15.seconds)
  }
}
