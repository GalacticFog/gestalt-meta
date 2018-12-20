package com.galacticfog.gestalt.container

import java.io.{PrintWriter, Writer}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.galacticfog.gestalt.caas.kube.{Ascii, KubeConfig}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import play.api.libs.json._
import skuber.api.client.RequestContext

import scala.concurrent.duration._
import scala.concurrent.Await
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

    val resp = (payload \ "action").asOpt[String] match {
      case Some(action) if Seq("container.import", "secret.import").contains(action) =>
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

  protected def initializeKube(provider: JsObject, namespace: String): RequestContext = {
    val cl = this.getClass.getClassLoader
    val config = ConfigFactory.load(this.getClass.getClassLoader)
    implicit val system = ActorSystem("my-system", Some(config), Some(cl), Some(global))
    implicit val materializer = ActorMaterializer()
    
    val rawKubeconfig = (provider \ "properties" \ "data").asOpt[String] getOrElse {
      throw new RuntimeException("provider did not have '.properties.data'")
    }
    val kubeconfig = if (Ascii.isBase64(rawKubeconfig)) Ascii.decode64(rawKubeconfig) else rawKubeconfig
    val c = KubeConfig.parseYaml(kubeconfig, Map.empty).setCurrentNamespace(namespace)
    skuber.api.client.init(c, ConfigFactory.load(this.getClass.getClassLoader))
  }

  private def doImport(payload: JsObject, ctx: JsObject): JsValue = {
    val provider = (payload \ "provider").asOpt[JsObject] getOrElse(throw new RuntimeException("payload did not have '.provider'"))
    log.debug("got provider config")
    // val providerConfig = (provider \ "properties" \ "config").asOpt[JsObject] getOrElse(throw new RuntimeException("provider did not have '.properties.config'"))

    val action = (payload \ "action").as[String]
    val resource = (payload \ "resource").as[JsObject]
    val inputProps = (resource \ "properties").asOpt[JsObject].getOrElse(Json.obj())
    val importTarget = (resource \ "properties" \ "external_id").as[String]
    
    val envId = (payload \ "context" \ "environment" \ "id").as[String]

    val fResp = (action, importTarget.split("/")) match {
      case ("container.import", Array("", "namespaces", namespaceValue, "deployments", deplNameValue)) => {
        log.debug(s"namespace: $namespaceValue, deployment: $deplNameValue")

        val kube = initializeKube(provider, namespaceValue)

        for {
          depl <- kube.getInNamespace[skuber.ext.Deployment](deplNameValue, namespaceValue)
          updatedDepl <- kube.jsonMergePatch(depl, s"""{"metadata":{"labels":{"meta/environment":"${envId}"}}}""");
          importProps = deploymentToContainerSpec(updatedDepl)
        } yield resource ++ Json.obj(
          "properties" -> (inputProps ++ importProps)
        )
      }
      case ("secret.import", Array("", "namespaces", namespaceValue, "secrets", secretName)) => {
        log.debug(s"namespace: $namespaceValue, secret: $secretName")

        val kube = initializeKube(provider, namespaceValue)
        
        for {
          secret <- kube.getInNamespace[skuber.Secret](secretName, namespaceValue)
          updatedSecret <- kube.jsonMergePatch(secret, s"""{"metadata":{"labels":{"meta/environment":"${envId}"}}}""");
          importProps = secretToSecretSpec(updatedSecret)
        } yield resource ++ Json.obj(
          "properties" -> (inputProps ++ importProps)
        )
      }
      case _ => throw BadRequestException(s"Invalid combination of action and External ID: (`${action}`, `${importTarget}`)")
    }

    Await.result(fResp, 15.seconds)
  }

  private def deploymentToContainerSpec(depl: skuber.ext.Deployment): JsObject = {
    val containerSpec = depl.getPodSpec.map(_.containers) match {
      case Some(List(single)) => single
      case None => throw new RuntimeException("Kubernetes deployment did not have a Pod spec")
      case Some(_) => throw new RuntimeException("Kubernetes container.import currently only supports Deployments with a single container spec")
    }

    val portMappings = containerSpec.ports.map({
      kp => Json.obj(
        "container_port" -> kp.containerPort,
        "service_port" -> kp.hostPort,
        "name" -> kp.name,
        "virtual_hosts" -> Seq.empty[String],
        "protocol" -> (kp.protocol match {
          case skuber.Protocol.TCP => "tcp"
          case skuber.Protocol.UDP => "udp"
        }),
        "expose_endpoint" -> false // ...for now
//      "service_port" -> (pm \ "servicePort").asOpt[Int],
//      "lb_port" -> vipHostPort.map(_._2),
//      "expose_endpoint" -> vipHostPort.isDefined,
//      "service_address" -> vipHostPort.map {
//        case (host, port) => Json.obj(
//          "protocol" -> (pm \ "protocol").asOpt[String],
//          "host" -> host,
//          "port" -> port
//        )
//      }
      )
    })

    val cpuLimits = for(
      r <- containerSpec.resources;
      cpu <- r.limits.get(skuber.Resource.cpu)
    ) yield cpu.amount.toDouble
    val cpuRequests = for(
      r <- containerSpec.resources;
      cpu <- r.requests.get(skuber.Resource.cpu)
    ) yield cpu.amount.toDouble
    val memLimits = for(
      r <- containerSpec.resources;
      memory <- r.limits.get(skuber.Resource.memory)
    ) yield (memory.amount / 1.0e6).toDouble
    val memRequests = for(
      r <- containerSpec.resources;
      memory <- r.requests.get(skuber.Resource.memory)
    ) yield (memory.amount / 1.0e6).toDouble

    Json.obj(
      "container_type" -> "DOCKER",
      "image" -> containerSpec.image,
      "force_pull" -> (containerSpec.imagePullPolicy == skuber.Container.PullPolicy.Always),
      "cpus" -> (cpuLimits orElse cpuRequests).getOrElse[Double](0.0),
      "memory" -> (memLimits orElse memRequests).getOrElse[Double](0.0),
      "labels" -> depl.metadata.labels,
      "env" -> Json.toJson(containerSpec.env.collect({
        case skuber.EnvVar(name, skuber.EnvVar.StringValue(value)) => name -> value
      }).toMap),
      "num_instances" -> depl.spec.flatMap(_.replicas).getOrElse[Int](0),
      "port_mappings" -> portMappings
    ) ++ JsObject(
      Seq(
        Option(containerSpec.command).filter(_.nonEmpty).map(cmds => "cmd" -> JsString(cmds.mkString(" "))),
        Option(containerSpec.args).filter(_.nonEmpty).map(args => "args" -> Json.toJson(args))
      ).flatten
    )
  }

  private def secretToSecretSpec(secret: skuber.Secret): JsObject = {
    Json.obj(
      // "name" -> secret.name,
      "items" -> (secret.data map { case(key, value) =>
        Json.obj("key" -> key, "value" -> new String(value, "UTF-8"))
      })
    )
  }
}
