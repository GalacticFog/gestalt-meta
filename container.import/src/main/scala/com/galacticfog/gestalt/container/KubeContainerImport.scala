package com.galacticfog.gestalt.container

import java.io.{PrintWriter, Writer}

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.galacticfog.gestalt.caas.kube.{Ascii, KubeConfig}
import com.typesafe.config.ConfigFactory
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsObject, JsString, Json}
import skuber.api.client.RequestContext

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}


class KubeContainerImport extends ContainerImport {

  implicit class Regex(sc: StringContext) {
    def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
  }

  val logger = LoggerFactory.getLogger(this.getClass)

  val cl = this.getClass.getClassLoader
  val config = ConfigFactory.load(this.getClass.getClassLoader)
  implicit val system = ActorSystem("my-system", Some(config), Some(cl), Some(global))
  implicit val materializer = ActorMaterializer()

  private[this] val logWriter = new PrintWriter(new Writer {
    override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
      logger.error(cbuf.subSequence(off, off+len).toString)
    }
    override def flush(): Unit = ()
    override def close(): Unit = ()
  })
  private[this] def log(s: String) = logger.debug(s)

  def run(payloadStr: String, ctxStr: String): String = {
    val payload = Try {Json.parse(payloadStr).as[JsObject]} getOrElse Json.obj()
    log(s"parsed payload is: '${payload}'")
    val ctx = Json.parse(ctxStr).as[JsObject]
    log(s"parsed context is: '${ctx}'")

    val resp = (payload \ "action").asOpt[String] match {
      case Some("container.import") =>
        log("performing container.import")
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
        log(s"action '${other}' not supported")
        Json.obj(
          "actionFailed" -> s"action '$other' not supported"
        )
      case None =>
        log("payload was bad")
        Json.obj(
          "actionFailed" -> "payload was not properly formatted"
        )
    }
    resp.toString
  }

  private[this] def doImport(payload: JsObject, ctx: JsObject): JsObject = {
    val provider = (payload \ "provider").asOpt[JsObject] getOrElse(throw new RuntimeException("payload did not have '.provider'"))
    log("got provider config")
    // val providerConfig = (provider \ "properties" \ "config").asOpt[JsObject] getOrElse(throw new RuntimeException("provider did not have '.properties.config'"))

    val resource = (payload \ "resource").asOpt[JsObject] getOrElse(throw new RuntimeException("payload did not have meta resource"))
    val inputProps = (resource \ "properties").asOpt[JsObject].getOrElse(Json.obj())
    val importTarget = (resource \ "properties" \ "external_id").asOpt[String] getOrElse(throw new RuntimeException("resource did not have 'external_id'"))

    val (namespace,deplName) = importTarget match {
      case r"/namespaces/([^/]+)${namespace}/deployments/(.*)${deployment}" => (namespace,deployment)
      case _ => throw new RuntimeException(s"resource '.properties.external_id' did not match expected pattern, was '${importTarget}'")
    }
    val kube = initializeKube(provider, namespace).get

    val fResp = for {
      depl <- getSkuberResource(kube, deplName)
      importProps = convertAppToInstanceProps(depl)
    } yield resource ++ Json.obj(
      "properties" -> (inputProps ++ importProps)
    )
    Await.result(fResp, 15.seconds)
  }

  private[this] def getSkuberResource(kube: RequestContext, deplName: String): Future[skuber.ext.Deployment] = {
    import skuber.json.ext.format.depFormat
    kube.getOption[skuber.ext.Deployment](deplName) flatMap {
      case Some(d) => Future.successful(d)
      case None => Future.failed(new RuntimeException(s"could not located deployment with name '${deplName}'"))
    }
  }

  private[this] def convertAppToInstanceProps(depl: skuber.ext.Deployment): JsObject = {
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

    val cpu =
      containerSpec.resources.flatMap(_.limits.get(skuber.Resource.cpu)) orElse
      containerSpec.resources.flatMap(_.requests.get(skuber.Resource.cpu))
    val mem =
      containerSpec.resources.flatMap(_.limits.get(skuber.Resource.memory)) orElse
        containerSpec.resources.flatMap(_.requests.get(skuber.Resource.memory))

    Json.obj(
      "container_type" -> "DOCKER",
      "image" -> containerSpec.image,
      "force_pull" -> (containerSpec.imagePullPolicy == skuber.Container.PullPolicy.Always),
      "cpus" -> cpu.flatMap(q => Try(q.amount.toDouble).toOption).getOrElse[Double](0.0),
      "memory" -> mem.flatMap(q => Try((q.amount / 1.0e6).toDouble).toOption).getOrElse[Double](0.0),
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
    ).as[JsObject]
  }

  private[this] def initializeKube( provider: JsObject, namespace: String )
                                  ( implicit ec: ExecutionContext ): Try[RequestContext] = for {
    config  <- extractKubeConfig(provider)
    context <- Try {
      val c = KubeConfig.parseYaml(config, Map.empty).setCurrentNamespace(namespace)
      skuber.api.client.init(c, ConfigFactory.load(this.getClass.getClassLoader))
    }
  } yield context

  private[this] def extractKubeConfig(provider: JsObject): Try[String] = {
    for {
      data <- (provider \ "properties" \ "data").asOpt[String] match {
        case Some(d) => Success(d)
        case None => Failure(new RuntimeException("provider did not have '.properties.data'"))
      }
      decoded = if (Ascii.isBase64(data)) Ascii.decode64(data) else data
    } yield decoded
  }

}
