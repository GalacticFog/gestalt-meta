package com.galacticfog.gestalt.container

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.galacticfog.gestalt.container.dcos.ACSTokenClient
import org.slf4j.LoggerFactory
// import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsObject, JsString, JsValue, Json}
import play.api.libs.ws.WSClientConfig
import play.api.libs.ws.ahc.{AhcWSClient, AhcWSClientConfig}
import play.api.libs.ws.ssl.{SSLConfig, SSLLooseConfig}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

class DcosContainerImport extends ContainerImport {

  implicit val system = ActorSystem("my-system", None, None, Some(defaultContext))
  implicit val materializer = ActorMaterializer()

  lazy val wsDefault = AhcWSClient()
  lazy val permissiveClient = {
    val config = AhcWSClientConfig(WSClientConfig(
      ssl = SSLConfig(
        loose = SSLLooseConfig(acceptAnyCertificate = true)
      )
    ))
    AhcWSClient(config)
  }
  lazy val acsTokenClient = Try{new ACSTokenClient(permissiveClient)}

  val logger = LoggerFactory.getLogger(this.getClass)

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
          case Failure(err) => Json.obj(
            "actionFailed" -> s"caught exception: ${err.getMessage}"
          )
        }
      case Some(other) =>
        log(s"action ${other} not supported")
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
    val providerConfig = (payload \ "provider" \ "properties" \ "config").asOpt[JsObject] getOrElse(throw new RuntimeException("provider did not have config"))
    log("got provider config")
    val marathonBaseUrl = (providerConfig \ "url").as[String]
    log(s"got marathon: $marathonBaseUrl")

    val resource = (payload \ "resource").asOpt[JsObject] getOrElse(throw new RuntimeException("payload did not have meta resource"))
    val inputProps = (resource \ "properties").asOpt[JsObject].getOrElse(Json.obj())
    val importTarget = (resource \ "properties" \ "external_id").asOpt[String] getOrElse(throw new RuntimeException("resource did not have 'external_id'"))

    val fAcsToken = (for {
      scheme <- (providerConfig \ "auth" \ "scheme").asOpt[String]
      if scheme == "acs"
      dcos_base_url      <- (providerConfig \ "auth" \ "dcos_base_url").asOpt[String]
      private_key        <- (providerConfig \ "auth" \ "private_key").asOpt[String]
      service_account_id <- (providerConfig \ "auth" \ "service_account_id").asOpt[String]
      _ = log("got acs credentials")
    } yield ACSTokenClient.DCOSAuthTokenRequest(
      serviceAccountId = service_account_id,
      privateKey = private_key,
      dcosUrl = dcos_base_url
    )) match {
      case None =>
        Future.successful(None)
      case Some(req) =>
        log("will attempt to get acs token")
        for {
          client <- Future.fromTry(acsTokenClient)
          _ = log("instantiated the client, requesting token now")
          token <- client.getToken(req)
        } yield Some(token)
    }

    val fResp = for {
      maybeToken <- fAcsToken
      _ = log(s"got token: $maybeToken")
      request = maybeToken.foldLeft( permissiveClient.url(marathonBaseUrl.stripSuffix("/") + "/v2/apps/" + importTarget.stripPrefix("/")) ) {
        case (req, token) => req.withHeaders("Authorization" -> s"token=$token")
      }
      resp <- request.get()
      marJson <- if (resp.status == 200) Future.successful(resp.json.as[JsObject]) else Future.failed(new RuntimeException(resp.body))
      app = (marJson \ "app").as[JsObject]
      importProps = convertAppToInstanceProps(app)
    } yield resource ++ Json.obj(
      "properties" -> (inputProps ++ importProps)
    )
    Await.result(fResp, 15 .seconds)
  }

  private[this] def convertAppToInstanceProps(app: JsObject): JsObject = {
    val network = (app \ "networks").asOpt[Seq[JsObject]].flatMap(_.headOption) flatMap {
      o => (o \ "mode").asOpt[String] match {
        case Some("container") => (o \ "name").asOpt[String]
        case Some("container/bridge") => Some("BRIDGE")
        case Some("host") => Some("HOST")
        case _ => None
      }
    }
    val portMappings = network match {
      case Some("BRIDGE") =>
        (app \ "container" \ "portMappings").asOpt[Seq[JsObject]].getOrElse(Seq.empty).zipWithIndex.map({
          case (pm, index) =>
            val vip = "^([^:]*):([0-9]+)$".r
            val vipHostPort = (pm \ "labels").asOpt[Map[String, JsValue]].flatMap(_.collectFirst({
              case (k, JsString(v)) if k.matches("VIP_([0-9]+)") => v
            })) match {
              case Some(vip(lbl, port)) =>
                val host = lbl.stripPrefix("/").replace("/","") + ".marathon.l4lb.thisdcos.directory"
                Some(host, port.toInt)
              case _ => None
            }
            val vHosts = (app \ "labels").asOpt[Map[String,String]].map(_.collect({
              case (k,v) if k == s"HAPROXY_${index}_VHOST" => v.split(",")
            }).flatten)
            Json.obj(
              "container_port" -> (pm \ "containerPort").asOpt[Int],
              "service_port" -> (pm \ "servicePort").asOpt[Int],
              "host_port" -> (pm \ "hostPort").asOpt[Int],
              "expose_endpoint" -> vipHostPort.isDefined,
              "name" -> (pm \ "name").asOpt[String],
              "protocol" -> (pm \ "protocol").asOpt[String],
              "virtual_hosts" -> vHosts,
              "lb_port" -> vipHostPort.map(_._2),
              "service_address" -> vipHostPort.map {
                case (host, port) => Json.obj(
                  "protocol" -> (pm \ "protocol").asOpt[String],
                  "host" -> host,
                  "port" -> port
                )
              }
            )
        })
      case _ => Seq.empty
    }

    Json.obj(
      "external_id" -> (app \ "id").as[String],
      "container_type" -> "DOCKER",
      "image" -> (app \ "container" \ "docker" \ "image").asOpt[String],
      "cmd" -> (app \ "cmd").asOpt[String],
      "cpus" -> (app \ "cpus").asOpt[Double],
      "memory" -> (app \ "mem").asOpt[Double],
      "age" -> (app \ "version").asOpt[String],
      "labels" -> (app \ "labels").asOpt[Map[String,String]].map(_.collect({
        case (k, v) if !k.matches("HAPROXY_([0-9]+)_VHOST") => k -> v
      })),
      "env" -> (app \ "env").asOpt[JsObject].getOrElse[JsObject](Json.obj()),
      "force_pull" -> (app \ "container" \ "docker" \ "forcePullImage").asOpt[Boolean].getOrElse[Boolean](false),
      "num_instances" -> (app \ "instances").asOpt[Int],
      "network" -> network,
      "port_mappings" -> portMappings
    )
  }

}
