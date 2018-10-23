package com.galacticfog.gestalt.kong

import scala.concurrent.Await
import scala.concurrent.duration._
import org.slf4j.LoggerFactory
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import play.api.libs.json._
import play.api.libs.ws.ahc.AhcWSClient
import play.api.libs.concurrent.Execution.Implicits.defaultContext

// {"TaskArn":"arn:aws:ecs:us-east-1:245814043176:task/8bdacde3-1458-4c9e-9cd9-6973d9c8f1f2",
// "TaskGroup":"service:5ff7072e-e719-47a6-b09e-184ecb51047e-nginx","InstanceId":"i-05c4c5da3dcc93287",
// "PrivateIpAddress":"10.0.0.158","NetworkBindings":[{"ContainerPort":80,"Protocol":"tcp","HostPort":32771}]}

case class NetworkBinding(
  ContainerPort: Int,
  Protocol: String,
  HostPort: Int
)

case class TaskMetadata(
  TaskArn: String,
  TaskGroup: String,
  InstanceId: String,
  PrivateIpAddress: String,
  ManagementUrl: String,
  NetworkBindings: Seq[NetworkBinding]
)

class ConfigureForEcs() {
  val log = LoggerFactory.getLogger(this.getClass)

  implicit val networkBindingReads = Json.reads[NetworkBinding]
  implicit val taskMetadataReads = Json.reads[TaskMetadata]

  // val managementUrl = "http://api1-mgmt.cluster1.galacticfog.com"

  implicit val system = ActorSystem("my-system", None, None, Some(defaultContext))
  implicit val materializer = ActorMaterializer()

  val ws = AhcWSClient()

  def configureFresh(metadata: TaskMetadata, networkBinding: NetworkBinding, api: JsObject): Unit = {
    val id = (api \ "id").as[String]
    val newApi = api ++ Json.obj(
      "upstream_url" -> s"http://${metadata.PrivateIpAddress}:${networkBinding.HostPort}",
      "name" -> s"${metadata.TaskGroup.replace(":", "_")}_${networkBinding.ContainerPort}"
    )
    val future = ws.url(s"${metadata.ManagementUrl}/apis/${id}").patch(newApi)
    val res = Await.result(future, 2 .minutes)
    log.debug(s"response: ${res.body}")
    ()
  }

  def configureExisting(metadata: TaskMetadata, networkBinding: NetworkBinding, api: JsObject): Unit = {
    val id = (api \ "id").as[String]
    val newApi = api ++ Json.obj(
      "upstream_url" -> s"http://${metadata.PrivateIpAddress}:${networkBinding.HostPort}"
    )
    val future = ws.url(s"${metadata.ManagementUrl}/apis/${id}").patch(newApi)
    val res = Await.result(future, 2 .minutes)
    log.debug(s"response: ${res.body}")
    ()
  }

  def run(payloadStr: String, ctxStr: String): String = {
    val parsed = Json.parse(payloadStr)
    val metadata = parsed.as[TaskMetadata]

    val future = ws.url(s"${metadata.ManagementUrl}/apis").get()
    val resp = Await.result(future, 2 .minutes)
    val apis = (resp.json \ "data").as[Seq[JsObject]]

    log.debug(s"metadata: $metadata")
    // log.debug(s"apis: $apis")

    metadata.NetworkBindings foreach { binding =>
      val freshApis = apis filter { api =>
        val upstreamUrl = (api \ "upstream_url").as[String]
        upstreamUrl.endsWith(s"${metadata.TaskGroup.replace("service:", "service/")}:${binding.ContainerPort}")
      }

      log.debug(s"freshApis for ${binding}: ${freshApis}")

      val existingApis = apis filter { api =>
        val name = (api \ "name").asOpt[String]
        val upstreamUrl = (api \ "upstream_url").as[String]
        val matchingName = name == Some(s"""${metadata.TaskGroup.replace(":", "_")}_${binding.ContainerPort}""")
        val matchingUpstream = upstreamUrl == s"http://${metadata.PrivateIpAddress}:${binding.HostPort}"
        matchingName && !matchingUpstream
      }

      log.debug(s"existingApis for ${binding}: ${existingApis}")

      freshApis.foreach(configureFresh(metadata, binding, _))

      existingApis.foreach(configureExisting(metadata, binding, _))
    }

    // ws.close()
    // system.terminate()
    ""
  }
}