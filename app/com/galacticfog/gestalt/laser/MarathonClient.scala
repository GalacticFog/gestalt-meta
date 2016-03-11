package com.galacticfog.gestalt.laser

import org.joda.time.DateTime
import play.api.libs.ws.WSClient
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.concurrent.{ExecutionContext, Future}

case class ContainerApp(service: String,
                        containerType: String,
                        status: String,
                        cpus: Double,
                        memory: Double,
                        image: String,
                        age: DateTime,
                        numInstances: Int)

case object ContainerApp {
  implicit val appContainerFormat = Json.format[ContainerApp]
}

case class MarathonClient(client: WSClient, marathonAddress: String) {

  def listApplications(implicit ex: ExecutionContext): Future[Seq[ContainerApp]] = {
    for {
      marResp <- client.url(s"${marathonAddress}/v2/apps").get()
      appsJson = (marResp.json \ "apps").as[Seq[JsObject]]
      apps = appsJson flatMap { marApp =>
        for {
          service <- (marApp \ "id").asOpt[String]
          ctype = (marApp \ "container" \ "type").asOpt[String] getOrElse "UNKNOWN"
          image = (marApp \ "container" \ "docker" \ "image").asOpt[String] getOrElse ""
          numStaged <- (marApp \ "tasksStaged").asOpt[Int]
          numRunning <- (marApp \ "tasksRunning").asOpt[Int]
          status = if (numStaged > 0 && numRunning > 0) "SCALING" else if (numRunning > 0) "RUNNING" else if (numStaged > 0) "SCALING" else "SUSPENDED"
          cpus <- (marApp \ "cpus").asOpt[Double]
          memory  <- (marApp \ "mem").asOpt[Double]
          age <- (marApp \ "version").asOpt[String] map DateTime.parse
        } yield ContainerApp(status = status, containerType = ctype, service = service, cpus = cpus, memory = memory, image = image, age = age, numInstances = numRunning)
      }
    } yield apps
  }

}