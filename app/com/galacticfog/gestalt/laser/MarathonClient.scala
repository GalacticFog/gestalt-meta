package com.galacticfog.gestalt.laser

import org.joda.time.DateTime
import play.api.Logger
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
      apps = appsJson flatMap MarathonClient.marathon2Container
    } yield apps
  }

  def listApplicationsInEnvironment(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[Seq[ContainerApp]] = {
    val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    // v0.16.0 and later will have the expansion on /v2/groups to get the counts for group.apps, but 0.15.x doesn't have it
    // therefore, to get these, we have to loop over all apps :(
    // val appGroup = client.url(s"${marathonAddress}/v2/groups/${groupId}?embed=group.apps.counts").get()
    val allApps = client.url(s"${marathonAddress}/v2/apps").get()
    allApps map { marResp =>
      marResp.status match {
        case 200 =>
          val marApps = (marResp.json \ "apps").as[Seq[JsObject]]
          val envApps = marApps.flatMap{MarathonClient.filterXformAppsByGroup(groupId) _}
          val capps = envApps.flatMap(MarathonClient.marathon2Container)
          capps
        case _ => throw new RuntimeException("unexpected return from Marathon REST API")
      }
    }
  }

  def listApplicationsInEnvironment_marathon_v2(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    // v0.16.0 and later will have the expansion on /v2/groups to get the counts for group.apps, but 0.15.x doesn't have it
    // therefore, to get these, we have to loop over all apps :(
    // val appGroup = client.url(s"${marathonAddress}/v2/groups/${groupId}?embed=group.apps.counts").get()
    val allApps = client.url(s"${marathonAddress}/v2/apps").get()
    allApps map { marResp =>
      marResp.status match {
        case 404 => Json.obj("apps" -> Json.arr())
        case 200 =>
          val envApps = (marResp.json \ "apps").as[Seq[JsObject]].flatMap(MarathonClient.filterXformAppsByGroup(groupId))
          Json.obj(
            "apps" -> envApps
          )
        case _ => throw new RuntimeException("unepected return from Marathon REST API")
      }
    }
  }

  def launchContainer(fqon: String, wrkName: String, envName: String, marPayload: JsObject)(implicit ex: ExecutionContext): Future[String] = {
    val appGroupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    val idTransformer = (__ \ 'id).json.update(
      __.read[JsString].map{ o => JsString(appGroupId + "/" + o.value) }
    )
    marPayload.transform(idTransformer) match {
      case e: JsError => Future.failed(new RuntimeException("error extracting and transforming app id"))
      case JsSuccess(newPayload,_) =>
        Logger.info(s"new payload:\n${Json.prettyPrint(newPayload)}")
        client.url(s"${marathonAddress}/v2/apps").post(newPayload) map { marResp =>
          Logger.info(marResp.statusText + "\n" + marResp.body)
          marResp.status match {
            case 201 => (marResp.json \ "id").as[String]
            case _ => throw new RuntimeException((marResp.json \ "message").asOpt[String] getOrElse marResp.statusText)
          }
        }
    }
  }

  def launchContainer_marathon_v2(fqon: String, wrkName: String, envName: String, marPayload: JsObject)(implicit ex: ExecutionContext): Future[JsValue] = {
    val appGroupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    val idTransformer = (__ \ 'id).json.update(
      __.read[JsString].map{ o => JsString(appGroupId + "/" + o.value) }
    )
    marPayload.transform(idTransformer) match {
      case e: JsError => Future.failed(new RuntimeException("error extracting and transforming app id"))
      case JsSuccess(newPayload,_) =>
        Logger.info(s"new payload:\n${Json.prettyPrint(newPayload)}")
        client.url(s"${marathonAddress}/v2/apps").post(newPayload) map { _.json }
    }
  }

  def deleteApplication(fqon: String, wrkName: String, envName: String, appId: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    val appGroupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    val longAppId = appGroupId + "/" + appId.stripPrefix("/")
    client.url(s"${marathonAddress}/v2/apps${longAppId}").delete() map { marResp =>
      Logger.info(s"delete app: marathon response:\n" + Json.prettyPrint(marResp.json))
//      marResp.status match {
//        case 200 => true
//        case 404 => false
//        case _ => throw new RuntimeException((marResp.json \ "message").asOpt[String] getOrElse marResp.statusText)
//      }
        marResp.json
    }
  }

}

case object MarathonClient {

  def filterXformAppsByGroup(groupId: String)(app: JsObject): Option[JsObject] = {
    (app \ "id").asOpt[String] match {
      case Some(id) if id.startsWith(groupId) =>
        val stripEnvGroup = (__ \ 'id).json.update(
          __.read[JsString].map{ id => JsString(id.value.stripPrefix(groupId)) }
        )
        app.transform(stripEnvGroup) match {
          case JsSuccess(newApp,_) => Some(newApp)
          case _ => None
        }
      case _ => None
    }
  }

  def marathon2Container(marApp: JsObject): Option[ContainerApp] = {
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

  def metaContextToMarathonGroup(fqon: String, wrkName: String, envName: String): String = {
    ("/" + fqon + "/" + wrkName.replace("/","") + "/" + envName.replace("/","")).replace(" ","-").toLowerCase
  }

}