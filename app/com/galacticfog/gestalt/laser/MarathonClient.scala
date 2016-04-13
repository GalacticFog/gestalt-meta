package com.galacticfog.gestalt.laser

import org.joda.time.DateTime
import play.api.Logger
import play.api.libs.ws.WSClient
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.concurrent.{ExecutionContext, Future}

case class ContainerStats(id: String,
                          containerType: String,
                          status: String,
                          cpus: Double,
                          memory: Double,
                          image: String,
                          age: DateTime,
                          numInstances: Int)

case class MarathonClient(client: WSClient, marathonAddress: String) {

  def listApplications(implicit ex: ExecutionContext): Future[Seq[ContainerStats]] = {
    for {
      marResp <- client.url(s"${marathonAddress}/v2/apps").get()
      appsJson = (marResp.json \ "apps").as[Seq[JsObject]]
      apps = appsJson flatMap MarathonClient.marathon2Container
    } yield apps
  }

  def listApplicationsInEnvironment(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[Seq[ContainerStats]] = {
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

  /**
     *Returns the raw json from Marathon, but transformed to remove the org structure
   **/
  def listApplicationsInEnvironment_marathon_v2(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    // v0.16.0 and later will have the expansion on /v2/groups to get the counts for group.apps, but 0.15.x doesn't have it
    // therefore, to get these, we have to loop over all apps :(
    // val appGroup = client.url(s"${marathonAddress}/v2/groups/${groupId}?embed=group.apps.counts").get()
    val allApps = client.url(s"${marathonAddress}/v2/apps").get()
    allApps map { marResp =>
      marResp.status match {
        case 200 =>
          val envApps = (marResp.json \ "apps").as[Seq[JsObject]].flatMap(MarathonClient.filterXformAppsByGroup(groupId))
          Json.obj(
            "apps" -> envApps
          )
        case _ => throw new RuntimeException("unexpected return from Marathon REST API")
      }
    }
  }

  def listDeploymentsAffectingEnvironment_marathon_v2(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    val allDeployments = client.url(s"${marathonAddress}/v2/deployments").get()
    allDeployments map { marResp =>
      marResp.status match {
        case 200 =>
          Logger.info(s"all deployments before filter/xform: ${Json.prettyPrint(marResp.json)}")
          val envDeployments = marResp.json.as[Seq[JsObject]].flatMap(MarathonClient.filterXformDeploymentsByGroup(groupId))
          Logger.info(s"deployments after filter/xform: ${Json.prettyPrint(Json.toJson(envDeployments))}")
          Json.toJson(envDeployments)
        case _ => throw new RuntimeException("unexpected return from Marathon REST API")
      }
    }
  }

  def launchContainer_marathon_v2(fqon: String, wrkName: String, envName: String, marPayload: JsObject)(implicit ex: ExecutionContext): Future[JsValue] = {
    val appGroupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    val idTransformer = (__ \ 'id).json.update(
      __.read[JsString].map{ o => JsString(appGroupId.stripSuffix("/") + "/" + o.value.stripPrefix("/")) }
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
    val longAppId = appGroupId + appId.stripPrefix("/")
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

  def scaleApplication(appId: String, numInstances: Int)(implicit ex: ExecutionContext): Future[JsValue] = {
    client.url(s"${marathonAddress}/v2/apps/${appId}").put(Json.obj(
      "instances" -> numInstances
    )) map { marResp =>
      Logger.info(s"scale app: marathon response:\n" + Json.prettyPrint(marResp.json))
      marResp.json
    }
  }

  def deleteApplication(fqon: String, appId: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    client.url(s"${marathonAddress}/v2/apps/${appId}").delete() map { marResp =>
      Logger.info(s"delete app: marathon response:\n" + Json.prettyPrint(marResp.json))
      marResp.json
    }
  }

  def getInfo()(implicit ex: ExecutionContext): Future[JsValue] = {
    client.url(s"${marathonAddress}/v2/info").get map { marResp =>
      marResp.status match {
        case 200 => marResp.json
        case _ => throw new RuntimeException(marResp.statusText)
      }
    }
  }

}

case object MarathonClient {

  def filterXformDeploymentsByGroup(groupId: String)(app: JsObject): Option[JsObject] = {
    val xformAppId = (__ \ "app").json.update(
      of[JsString].map {
        case JsString(appId) => JsString("/" + appId.stripPrefix(groupId))
      }
    )
    val xformAndFilter = (
      (__ \ "steps").json.update( of[JsArray].map { case _ => JsArray() } ) andThen
      (__ \ "currentStep").json.update( of[JsNumber].map {case _ => JsNumber(0) } ) andThen
      (__ \ "totalSteps").json.update( of[JsNumber].map {case _ => JsNumber(0) } ) andThen
      (__ \ "currentActions").json.update( of[JsArray].map{ arr =>
        Json.toJson(arr.as[Seq[JsObject]].flatMap { action =>
          (action \ "app").asOpt[String] match {
            case Some(appId) if appId.startsWith(groupId) => action.transform(xformAppId).asOpt
            case _ => None
          }
        })
      }) andThen
      (__ \ "affectedApps").json.update( of[JsArray].map{ arr =>
        Json.toJson(arr.as[Seq[String]].flatMap {
          case appId if appId.startsWith(groupId) => Some("/" + appId.stripPrefix(groupId))
          case _ => None
        })
      })
    )

    app.transform(xformAndFilter) match {
      case JsSuccess(newList,_) if !(newList \ "affectedApps").as[Seq[JsString]].isEmpty => Some(newList)
      case _ => None
    }
  }

  def filterXformAppsByGroup(groupId: String)(app: JsObject): Option[JsObject] = {
    (app \ "id").asOpt[String] match {
      case Some(id) if id.startsWith(groupId) =>
        val stripEnvGroup = (__ \ 'id).json.update(
          __.read[JsString].map{ id => JsString("/" + id.value.stripPrefix(groupId)) }
        )
        app.transform(stripEnvGroup) match {
          case JsSuccess(newApp,_) => Some(newApp)
          case _ => None
        }
      case _ => None
    }
  }

  def marathon2Container(marApp: JsObject): Option[ContainerStats] = {
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
    } yield ContainerStats(status = status, containerType = ctype, id = service, cpus = cpus, memory = memory, image = image, age = age, numInstances = numRunning)
  }

  def metaContextToMarathonGroup(fqon: String, wrkName: String, envName: String): String = {
    ("/" + fqon + "/" + wrkName.replace("/","-") + "/" + envName.replace("/","-") + "/").replace(" ","-").toLowerCase
  }

}