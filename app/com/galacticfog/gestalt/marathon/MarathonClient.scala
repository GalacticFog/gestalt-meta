package com.galacticfog.gestalt.marathon

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import org.joda.time.DateTime
import play.api.libs.ws.WSClient
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import scala.math.BigDecimal.int2bigDecimal

import play.api.Logger

import scala.util.Try

case class MarathonClient(client: WSClient, marathonAddress: String) {

  private[this] val log = Logger(this.getClass)

  def listApplications(implicit ex: ExecutionContext): Future[Seq[ContainerStats]] = {
    for {
      marResp <- client.url(s"${marathonAddress}/v2/apps").get()
      appsJson = (marResp.json \ "apps").as[Seq[JsObject]]
      apps = appsJson flatMap MarathonClient.marathon2Container
    } yield apps
  }

  // TODO: update this per below to account for marathon API in DCOS 1.8+
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
    * Returns the raw json from Marathon, but transformed to remove the org structure
   **/
  def listApplicationsInEnvironment_marathon_v2(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[Seq[JsObject]] = {
    val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    // TODO: revisit this now, targetting Marathon in DC/OS 1.8 and later
    // v0.16.0 and later will have the expansion on /v2/groups to get the counts for group.apps, but 0.15.x doesn't have it
    // therefore, to get these, we have to loop over all apps :(
    // val appGroup = client.url(s"${marathonAddress}/v2/groups/${groupId}?embed=group.apps.counts").get()
    val allApps = client.url(s"${marathonAddress}/v2/apps").get()
    allApps map { marResp =>
      marResp.status match {
        case 200 =>
          (marResp.json \ "apps").as[Seq[JsObject]].flatMap(MarathonClient.filterXformAppsByGroup(groupId))
        case _ => throw new RuntimeException("unexpected return from Marathon REST API")
      }
    }
  }

  def getApplicationByAppId(appId: String)(implicit ex: ExecutionContext): Future[JsObject] = {
    val url = "%s/v2/apps/%s".format(marathonAddress, appId.stripPrefix("/").stripSuffix("/"))
    val group = appId.take(appId.lastIndexOf("/"))
    
    log.debug("Looking up Marathon application: " + url)
    log.debug("Group ID: " + group)
    
    client.url(url).get map { response =>
      response.status match {
        case 404 => throw new ResourceNotFoundException(response.body)
        case 200 => {
          val xformApp = (response.json \ "app").asOpt[JsObject] flatMap {
            MarathonClient.filterXformAppsByGroup(group)
          }
          xformApp getOrElse {
            throw new RuntimeException("could not extract/transform app from Marathon REST response")
          }
        }
        case _ => throw new RuntimeException("unexpected return from Marathon REST API")
      }
    }
  }

  /**
    * Returns the raw json for the queried Marathon app, but transformed to remove the org structure
    *
    * @param fqon fully-qualified org name
    * @param wrkName workspace name
    * @param envName environment name
    * @param name local name of the app, stripped of the above
    */
  def getApplicationByName(fqon: String, wrkName: String, envName: String, name: String)(implicit ex: ExecutionContext): Future[JsObject] = {
    val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    // v0.16.0 and later will have the expansion on /v2/groups to get the counts for group.apps, but 0.15.x doesn't have it
    // therefore, to get these, we have to loop over all apps :(
    // val appGroup = client.url(s"${marathonAddress}/v2/groups/${groupId}?embed=group.apps.counts").get()
    val url = s"${marathonAddress}/v2/apps/" + groupId.stripPrefix("/").stripSuffix("/") + "/" + name.stripPrefix("/")
    val app = client.url(url).get()
    app map { marResp =>
      marResp.status match {
        case 404 =>  throw new ResourceNotFoundException(marResp.body)
        case 200 =>
          val xformApp = (marResp.json \ "app").asOpt[JsObject] flatMap MarathonClient.filterXformAppsByGroup(groupId)
          xformApp getOrElse(throw new RuntimeException("could not extract/transform app from Marathon REST response"))
        case _ => throw new RuntimeException("unexpected return from Marathon REST API")
      }
    }
  }

  /**
   * Returns the raw json from Marathon, but transformed to remove the org structure
   */
  def getApplicationInEnvironment_marathon_v2(fqon: String, wrkName: String, envName: String, appId: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    // v0.16.0 and later will have the expansion on /v2/groups to get the counts for group.apps, but 0.15.x doesn't have it
    // therefore, to get these, we have to loop over all apps :(
    // val appGroup = client.url(s"${marathonAddress}/v2/groups/${groupId}?embed=group.apps.counts").get()
    val allApps = client.url(s"${marathonAddress}/v2/apps/${groupId.stripSuffix("/")}/${appId.stripPrefix("/")}").get()
    allApps map { marResp =>
      marResp.status match {
        case 404 => throw new ResourceNotFoundException(marResp.statusText)
        case 200 =>
          val app = (marResp.json \ "app").as[JsObject]
          MarathonClient.filterXformAppsByGroup(groupId)(app) match {
            case Some(xformedApp) => Json.obj("app" -> xformedApp)
            case None => throw new RuntimeException("could not transform appId from Marathon RESP API response")
          }
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
    val prefixIdXForm = (__ \ 'id).json.update(
      __.read[JsString].map{ o => JsString(appGroupId.stripSuffix("/") + "/" + o.value.stripPrefix("/")) }
    )
    val stripIdXForm = (__ \ 'id).json.update(
      __.read[JsString].map{ o => JsString("/" + o.value.stripPrefix(appGroupId).stripPrefix("/")) }
    )
    marPayload.transform(prefixIdXForm) match {
      case e: JsError => Future.failed(new RuntimeException("error extracting and transforming app id"))
      case JsSuccess(newPayload,_) =>
        Logger.info(s"new payload:\n${Json.prettyPrint(newPayload)}")
        client.url(s"${marathonAddress}/v2/apps").post(newPayload) map { marResp =>
          marResp.status match {
            case s if (200 to 299).toSeq.contains(s) => marResp.json.transform(stripIdXForm) match {
              case e: JsError =>
                Logger.warn("Error stripping Gestalt context prefix from Marathon app id post-creation")
                marResp.json
              case JsSuccess(newPayload,_) =>
                newPayload
            }
            case b if b == 409 =>
              throw new ConflictException(
                Json.stringify(marResp.json.as[JsObject] ++ Json.obj("status" -> marResp.status)))
            case b if Seq(400,422).contains(b)  =>
              throw new BadRequestException(
                Json.stringify(marResp.json.as[JsObject] ++ Json.obj("status" -> marResp.status)))
            case _ => throw new RuntimeException(marResp.body)
          }
        }
    }
  }

  def deleteApplication(fqon: String, wrkName: String, envName: String, appId: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    val appGroupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName)
    val longAppId = appGroupId + appId.stripPrefix("/")
    client.url(s"${marathonAddress}/v2/apps${longAppId}").delete() map { marResp =>
      Logger.info(s"delete app: marathon response:\n" + Json.prettyPrint(marResp.json))
      marResp.json
    }
  }

  def deleteApplication(externalId: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    client.url(s"${marathonAddress}/v2/apps${externalId}").delete() map { marResp =>
      Logger.info(s"delete app: marathon response:\n" + Json.prettyPrint(marResp.json))
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
          instances <- (marApp \ "instances").asOpt[Int]
          tasksStaged = (marApp \ "tasksStaged").asOpt[Int] getOrElse 0
          tasksRunning = (marApp \ "tasksRunning").asOpt[Int] getOrElse 0
          tasksHealthy = (marApp \ "tasksHealthy").asOpt[Int] getOrElse 0
          tasksUnhealthy = (marApp \ "tasksUnhealthy").asOpt[Int] getOrElse 0
          status = (instances, tasksRunning, tasksHealthy, tasksUnhealthy) match {
            case (i,r,h,u) if r != i => "SCALING"
            case (i,r,h,u) if i == 0 => "SUSPENDED"
            case (i,r,h,u) if h == i => "HEALTHY"
            case (i,r,h,u) if u > 0  => "UNHEALTHY"
            case _ => "RUNNING" // r == i > 0 && u == 0 but no health checks
          }
          // if (tasksStaged > 0 && tasksRunning > 0) "SCALING" else if (tasksRunning > 0) "RUNNING" else if (tasksStaged > 0) "SCALING" else "SUSPENDED"
          cpus <- (marApp \ "cpus").asOpt[Double]
          memory  <- (marApp \ "mem").asOpt[Double]
          age <- (marApp \ "version").asOpt[String] map DateTime.parse
    } yield ContainerStats(
      status = status,
      containerType = ctype,
      id = service,
      cpus = cpus,
      memory = memory,
      image = image,
      age = age,
      numInstances = instances,
      tasksRunning = tasksRunning,
      tasksStaged = tasksStaged,
      tasksHealthy = tasksHealthy,
      tasksUnhealthy = tasksUnhealthy
    )
  }

  def metaContextToMarathonGroup(fqon: String, wrkName: String, envName: String): String = {
    (fqon.toLowerCase.split('.').foldLeft("")(_ + "/" + _) + "/" + wrkName.replace(".","").replace("/","-").toLowerCase + "/" + envName.replace(".","").replace("/","-") + "/").replace(" ","-").toLowerCase
  }

}