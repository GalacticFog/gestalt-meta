package com.galacticfog.gestalt.marathon

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerStats
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ConflictException, InternalErrorException, ResourceNotFoundException, UnprocessableEntityException}
import org.joda.time.DateTime
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.json.Json.toJsFieldJsValueWrapper

import scala.math.BigDecimal.int2bigDecimal
import play.api.Logger
import play.api.http.HeaderNames

import scala.util.Try

case class MarathonClient(client: WSClient, marathonBaseUrl: String, acsToken: Option[String] = None, secretBaseUrl: Option[String] = None, secretStore: String = MarathonClient.DEFAULT_SECRET_STORE) {

  private[this] val log = Logger(this.getClass)

  private[this] def otherError(marResp: WSResponse) = new InternalErrorException({
    val resp = Try(marResp.json.as[JsObject] ++ Json.obj("status" -> marResp.status)).map(_.toString).getOrElse(
      s"code ${marResp.status}: ${marResp.statusText}"
    )
    log.warn(marResp.toString)
    log.warn(marResp.body)
    "error from Marathon REST API: " + resp
  })

  private[this] def genRequest(endpoint: String, baseUrl: String = marathonBaseUrl) = {
    val url = s"${baseUrl}/${endpoint.stripPrefix("/")}"
    acsToken.foldLeft(
      client.url(url)
    ) {
      case (req,token) => req.withHeaders(HeaderNames.AUTHORIZATION -> s"token=${token}")
    }
  }

  def listApplicationsInEnvironment(groupPrefix: Option[String], fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[Seq[ContainerStats]] = {
    val groupId = metaContextToMarathonAppGroup(groupPrefix, fqon, wrkName, envName).stripPrefix("/").stripSuffix("/")
    val url = s"/v2/groups/${groupId}?embed=group.apps&embed=group.apps.counts&embed=group.apps.tasks"
    log.debug(s"GETing $url from Marathon")
    val allApps = genRequest(url).get()
    allApps map { marResp =>
      marResp.status match {
        case 404 =>
          log.debug(s"received 404 from Marathon $url, group does not exist, returning empty list")
          Seq.empty
        case 200 =>
          log.trace(s"received 200 from Marathon $url: ${Json.prettyPrint(marResp.json)}")
          (marResp.json \ "apps").as[Seq[JsObject]]
        case _ => throw otherError(marResp)
      }
    } map (_.flatMap{js =>
      val cs = MarathonClient.marathon2Container(js)
      log.trace(s"parsed ${js} to ${cs}")
      cs
    } )
  }

  def getApplicationByAppId(appId: String)(implicit ex: ExecutionContext): Future[JsObject] = {
    val endpoint = "/v2/apps/%s".format(stripAppId(appId))
    val group = appId.take(appId.lastIndexOf("/"))
    log.debug("Looking up Marathon application: " + endpoint)
    log.debug("Group ID: " + group)
    genRequest(endpoint).get map { response =>
      response.status match {
        case 404 => throw new ResourceNotFoundException(response.body)
        case 200 => (response.json \ "app").asOpt[JsObject].getOrElse(throw new RuntimeException("could not extract/transform app from Marathon REST response"))
        case _ => throw otherError(response)
      }
    }
  }

  def listDeploymentsAffectingEnvironment(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    val allDeployments = genRequest("/v2/deployments?embed=group.apps&embed=group.apps.deployments").get()
    allDeployments map { marResp =>
      marResp.status match {
        case 200 =>
          val allApp = (marResp.json \ "apps").asOpt[Seq[JsObject]].getOrElse(Seq.empty)
          val allDeps = allApp flatMap {app => (app \ "deployments").asOpt[Seq[JsObject]].getOrElse(Seq.empty)}
          Json.arr(allDeps)
        case _ => throw otherError(marResp)
      }
    }
  }

  def standardResponseHandler(resp: WSResponse): Future[JsValue] = {
    resp.status match {
      case s if (200 to 299).contains(s) => Future.successful(resp.json)
      case 409 =>
        Future.failed(new ConflictException(
          (resp.json.as[JsObject] ++ Json.obj("status" -> resp.status)).toString
        ))
      case 403 | 401 => Future.failed(new UnprocessableEntityException(
        s"DC/OS backend returned ${resp.status}: ${resp.statusText}"
      ))
      case b if Seq(400, 422).contains(b) =>
        Future.failed(new BadRequestException(
          (resp.json.as[JsObject] ++ Json.obj("status" -> resp.status)).toString
        ))
      case _ => Future.failed(otherError(resp))
    }
  }

  def launchApp(marPayload: JsObject)(implicit ex: ExecutionContext): Future[JsValue] = {
    log.info(s"new app payload:\n${Json.prettyPrint(marPayload)}")
    genRequest("/v2/apps").post(marPayload) map { marResp =>
      marResp.status match {
        case s if (200 to 299).contains(s) => marResp.json
        case b if b == 409 =>
          throw new ConflictException(
            Json.stringify(marResp.json.as[JsObject] ++ Json.obj("status" -> marResp.status)))
        case b if Seq(400, 422).contains(b) =>
          throw new BadRequestException(
            Json.stringify(marResp.json.as[JsObject] ++ Json.obj("status" -> marResp.status)))
        case _ => throw otherError(marResp)
      }
    }
  }

  private def withSecretUrl[B](f: String => Future[B]): Future[B] = {
    secretBaseUrl.fold[Future[B]] (
      Future.failed(new UnprocessableEntityException("provider was not configured with secret support"))
    )(
      url => f(url)
    )
  }

  def createSecret(secretId: String, marPayload: JsValue)(implicit ex: ExecutionContext): Future[Unit] = {
    log.info(s"new secret payload:\n${Json.prettyPrint(marPayload)}")
    withSecretUrl(
      genRequest(s"/secret/${secretStore}/${secretId.stripPrefix("/")}", _)
        .put(marPayload)
        .flatMap({ resp =>
          resp.status match {
            case 201 => Future.successful(())
            case 409 =>
              Future.failed(new ConflictException(
                (resp.json.as[JsObject] ++ Json.obj("status" -> resp.status)).toString
              ))
            case 403 | 401 => Future.failed(new UnprocessableEntityException(
              s"DC/OS backend returned ${resp.status}: ${resp.statusText}"
            ))
            case b if Seq(400, 422).contains(b) =>
              Future.failed(new BadRequestException(
                (resp.json.as[JsObject] ++ Json.obj("status" -> resp.status)).toString
              ))
            case _ => Future.failed(otherError(resp))
          }
        })
    )
  }

  def deleteSecret(secretId: String)(implicit ex: ExecutionContext): Future[Unit] = {
    withSecretUrl(
      genRequest(s"/secret/${secretStore}/${secretId.stripPrefix("/")}", _).withQueryString(
        "force" -> "true"
      ).delete() flatMap { marResp => marResp.status match {
        case s if (200 to 299).contains(s) =>
          Future.successful(())
        case _ =>
          Future.failed(otherError(marResp))
      } }
    )
  }

  def updateApplication(appId: String, marPayload: JsObject)(implicit ex: ExecutionContext): Future[JsValue] = {
    log.info(s"update app payload:\n${Json.prettyPrint(marPayload)}")
    val endpoint = "/v2/apps/%s".format(stripAppId(appId))
    genRequest(endpoint).withQueryString(
      "force" -> "true"
    ).put(marPayload) flatMap standardResponseHandler
  }

  private[this] def stripAppId: String => String = _.stripPrefix("/").stripSuffix("/")

  def deleteApplication(externalId: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    genRequest("/v2/apps/%s".format(stripAppId(externalId))).withQueryString(
      "force" -> "true"
    ).delete() map { marResp => marResp.status match {
      case s if (200 to 299).contains(s) =>
        log.info(s"delete app: marathon response:\n" + Json.prettyPrint(marResp.json))
        marResp.json
      case _ => throw otherError(marResp)
    } }
  }  

  def scaleApplication(appId: String, numInstances: Int)(implicit ex: ExecutionContext): Future[JsValue] = {
    genRequest(s"/v2/apps/${appId}").withQueryString(
      "force" -> "true"
    ).put(Json.obj(
      "instances" -> numInstances
    )) map { marResp => marResp.status match {
      case s if (200 to 299).contains(s) =>
        log.info(s"scale app: marathon response:\n" + Json.prettyPrint(marResp.json))
        marResp.json
      case _ => throw otherError(marResp)
    } }
  }

  def getInfo()(implicit ex: ExecutionContext): Future[JsValue] = {
    genRequest("/v2/info").get map { marResp =>
      marResp.status match {
        case 200 => marResp.json
        case _ => throw otherError(marResp)
      }
    }
  }

}

case object MarathonClient {

  val DEFAULT_SECRET_STORE: String = "default"

  def marathon2Container(marApp: JsObject): Option[ContainerStats] = {
    for {
      appId <- (marApp \ "id").asOpt[String]
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
      tasks = (marApp \ "tasks").asOpt[Seq[ContainerStats.TaskStat]]
    } yield ContainerStats(
      status = status,
      containerType = ctype,
      external_id = appId,
      cpus = cpus,
      memory = memory,
      image = image,
      age = age,
      numInstances = instances,
      tasksRunning = tasksRunning,
      tasksStaged = tasksStaged,
      tasksHealthy = tasksHealthy,
      tasksUnhealthy = tasksUnhealthy,
      taskStats = tasks
    )
  }

}