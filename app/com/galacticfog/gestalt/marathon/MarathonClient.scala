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

  def listApplicationsInEnvironment(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[Seq[ContainerStats]] = {
    listApplicationsInEnvironmentJSON(fqon, wrkName, envName) map (_.flatMap(MarathonClient.marathon2Container))
  }

  /**
    * Returns the raw json from Marathon
   **/
  def listApplicationsInEnvironmentJSON(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[Seq[JsObject]] = {
    val groupId = MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName).stripPrefix("/").stripSuffix("/")
    val allApps = client.url(s"${marathonAddress}/v2/groups/${groupId}?embed=group.apps&embed=group.apps.counts&embed=group.apps.tasks").get()
    allApps map { marResp =>
      marResp.status match {
        case 404 => Seq.empty
        case 200 => (marResp.json \ "apps").as[Seq[JsObject]]
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
        case 200 => (response.json \ "app").asOpt[JsObject].getOrElse(throw new RuntimeException("could not extract/transform app from Marathon REST response"))
        case _ => throw new RuntimeException("unexpected return from Marathon REST API")
      }
    }
  }

  def listDeploymentsAffectingEnvironment(fqon: String, wrkName: String, envName: String)(implicit ex: ExecutionContext): Future[JsValue] = {
    val allDeployments = client.url(s"${marathonAddress}/v2/deployments?embed=group.apps&embed=group.apps.deployments").get()
    allDeployments map { marResp =>
      marResp.status match {
        case 200 =>
          val allApp = (marResp.json \ "apps").asOpt[Seq[JsObject]].getOrElse(Seq.empty)
          val allDeps = allApp flatMap {app => (app \ "deployments").asOpt[Seq[JsObject]].getOrElse(Seq.empty)}
          Json.arr(allDeps)
        case _ => throw new RuntimeException("unexpected return from Marathon REST API")
      }
    }
  }

  def launchApp(fqon: String, wrkName: String, envName: String, name: String, marPayload: JsObject)(implicit ex: ExecutionContext): Future[JsValue] = {
    Logger.info(s"new payload:\n${Json.prettyPrint(marPayload)}")
    val appId = "/" + MarathonClient.metaContextToMarathonGroup(fqon, wrkName, envName).stripPrefix("/").stripSuffix("/") + "/" + name.stripPrefix("/")
    Logger.debug("posting to " + appId)
    val payloadWithId = marPayload ++ Json.obj("id" -> appId)
    client.url(s"${marathonAddress}/v2/apps").post(payloadWithId) map { marResp =>
      marResp.status match {
        case s if (200 to 299).toSeq.contains(s) => marResp.json
        case b if b == 409 =>
          throw new ConflictException(
            Json.stringify(marResp.json.as[JsObject] ++ Json.obj("status" -> marResp.status)))
        case b if Seq(400, 422).contains(b) =>
          throw new BadRequestException(
            Json.stringify(marResp.json.as[JsObject] ++ Json.obj("status" -> marResp.status)))
        case _ => throw new RuntimeException(marResp.body)
      }
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
          portLabels = (marApp \ "container" \ "docker" \ "network").asOpt[String] map {
            _ match {
              case "USER" | "BRIDGE" =>
                (marApp \ "container" \ "docker" \ "portMappings" \\ "labels").map(_.as[Map[String,String]])
              case "HOST" =>
                (marApp \ "portDefinitions" \\ "labels").map(_.as[Map[String,String]])
            }
          } getOrElse Seq.empty
          vipLabels = portLabels.flatMap(_.collect{
            case (k,v) if k.matches("VIP_[0-9]+") => v
          }) groupBy (_.split(":").head.stripPrefix("/") + ".marathon.l4lb.thisdcos.directory") filter {case (k,v) => k.trim.nonEmpty}
          addresses = vipLabels map {
            case (address,vips) => ContainerStats.ServiceAddress(
              address = address,
              ports = vips flatMap (_.split(":") match {
                case Array(address,port) => Try(port.toInt).toOption
                case _ => None
              })
            )
          }
    } yield ContainerStats(
      status = status,
      containerType = ctype,
      id = appId,
      cpus = cpus,
      memory = memory,
      image = image,
      age = age,
      numInstances = instances,
      tasksRunning = tasksRunning,
      tasksStaged = tasksStaged,
      tasksHealthy = tasksHealthy,
      tasksUnhealthy = tasksUnhealthy,
      taskStats = tasks,
      serviceAddresses = Some(addresses.toSeq)
    )
  }

  def metaContextToMarathonGroup(fqon: String, wrkName: String, envName: String): String = {
    (fqon.toLowerCase.split('.').foldLeft("")(_ + "/" + _) + "/" + wrkName.replace(".","").replace("/","-").toLowerCase + "/" + envName.replace(".","").replace("/","-")).replace(" ","-").toLowerCase
  }

}