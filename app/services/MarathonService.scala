package services


import java.util.UUID

import com.galacticfog.gestalt.events.{AmqpClient, AmqpConnection, AmqpEndpoint, PolicyEvent}


import play.api.Logger

import play.api.libs.ws.WS
import play.api.Play.current
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, ResourceIds}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.{ContainerInstance, ContainerSpec, Resource, ResourcePath}
import com.galacticfog.gestalt.events._
import com.google.inject.Inject


import skuber._
import skuber.api.client._
import skuber.json.format._
import skuber.ext._
import skuber.json.ext.format._
import org.yaml.snakeyaml._
import play.api.libs.json._
import skuber.api.client.ObjKind

import com.galacticfog.gestalt.caas.kube._

import controllers.util._
import com.galacticfog.gestalt.json.Js
import scala.concurrent.ExecutionContext


class MarathonService extends CaasService with JsonInput with MetaControllerUtils {
  
  
  import com.galacticfog.gestalt.meta.api.ContainerSpec._
  
  
  def create(context: ProviderContext, container: GestaltResourceInstance)(
      implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    
    def updateSuccessfulLaunch(resource: GestaltResourceInstance)(marathonResponse: JsValue): GestaltResourceInstance = {
      
      updateServiceAddresses(marathonResponse, container)
      
      val marathonAppId = (marathonResponse \ "id").as[String]
      log.debug("MARATHON-APPID : " + marathonAppId)
      upsertProperties(resource,
        "external_id" -> marathonAppId,
        "status" -> "LAUNCHED"
      )
    }
    
    import com.galacticfog.gestalt.json._
    import com.galacticfog.gestalt.json.Js.find
    
    implicit def jsval2obj(jsv: JsValue) = jsv.as[JsObject]
    
    def updateServiceAddresses(marApp: JsValue, r: GestaltResourceInstance) = {
      val clusterid = ".marathon.l4lb.thisdcos.directory"
      
      val portlabels: Seq[Map[String,String]] = {
        Js.find(marApp.as[JsObject], "/container/docker/network") map {
          _.as[String] match {
            case "USER" | "BRIDGE" =>
              (marApp \ "container" \ "docker" \ "portMappings" \\ "labels").map(_.as[Map[String,String]])
            case "HOST" =>
              (marApp \ "portDefinitions" \\ "labels").map(_.as[Map[String,String]])            
          }
        } getOrElse Seq.empty
      }
      
      val pmaps = Js.find(marApp.as[JsObject], "/container/docker/portMappings") map { portmaps =>
        portmaps.as[JsArray].value map { pm =>
          val name     = find(pm, "/name").get.as[String]
          val protocol = find(pm, "/protocol").map(_.as[String]) orElse Some("tcp")
          val port     = find(pm, "/containerPort").get.as[Int]
          
          find(pm.as[JsObject], "/labels").foldLeft(Map[String,ServiceAddress]()) { (acc, next) => 
            val labelvalue = next.as[Map[String, String]] collect { case (k,v) if k.matches("VIP_[0-9]+") => 
                v.split(":").head.stripPrefix("/") + clusterid 
            }
            acc + (name -> ServiceAddress(labelvalue.headOption.get, port, protocol))
          }
        }
      }
  
      /*
       * TODO: WIP - this needs to be cleaned up and validated safely.
       */
      val portList = pmaps.get.head

      val portprops = r.properties.get.get("port_mappings") map { 
        p => Js.parse[Seq[PortMapping]](Json.parse(p)) getOrElse {
          throw new RuntimeException("Could not parse portMappings to JSON.")
        }
      }
 
      def isExposed(p: PortMapping): Boolean = p.expose_endpoint getOrElse false
      
      val finalmap = portprops.get collect { case p if isExposed(p) => 
        p.copy(service_address = Some(portList(p.name.get)))
      }
      
      val finaljson = Json.toJson(finalmap)
      
      val newproperties = container.properties.get ++ Map("port_mappings" -> finaljson.toString)
      
      newproperties foreach { case (k,v) => println("%s:\n%s\n".format(k, v)) }
      
      container.copy(properties = Some(newproperties))
      
      /*
       * NOW have to map each of these service addresses to a properties.port_mapping in the
       * container resource. 
       * Don't think i have a choice but to filter and replace (inject service_address into port_mapping).
       * 
       * Then, back in ProviderManager, we have to read any service addresses defined to set the variables
       * on the provider.
       */
    }
    
    
    def updateFailedLaunch(resource: GestaltResourceInstance)(t: Throwable): Throwable = {
      val updatedResource = upsertProperties(resource,
        "status" -> "LAUNCH_FAILED"
      )
      new BadRequestException(s"launch failed: ${t.getMessage}")
    }
    
//      val containerResourcePre = upsertProperties(origContainerResourcePre, "provider" -> Json.obj(
//        "name" -> provider.name,
//        "id" -> provider.id
//      ).toString)
//      
//    context.provider
    ContainerSpec.fromResourceInstance(container) match {
      case Failure(e) => Future.failed(e)
      case Success(spec) =>          
        val marathonApp = toMarathonLaunchPayload(
          fqon = context.fqon,
          workspaceName = context.workspace.name,
          environmentName = context.environment.name,
          name = container.name,
          props = spec,
          provider = context.provider
        )
        val marathonAppCreatePayload = Json.toJson(marathonApp).as[JsObject]
        marathonClient(context.provider).launchApp(
          fqon = context.fqon,
          wrkName = context.workspace.name,
          envName = context.environment.name,
          name = spec.name,
          marPayload = marathonAppCreatePayload
        ).transform( updateSuccessfulLaunch(container), updateFailedLaunch(container) )
          
    }
  }
  
  
  def destroyContainer(container: GestaltResourceInstance): Future[Unit] = {
    val providerId = Json.parse(container.properties.get("provider")) \ "id"
    val provider   = ResourceFactory.findById(UUID.fromString(providerId.as[String])) getOrElse {
      throw new RuntimeException("Could not find Provider : " + providerId)
    }
    // TODO: what to do if there is no external_id ? delete the resource? attempt to reconstruct external_id from resource?
    val maybeExternalId = for {
      props <- container.properties
      eid <- props.get("external_id")
    } yield eid
    
    maybeExternalId match {
      case Some(eid) => marathonClient(provider).deleteApplication(eid) map { js =>
        log.debug(s"response from MarathonClient.deleteApplication:\n${Json.prettyPrint(js)}")
      }
      case None =>
        log.debug(s"no external_id property in container ${container.id}, will not attempt delete against provider")
        Future.successful(())
    }
  }  
  
//def findEnvironmentContainerByName(fqon: String, environment: UUID, containerName: String): Future[Option[(GestaltResourceInstance,Seq[ContainerInstance])]] = {
//    println("***Finding container by name...")
//    try {
//      
//      println(s"***ENVORONMENT: $environment, name: $containerName")
//      val cbn = ResourceFactory.findChildByName(parent = environment, childType = ResourceIds.Container, name = containerName)
//      println("***CHILD-BY-NAME : " + cbn)
//      
//    } catch {
//      case e : Throwable => {
//        println("***FAILED CALLING findChildByName()")
//        e.printStackTrace()
//        log.error(e.getMessage, e.getCause)
//      }
//    }
//    // Find container resource in Meta, convert to ContainerSpec
//    val maybeContainerSpec = for {
//      r <- ResourceFactory.findChildByName(parent = environment, childType = ResourceIds.Container, name = containerName)
//      s <- ContainerSpec.fromResourceInstance(r).toOption
//    } yield (r -> s)
//    
//    println("***Getting stats...")
//    val maybeStatsFromMarathon = for {
//      metaContainerSpec <- maybeContainerSpec
//      provider <- Try { marathonProvider(metaContainerSpec._2.provider.id) }.toOption
//      extId    <- metaContainerSpec._2.external_id
//      
//      // Lookup container in marathon, convert to ContainerStats
//      marathonApp = for {
//        client <- Future.fromTry(Try(
//          marathonClient(provider)
//        ))
//        js <- client.getApplicationByAppId(extId)
//        stats <- Future.fromTry(Try {MarathonClient.marathon2Container(js).get})
//      } yield stats
//      
//    } yield marathonApp
//    
//    maybeStatsFromMarathon match {
//      case None => Future {
//        maybeContainerSpec.map(containerSpec =>
//          updateMetaContainerWithStats(containerSpec._1, None) -> Seq.empty
//        )
//      }
//      case Some(fStatsFromMarathon) => fStatsFromMarathon.map(stats =>
//        Some(updateMetaContainerWithStats(maybeContainerSpec.get._1, Some(stats)) -> Seq.empty)
//      ) recover {
//        case e: Throwable =>
//          maybeContainerSpec.map(containerSpec =>
//            updateMetaContainerWithStats(containerSpec._1, None) -> Seq.empty
//          )
//      }
//    }
//  }  
//  
//  def marathonProvider(provider: UUID): GestaltResourceInstance = {
//    ResourceFactory.findById(ResourceIds.MarathonProvider, provider) getOrElse {
//      throw new ResourceNotFoundException(s"MarathonProvider with ID '$provider' not found.")
//    }
//  }
//  
//  protected [controllers] def updateMetaContainerWithStats(metaCon: GestaltResourceInstance, stats: Option[ContainerStats]): Instance = {
//    // TODO: do not overwrite status if currently MIGRATING: https://gitlab.com/galacticfog/gestalt-meta/issues/117
//    val newStats = stats match {
//      case Some(stats) => Seq(
//        "age" -> stats.age.toString,
//        "status" -> stats.status,
//        "num_instances" -> stats.numInstances.toString,
//        "tasks_running" -> stats.tasksRunning.toString,
//        "tasks_healthy" -> stats.tasksHealthy.toString,
//        "tasks_unhealthy" -> stats.tasksUnhealthy.toString,
//        "tasks_staged" -> stats.tasksStaged.toString,
//        "instances"       -> stats.taskStats.map{Json.toJson(_).toString}.getOrElse("[]")
//        /*"service_addresses" -> stats.serviceAddresses.map{Json.toJson(_).toString}.getOrElse("[]")*/
//      )
//      case None => Seq(
//        "status" -> "LOST",
//        "num_instances" -> "0",
//        "tasks_running" -> "0",
//        "tasks_healthy" -> "0",
//        "tasks_unhealthy" -> "0",
//        "tasks_staged" -> "0",
//        "instances"       -> "[]",
//        "service_addresses" -> "[]"
//      )
//    }
//    val updatedMetaCon = metaCon.copy(
//      properties = metaCon.properties map {
//        _ ++ newStats
//      } orElse {
//        Some(newStats toMap)
//      }
//    )
//    // this update is passive... mark the "modifier" as the last person to have actively modified the container, or the creator...
//    (metaCon.modified.flatMap(_.get("id")) orElse metaCon.created.flatMap(_.get("id"))) flatMap {s => Try(UUID.fromString(s)).toOption} match {
//      case None => metaCon // TODO: not sure what else to do here
//      case Some(updater) =>
//        Future{ ResourceFactory.update(updatedMetaCon, updater) } onComplete {
//          case Success(Success(updatedContainer)) =>
//            log.trace(s"updated container ${updatedContainer.id} with info from marathon")
//          case Success(Failure(e)) =>
//            log.warn(s"failure to update container ${updatedMetaCon.id}",e)
//          case Failure(e) =>
//            log.warn(s"failure to update container ${updatedMetaCon.id}",e)
//        }
//        updatedMetaCon
//    }
//  }  
  
  private[services] def marathonClient(provider: GestaltResourceInstance): MarathonClient = {
    val providerUrl = (Json.parse(provider.properties.get("config")) \ "url").as[String]
    log.debug("Marathon URL: " + providerUrl)
    MarathonClient(WS.client, providerUrl)
  }      
      
  private[services] def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
    resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
  }

  private[services] def futureToFutureTry[T](f: Future[T]): Future[Try[T]] = f.map(Success(_)).recover({case x => Failure(x)})
  
}