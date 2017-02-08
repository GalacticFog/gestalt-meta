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
  
  def create(context: ProviderContext, container: GestaltResourceInstance)(
      implicit ec: ExecutionContext): Future[GestaltResourceInstance] = {
    
    def updateSuccessfulLaunch(resource: GestaltResourceInstance)(marathonResponse: JsValue): GestaltResourceInstance = {
      val marathonAppId = (marathonResponse \ "id").as[String]
      upsertProperties(resource,
        "external_id" -> marathonAppId,
        "status" -> "LAUNCHED"
      )
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