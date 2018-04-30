package actors

import java.util.UUID

import akka.actor.{Actor, ActorLogging, OneForOneStrategy, Props, SupervisorStrategy}
import com.google.inject.Singleton
import akka.pattern.{ask, pipe}
import com.galacticfog.gestalt.data.{ResourceFactory, ResourceState}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceOwnerLink, ResourceStates}
import play.api.libs.json.{JsObject, Json}

import scala.language.postfixOps
import scala.concurrent.duration._

@Singleton
class SystemConfigActor() extends Actor with ActorLogging {

  implicit val askTimeout: akka.util.Timeout = 30 seconds
  import SystemConfigActor._
  import context.dispatcher

  override def supervisorStrategy: SupervisorStrategy = OneForOneStrategy(
    maxNrOfRetries = 0, withinTimeRange = 0 seconds
  ) {
    case _: Exception =>
      log.error("ConfigDelegationActor crashed unexpectedly")
      SupervisorStrategy.Stop
  }

  override def receive: Receive = {
    case sk: SetKeys =>
      val f = (context.actorOf(Props(new ConfigDelegationActor)) ? sk)
        .mapTo[Boolean]
      f pipeTo sender()
    case GetKey(key) =>
      val f = (context.actorOf(Props(new ConfigDelegationActor)) ? GetAllConfig)
        .mapTo[Map[String,String]]
        .map(_.get(key))
      f pipeTo sender()
    case GetAllConfig =>
      val f = (context.actorOf(Props(new ConfigDelegationActor)) ? GetAllConfig)
        .mapTo[Map[String,String]]
      f pipeTo sender()
  }

}

object SystemConfigActor {
  final val name = "meta-system-config-actor"

  case class SetKeys(caller: UUID, pairs: (String,String)*)
  case class GetKey(key: String)
  case object GetAllConfig
}

class ConfigDelegationActor extends Actor with ActorLogging {

  lazy val root = ResourceFactory.findAllByName(ResourceIds.Org, "root").head

  def getConfig: Option[Map[String,String]] = {
    for {
      sc <- ResourceFactory.findById(ResourceIds.SystemConfig)
      props <- sc.properties
      d <- props.get("data")
      m <- Json.parse(d).asOpt[Map[String,String]]
    } yield m
  }

  def updateConfig(caller: UUID, data: Map[String,String]) = {
    ResourceFactory.findById(ResourceIds.SystemConfig).fold {
      ResourceFactory.create(ResourceIds.Org, caller)(
        GestaltResourceInstance(
          id = ResourceIds.SystemConfig,
          typeId = ResourceIds.Configuration,
          state = ResourceState.id(ResourceStates.Active),
          orgId = root.id,
          owner = ResourceOwnerLink(ResourceIds.Org,root.id.toString),
          name = "gestalt-system-config",
          properties = Some(Map(
            "data" -> Json.toJson(data).toString
          ))
        )
      ).get
    } {
      sc =>
        val props = sc.properties.getOrElse(Map.empty)
        val oldData = props.get("data").map(Json.parse(_)).map(_.as[JsObject]).getOrElse(Json.obj())
        val updatedData = oldData ++ Json.toJson(data).as[JsObject]
        val updatedProps = props ++ Map(
          "data" -> updatedData.toString
        )
        ResourceFactory.update(
          resource = sc.copy(
            properties = Some(updatedProps)
          ),
          identity = caller
        ).get
    }
  }

  override def receive: Receive = {
    case SystemConfigActor.SetKeys(caller, pairs) =>
      updateConfig(caller, Map(pairs))
      sender() ! true
    case SystemConfigActor.GetAllConfig =>
      sender() ! getConfig.getOrElse(Map.empty[String,String])
    case _ =>
  }
}

