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
    case sk: SetKey =>
      val f = (context.actorOf(Props(new ConfigDelegationActor)) ? sk)
        .mapTo[Option[String]]
      f pipeTo sender()
    case sk: SetKeys =>
      val f = (context.actorOf(Props(new ConfigDelegationActor)) ? sk)
        .mapTo[Map[String,Option[String]]]
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
    case _ =>
  }

}

object SystemConfigActor {
  final val name = "meta-system-config-actor"

  case class SetKey(caller: UUID, k: String, v: Option[String])
  case class SetKeys(caller: UUID, pairs: Map[String,Option[String]])
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

  def updateConfig(caller: UUID, data: Map[String,Option[String]]) = {
    ResourceFactory.findById(ResourceIds.SystemConfig).fold {
      val initData = data collect {
        case (k, Some(v)) => k -> v
      }
      ResourceFactory.create(ResourceIds.Org, caller)(
        GestaltResourceInstance(
          id = ResourceIds.SystemConfig,
          typeId = ResourceIds.Configuration,
          state = ResourceState.id(ResourceStates.Active),
          orgId = root.id,
          owner = ResourceOwnerLink(ResourceIds.Org,root.id.toString),
          name = "gestalt-system-config",
          properties = Some(Map(
            "data" -> Json.toJson(initData).toString
          ))
        )
      ).get
      data.keys.map(_ -> Option.empty[String]).toMap
    } {
      sc =>
        val props = sc.properties.getOrElse(Map.empty)
        val oldData = props.get("data").map(Json.parse(_)).map(_.as[Map[String,String]]).getOrElse(Map.empty)
        val removeKeys = data collect {
          case (k, None) => k
        }
        val updateKeys = data collect {
          case (k, Some(v)) => k -> v
        }
        val updatedProps = props ++ Map(
          "data" -> Json.toJson(oldData -- removeKeys ++ updateKeys).toString
        )
        ResourceFactory.update(
          resource = sc.copy(
            properties = Some(updatedProps)
          ),
          identity = caller
        ).get
        data.keys map (k => k -> oldData.get(k)) toMap
    }
  }

  override def receive: Receive = {
    case SystemConfigActor.SetKey(caller, k, v) =>
      sender() ! updateConfig(caller, Map(k -> v)).get(k).flatten
    case SystemConfigActor.SetKeys(caller, pairs) =>
      sender() ! updateConfig(caller, pairs)
    case SystemConfigActor.GetAllConfig =>
      sender() ! getConfig.getOrElse(Map.empty[String,String])
    case _ =>
  }
}
