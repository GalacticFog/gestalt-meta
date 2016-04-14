package com.galacticfog.gestalt

import com.rabbitmq.client.{Channel, ConnectionFactory, Connection}
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.output._
import play.api.libs.json._
import java.util.UUID
import scala.util.{Try,Success,Failure}
import controllers.util.db.EnvConfig

package object events {

  implicit lazy val eventContextFormat = Json.format[EventContext]
  implicit lazy val eventLambdaArgsFormat = Json.format[EventLambdaArgs]
  implicit lazy val migrateEventFormat = Json.format[MigrateEvent]      
  
  lazy val RABBIT_HOST = EnvConfig.rabbitHost
  lazy val RABBIT_PORT = EnvConfig.rabbitPort.toInt
  lazy val RABBIT_EXCHANGE = EnvConfig.rabbitExchange
  lazy val RABBIT_ROUTE = EnvConfig.rabbitRoute
  
  case class AmqpConnection(
    host: String = ConnectionFactory.DEFAULT_HOST,
    port: Int = ConnectionFactory.DEFAULT_AMQP_PORT,
    username: String = ConnectionFactory.DEFAULT_USER,
    password: String = ConnectionFactory.DEFAULT_PASS,
    heartbeat: Int   = ConnectionFactory.DEFAULT_HEARTBEAT)  

  case class EventContext(
    eventName: String,
    meta: String,
    workspace: UUID,
    environment: UUID,
    org: UUID,
    resourceId: UUID,
    resourceType: String)
    
  case class EventLambdaArgs(
    resource: GestaltResourceInstance, 
    rule: GestaltResourceInstance)
  
  case class MigrateEvent(
      eventContext: EventContext,
      lambdaArgs: EventLambdaArgs,
      providerId: UUID,
      metaUrl: Option[String] = None) {
    def toJson() = Json.toJson(this)       
  }
  
  object MigrateEvent {
    def make(
        env: GestaltResourceInstance, 
        container: GestaltResourceInstance,
        rule: GestaltResourceInstance,
        provider: UUID,
        meta: String) = Try {

      val workspace = UUID.fromString(env.properties.get("workspace"))
      val context = EventContext("container.migrate", meta, workspace, env.id, env.orgId, container.id, "")
      val args = EventLambdaArgs(container, rule)
      MigrateEvent(context, args, provider, Some(meta))
    }
  }
  
  case class AmqpEndpoint(exchange: String, route: String)
  
  class AmqpClient(cnn: AmqpConnection) {
    
    val factory = new ConnectionFactory()
    
    factory.setHost(cnn.host)
    factory.setPort(cnn.port)
    factory.setUsername(cnn.username)
    factory.setPassword(cnn.password)
    factory.setRequestedHeartbeat(cnn.heartbeat)
    
    def publish(ep: AmqpEndpoint, message: JsValue): Try[Unit] = {
      publish(ep, Json.stringify(message))
    }
    
    def publish(ep: AmqpEndpoint, message: String): Try[Unit] = Try {
      val connection = factory.newConnection
      val channel = connection.createChannel
      
      try {
        channel.exchangeDeclare( ep.exchange, "direct" )
        channel.basicPublish( ep.exchange, ep.route, null, message.getBytes() )
      } finally {
        channel.close()
        connection.close()
      }
    }
  }
  
  object AmqpClient {
    def apply(cnn: AmqpConnection) = new AmqpClient(cnn)
  }      
      
  
  

  
}