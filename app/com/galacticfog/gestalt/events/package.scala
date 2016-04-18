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
  implicit lazy val migrateEventFormat = Json.format[PolicyEvent]      
  
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
  
  //
  // TODO: Rename MigrateEvent -> PolicyEvent
  //
    
  case class PolicyEvent(
      eventContext: EventContext,
      lambdaArgs: EventLambdaArgs,
      providerId: UUID,
      metaUrl: Option[String] = None) {
    def toJson() = Json.toJson(this)       
  }
  
  object PolicyEvent {
    def make(
        eventName: String,
        env: GestaltResourceInstance, 
        container: GestaltResourceInstance, 
        rule: GestaltResourceInstance, 
        provider: UUID, 
        meta: String) = Try {

      val workspace = UUID.fromString(env.properties.get("workspace"))
      val context = EventContext(eventName, meta, workspace, env.id, env.orgId, container.id, "")
      val args = EventLambdaArgs(container, rule)
      PolicyEvent(context, args, provider, Some(meta))
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
  
  case class RabbitUser(name: String, password_hash: String, hashing_algorithm: String, tags: String)
  case class RabbitPermission(user: String, vhost: String, configure: String, write: String, read: String)
  
  case class RabbitInfo(rabbit_version: String, exchanges: Seq[RabbitExchange], bindings: Seq[RabbitBinding]) {
    def hasBinding(source: String, routeKey: String) = {
      bindings exists { b => (b.source == source && b.routing_key == routeKey) }
    }
    
    def hasExchange(name: String) = exchanges exists { _.name == name }
  }
  
  case class RabbitExchange(
      name: String,
      vhost: String,
      etype: String,
      durable: Boolean,
      auto_delete: Boolean,
      internal: Boolean,
      arguments: Option[Map[String,String]])
      
  case class RabbitBinding(
      source: String,
      vhost: String,
      destination: String,
      destination_type: String,
      routing_key: String,
      arguments: Option[Map[String,String]])
  
  import play.api.libs.functional.syntax._
  
  
  implicit lazy val rabbitBindingFormat  = Json.format[RabbitBinding]
  implicit lazy val rabbitInfoFormat     = Json.format[RabbitInfo]
  implicit lazy val rabbitExchangeReads: Reads[RabbitExchange] = (
    ( __ \ "name").read[String] and
    ( __ \ "vhost").read[String] and
    ( __ \ "type").read[String] and
    ( __ \ "durable").read[Boolean] and
    ( __ \ "auto_delete").read[Boolean] and
    ( __ \ "internal").read[Boolean] and
    ( __ \ "arguments").readNullable[Map[String,String]]
  )(RabbitExchange) 
  implicit lazy val rabbitExchangeWrites: Writes[RabbitExchange] = (
    ( __ \ "name").write[String] and
    ( __ \ "vhost").write[String] and
    ( __ \ "type").write[String] and
    ( __ \ "durable").write[Boolean] and
    ( __ \ "auto_delete").write[Boolean] and
    ( __ \ "internal").write[Boolean] and
    ( __ \ "arguments").writeNullable[Map[String,String]]
  )(unlift(RabbitExchange.unapply))  
  
 
  
  
/*
    {
      "source": "env-test-exchange",
      "vhost": "/",
      "destination": "sy-worker-queue",
      "destination_type": "queue",
      "routing_key": "policy",
      "arguments": {}
    },
 */
  
  /*
{
  "rabbit_version": "3.6.1",
  "users": [
    {
      "name": "guest",
      "password_hash": "a8XymgDLUSIzt7z/EUD6vzhsLrAhACDMsksYDHwX/PiSQvTb",
      "hashing_algorithm": "rabbit_password_hashing_sha256",
      "tags": "administrator"
    }
  ],
  "vhosts": [
    {
      "name": "/"
    }
  ],
  "permissions": [
    {
      "user": "guest",
      "vhost": "/",
      "configure": ".*",
      "write": ".*",
      "read": ".*"
    }
  ],
  "parameters": [],
  "policies": [],
  "queues": [
    {
      "name": "worker-queue",
      "vhost": "/",
      "durable": true,
      "auto_delete": false,
      "arguments": {}
    },
    {
      "name": "sy-worker-queue",
      "vhost": "/",
      "durable": false,
      "auto_delete": false,
      "arguments": {}
    }
  ],
  "exchanges": [
    {
      "name": "env-test-exchange",
      "vhost": "/",
      "type": "direct",
      "durable": false,
      "auto_delete": false,
      "internal": false,
      "arguments": {}
    },
    {
      "name": "test-exchange",
      "vhost": "/",
      "type": "direct",
      "durable": false,
      "auto_delete": false,
      "internal": false,
      "arguments": {}
    },
    {
      "name": "sy-test-exchange",
      "vhost": "/",
      "type": "direct",
      "durable": false,
      "auto_delete": false,
      "internal": false,
      "arguments": {}
    }
  ],
  "bindings": [
    {
      "source": "env-test-exchange",
      "vhost": "/",
      "destination": "sy-worker-queue",
      "destination_type": "queue",
      "routing_key": "policy",
      "arguments": {}
    },
    {
      "source": "sy-test-exchange",
      "vhost": "/",
      "destination": "sy-worker-queue",
      "destination_type": "queue",
      "routing_key": "policy",
      "arguments": {}
    },
    {
      "source": "test-exchange",
      "vhost": "/",
      "destination": "worker-queue",
      "destination_type": "queue",
      "routing_key": "policy",
      "arguments": {}
    }
  ]
}
   */

  
}