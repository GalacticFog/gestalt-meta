package com.galacticfog.gestalt.events


import scala.util.Try

import com.rabbitmq.client.ConnectionFactory

import play.api.Logger
import play.api.libs.json.{Json, JsValue}
import play.api.libs.json.Json.toJsFieldJsValueWrapper

  
class AmqpClient(cnn: AmqpConnection) {
  
  private[this] val log = Logger(this.getClass)
  private[events] val factory = new ConnectionFactory()
  
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
      /*
       * This will throw an exception if the exchange does not exist.
       */
      channel.exchangeDeclarePassive(ep.exchange)
      channel.basicPublish(ep.exchange, ep.route, null, message.getBytes())
      
    } catch {  
      case e: Throwable => throw e
    } finally {
      channel.close()
      connection.close()
    }
  }
  
  def listExchanges() = {
    
  }
  
  def config(): JsValue = {
    Json.obj(
        "host" -> cnn.host,
        "port" -> cnn.port,
        "user" -> cnn.username,
        "pass" -> cnn.password,
        "heartbeat_ms" -> cnn.heartbeat)
  }
}

object AmqpClient {
  def apply(cnn: AmqpConnection) = new AmqpClient(cnn)
}

case class AmqpEndpoint(exchange: String, route: String)
  
case class AmqpConnection(
  host: String   = ConnectionFactory.DEFAULT_HOST,
  port: Int      = ConnectionFactory.DEFAULT_AMQP_PORT,
  username: String = ConnectionFactory.DEFAULT_USER,
  password: String = ConnectionFactory.DEFAULT_PASS,
  heartbeat: Int   = ConnectionFactory.DEFAULT_HEARTBEAT)  