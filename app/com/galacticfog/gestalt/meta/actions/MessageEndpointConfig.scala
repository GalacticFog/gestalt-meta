package com.galacticfog.gestalt.meta.actions

import java.util.UUID

case class MessageImplementation(kind: String, id: UUID)
case class MessageEndpointConfig(
    service_host: String,
    service_port: Option[Int],
    message_exchange: String,
    message_topic: String,
    response_topic: Option[String],
    implementation: MessageImplementation)  
      
    