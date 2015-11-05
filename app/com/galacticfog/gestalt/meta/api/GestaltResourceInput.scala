package com.galacticfog.gestalt.meta.api


import com.galacticfog.gestalt.data._
import java.util.UUID


case class GestaltResourceInput(
    name: String,
    resource_type: Option[UUID],
    id: Option[UUID] = Option(UUID.randomUUID),
    resource_state: Option[String] = Option(ResourceStates.Active),
    description: Option[String] = None,
    properties: Option[Hstore] = None,
    variables: Option[Hstore] = None,
    tags: Option[Hstore] = None,
    auth: Option[Hstore] = None)

