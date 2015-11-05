package com.galacticfog.gestalt.meta.api

import java.util.UUID

import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.ResourceStates
import com.galacticfog.gestalt.data.models.ResourceOwnerLink

case class GestaltResourceTypeInput(
  id: Option[UUID] = Some(UUID.randomUUID),
  extend: Option[UUID] = None,
  resource_state: Option[UUID] = Some(ResourceState.id(ResourceStates.Active)),
  owner: Option[ResourceOwnerLink] = None,
  name: String,
  description: Option[String] = None,
  
  created: Option[Hstore] = None,
  modified: Option[Hstore] = None,
  properties: Option[Hstore] = None,
  variables: Option[Hstore] = None,
  tags: Option[Hstore] = None,
  auth: Option[Hstore] = None,
  
  property_defs: Option[Seq[GestaltTypePropertyInput]] = None)

