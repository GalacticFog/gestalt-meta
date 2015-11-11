package com.galacticfog.gestalt.meta.api

/*
  id          UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  resource_type_id    UUID REFERENCES resource_type,
  resource_state_id   UUID NOT NULL,
  org_id        UUID NOT NULL,
  owner         HSTORE NOT NULL,
  name          VARCHAR NOT NULL,
  description       VARCHAR,
  created         HSTORE NOT NULL,
  modified        HSTORE NOT NULL,
  
  is_sealed            BOOLEAN DEFAULT FALSE,  -- can the value be overridden in sub-types?
  is_system            BOOLEAN DEFAULT FALSE,  -- value 'true' means the property is not editable by users.
  
  applies_to       UUID REFERENCES resource_type,       -- type of resource the property is defined for.
  data_type_id         UUID REFERENCES data_type,
  requirement_type_id  UUID REFERENCES data_requirement_type, -- required, optional, etc.
  visibility_type_id   UUID REFERENCES data_visibility_type,  -- plain, encrypted, etc.
  default_value        VARCHAR
 */
import com.galacticfog.gestalt.data._
import java.util.UUID


case class GestaltTypePropertyInput(
    id: Option[UUID] = Some(UUID.randomUUID),
    name: String,
    data_type: String,
    requirement_type: String,
    applies_to: Option[UUID],
    
    refers_to: Option[UUID] = None,
    resource_state: Option[String] = Option(ResourceStates.Active),
    description: Option[String] = None,
    is_sealed: Option[Boolean] = Some(false),
    is_system: Option[Boolean] = Some(false),
    
    properties: Option[Hstore] = None,
    variables: Option[Hstore] = None,
    tags: Option[Hstore] = None,
    auth: Option[Hstore] = None,
    visibility_type: Option[String] = Some("plain"),
    default_value: Option[String] = None )

    