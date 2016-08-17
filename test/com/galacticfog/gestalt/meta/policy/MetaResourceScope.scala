package com.galacticfog.gestalt.meta.policy

import java.util.UUID

import org.specs2.specification.Scope

import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.uuid
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceStates
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

trait MetaResourceScope extends Scope {
  
  val dummyRootOrgId = UUID.randomUUID()
  val dummyOwner = ResourceOwnerLink(ResourceIds.Org, dummyRootOrgId.toString)
  
  def newInstance(typeId: UUID, name: String, id: UUID = uuid(), 
      owner: ResourceOwnerLink = dummyOwner, 
      org: UUID = dummyRootOrgId, 
      properties: Option[Hstore] = None) = {
    GestaltResourceInstance(
      id = id,
      typeId = typeId,
      state = ResourceState.id(ResourceStates.Active),
      orgId = org,
      owner = owner,
      name = name,
      properties = properties)
  }
  
}