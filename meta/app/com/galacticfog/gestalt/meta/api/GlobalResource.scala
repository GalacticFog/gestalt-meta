package com.galacticfog.gestalt.meta.api

import java.util.UUID
import scala.util.Try
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors._


object GlobalResource {
  
  def getResource(typeName: String, id: UUID): Try[GestaltResourceInstance] = Try {
    val typeId = Resource.typeOrElse(typeName)
    ResourceFactory.findById(typeId, id) getOrElse {
      throw new ResourceNotFoundException(s"Resource '$typeName/$id' not found.")
    }
  }

}