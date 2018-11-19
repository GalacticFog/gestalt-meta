package com.galacticfog.gestalt.meta.validation

import java.util.UUID
import scala.util.{Try,Success}
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._


trait ResourceValidation[A] {
  def validate(resource: A, parentType: UUID, parentId: UUID): Try[A]
  def validate(resource: A, parent: UUID): Try[A]
}

object DefaultValidation extends ResourceValidation[ResourceLike] {
  
  def parentExists(parentType: UUID, parentId: UUID): Try[ResourceLike] = {
    ResourceFactory.findById(parentType, parentId).fold {
      throw new ResourceNotFoundException(
          s"Parent ${ResourceLabel(parentType)} with ID '${parentId}' not found.")
    }{ Success(_) }
  }
  
  def nameUniqueInParent(parent: UUID, resource: ResourceLike): Try[ResourceLike] = {
    ResourceFactory.findChildByName(parent, resource.typeId, resource.name).fold {
      Success(resource)
    }{ _ => 
      throw new ConflictException(    
        s"${ResourceLabel(resource.typeId)} with name '${resource.name}' already exists")
    }
  }
  
  def validate(resource: ResourceLike, parentType: UUID, parentId: UUID): Try[ResourceLike] = {
    for {
      parent <- parentExists(parentType, parentId)
      result <- nameUniqueInParent(parent.id, resource) 
    } yield result
  }
  
  def validate(resource: ResourceLike, parentId: UUID): Try[ResourceLike] = {
    for {
      result <- nameUniqueInParent(parentId, resource)
    } yield result
  }
}