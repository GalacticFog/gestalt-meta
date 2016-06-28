package com.galacticfog.gestalt.meta.policy


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._

import org.joda.time.DateTime

import org.specs2.specification.Scope
import java.util.UUID

import scala.util.{Try,Success,Failure}


trait ResourceScope extends Scope {
  
  val dummyRootOrgId = UUID.randomUUID()
  val dummyOwner = ResourceOwnerLink(ResourceIds.Org, dummyRootOrgId.toString)
  
  def newInstance(typeId: UUID, name: String, id: UUID = uuid(), 
      owner: ResourceOwnerLink = dummyOwner, 
      org: UUID = dummyRootOrgId, 
      properties: Option[Hstore] = None) = {
    
    GestaltResourceInstance(
      id = id,
      typeId = typeId,
      state = uuid(),
      orgId = org,
      owner = owner,
      name = name,
      properties = properties)
  }
  
  def uuid() = UUID.randomUUID
  
  object DummyProps extends ResourceProperties {
    
    override val properties = baseResourceProperties ++ Map(
      "alpha" -> "foo", "bravo" -> "bar")
    
    def getValue(res: ResourceLike, propertyName: String): Try[String] = Try {
      ???
    }
    
  }
  
  
}


class ResourcePropertiesSpec extends Specification {
  
  "ResourceProperties" should {
    
    "exists" should {
      
      "return TRUE when the given property exists in the given Resource property collection" in new ResourceScope {
        
        val res = newInstance(uuid, "foo")
        
        EnvironmentProperties.exists("id") must beTrue
        EnvironmentProperties.exists("containers.count") must beTrue
        EnvironmentProperties.exists("container.cpus") must beTrue
        EnvironmentProperties.exists("container.memory") must beTrue
        EnvironmentProperties.exists("container.numInstances") must beTrue
        EnvironmentProperties.exists("container.image") must beTrue
        EnvironmentProperties.exists("container.user") must beTrue
        EnvironmentProperties.exists("container.acceptedResourceRoles") must beTrue
        EnvironmentProperties.exists("container.labels") must beTrue
        EnvironmentProperties.exists("container.constraints") must beTrue
        
      }
      
    }
    
  }
  
}

