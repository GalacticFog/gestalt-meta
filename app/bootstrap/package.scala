//
//
//import play.api.libs.json._
//
//import com.galacticfog.gestalt.data._
//import com.galacticfog.gestalt.data.util._
//import com.galacticfog.gestalt.data.models._
//
//import org.joda.time.DateTime
//import scalikejdbc._
//
//import java.util.UUID
//
//import org.flywaydb.core.Flyway
//import org.apache.commons.dbcp2.BasicDataSource
//
//import controllers.util.db._
//
//
//
//package object bootstrap {
//  
//  val rootOrgId  = "8a611fab-c2c8-4977-8abf-da2d02225971"
//  val rootUserId = "299ee3cb-5145-46fd-a0bb-889da788ca5a"
//  
//  val dummyOwner       = ResourceOwnerLink(ResourceIds.Org, rootOrgId.toString)  
//
//  
//  /**
//   * Type/Resource Helper Functions
//   */
//  def newResourceType
//      (org: UUID, owner: ResourceOwnerLink)
//      (id: UUID = uuid, name: String, 
//          description: Option[String] = None,
//          properties: Option[Hstore] = None,
//          parent: Option[UUID] = None) = {
//    
//    TypeFactory.create(rootOrgId)(GestaltResourceType(
//        id = id, 
//        extend = parent,
//        orgId = org, 
//        owner = owner,
//        name = name,
//        description = description))
//  }
//  
//  
//  def newTestResourceType(typeId: UUID, name: String) = {
//    TypeFactory.create(
//      id = typeId,
//      orgId = rootOrgId,
//      owner = dummyOwner,
//      name = name)
//  }
//  
//  
//  def newTestInstance(typeId: UUID, name: String, props: Option[Hstore]) = {
//    ResourceFactory.create {
//      GestaltResourceInstance(
//        id = uuid(),
//        typeId = typeId,
//        orgId = rootOrgId,
//        owner = dummyOwner,
//        name = name,
//        properties = props)
//    }
//  }    
//  
//  
//  def newTestProperty(name: String, datatype: String, appliesTo: UUID, 
//      requirement: UUID, refersTo: Option[UUID] = None) = {
//    
//    PropertyFactory.create(rootUserId) {
//      GestaltTypeProperty(
//        requirementType = requirement,        
//        orgId = rootOrgId,
//        owner = dummyOwner,
//        
//        datatype = DataType.id(datatype),        
//        appliesTo = appliesTo,
//        refersTo = refersTo,
//        name = name )
//    }    
//  }
//  
//  def newProperty
//      (org: UUID, owner: ResourceOwnerLink)
//      (name: String, datatype: String, requirement: UUID, 
//          appliesTo: UUID, refersTo: Option[UUID] = None) = {
//    
//    PropertyFactory.create(rootUserId) {
//      GestaltTypeProperty(
//        isSystem = false,
//        requirementType = requirement,//RequirementType.id(requirement),        
//        orgId = org,
//        owner = owner,
//        
//        datatype = DataType.id(datatype),        
//        appliesTo = appliesTo,
//        refersTo = refersTo,
//        name = name )
//    }    
//  }
//  
//  def newRequiredProperty
//      (org: UUID, owner: ResourceOwnerLink)
//      (name: String, datatype: String, appliesTo: UUID, refersTo: Option[UUID] = None) = {
//    
//    newProperty(org, owner)(name, datatype, RequirementType.id("required"), appliesTo, refersTo)
//  }
//  
//  def newOptionalProperty
//      (org: UUID, owner: ResourceOwnerLink)
//      (name: String, datatype: String, appliesTo: UUID, refersTo: Option[UUID] = None) = {
//    
//    newProperty(org, owner)(name, datatype, RequirementType.id("required"), appliesTo, refersTo)
//  }
//  
//
//  
//  def newRequiredSystemProperty(name: String, datatype: String, appliesTo: UUID, 
//      refersTo: Option[UUID] = None) = {
//    
//    PropertyFactory.create(rootUserId) {
//      GestaltTypeProperty(
//        isSystem = true,
//        requirementType = RequirementType.id("required"),        
//        orgId = rootOrgId,
//        owner = dummyOwner,
//        
//        datatype = DataType.id(datatype),        
//        appliesTo = appliesTo,
//        refersTo = refersTo,
//        name = name )
//    }
//  }
//  
//  
//  def newRequiredTestProperty(name: String, datatype: String, appliesTo: UUID, 
//      refersTo: Option[UUID] = None) = {
//    newTestProperty(name, datatype, appliesTo, RequirementType.id("required"), refersTo)
//  }
//  
//  def newOptionalTestProperty(name: String, datatype: String, 
//      appliesTo: UUID, refersTo: Option[UUID] = None) = {
//    newTestProperty(name, datatype, appliesTo, RequirementType.id("optional"), refersTo)
//  }    
//  
//}