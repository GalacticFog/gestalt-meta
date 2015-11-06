package bootstrap



import play.api.libs.json._

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.util._
import com.galacticfog.gestalt.data.models._

import org.joda.time.DateTime
import scalikejdbc._

import java.util.UUID

import org.flywaydb.core.Flyway
import org.apache.commons.dbcp2.BasicDataSource

import controllers.util.db._


import scala.util.{Try,Success,Failure}

class DbUtil(org: UUID, rootOwner: UUID) {
  
  private val ownerLink = ResourceOwnerLink(ResourceIds.Org, rootOrgId.toString)
  private val flyway = new Flyway()
  
  flyway.setDataSource(datasource(ConnectionManager.config))  

  
  clean()
  migrate()
  createReferenceData()
  createBaseTypes()  
  
  
  private def datasource(info: ScalikePostgresInfo) = {
    val ds = new BasicDataSource();
    ds.setDriverClassName(info.driver);
    ds.setUsername(info.username.get);
    ds.setPassword(info.password.get);
    ds.setUrl(info.url());
    ds
  }

  def clean() = Try { flyway.clean() }
  
  def migrate() = Try { flyway.migrate() }
  
  def createReferenceData() = Try {
    /*
     * VISIBILITY TYPES
     */
    ReferenceFactory.createReferenceType(
        References.VisibilityType,
        None, rootOrgId, dummyOwner )("plain")    

    ReferenceFactory.createReferenceType(
        References.VisibilityType,
        None, rootOrgId, dummyOwner )("encrypted")


    /*
     * REQUIREMENT TYPES
     */
    ReferenceFactory.createReferenceType(
        References.RequirementType,
        None, rootOrgId, dummyOwner )("required")
    
    ReferenceFactory.createReferenceType(
        References.RequirementType,
        None, rootOrgId, dummyOwner )("required_wait")

    ReferenceFactory.createReferenceType(
        References.RequirementType,
        None, rootOrgId, dummyOwner )("optional")        
        
    /*
     * DATA TYPES
     */
    val newDataType = ReferenceFactory.createReferenceType(
      References.DataType,
      None, rootOrgId, dummyOwner ) _

    // TODO: create lists of these types
    newDataType("string")
    newDataType("int")
    newDataType("long")
    newDataType("float")
    newDataType("boolean")

    newDataType("uuid")
    newDataType("json")
    newDataType("resource::uuid")
    newDataType("resource::uuid::name")
    newDataType("resource::uuid::list")
    newDataType("resource::uuid::link")
    newDataType("resource::uuid::link::list")

    
    /*
     * NODE TYPE
     */
    ReferenceFactory.createReferenceType(
        References.NodeType,
        None, rootOrgId, dummyOwner )("web")
        
    ReferenceFactory.createReferenceType(
        References.NodeType,
        None, rootOrgId, dummyOwner )("db")        

    ReferenceFactory.createReferenceType(
        References.NodeType,
        None, rootOrgId, dummyOwner )("other")     
    
    /*
     * RESOURCE STATE
     */
    ReferenceFactory.createReferenceType(
        References.ResourceState,
        None, rootOrgId, dummyOwner )(ResourceStates.Active)
    ReferenceFactory.createReferenceType(
        References.ResourceState,
        None, rootOrgId, dummyOwner )(ResourceStates.Deleted)
    ReferenceFactory.createReferenceType(
        References.ResourceState,
        None, rootOrgId, dummyOwner )(ResourceStates.Disabled)        
        
        
    /*
     * ENVIRONMENT TYPE
     */
    ReferenceFactory.createReferenceType(
      References.EnvironmentType,
      None, rootOrgId, dummyOwner)("development")

    ReferenceFactory.createReferenceType(
      References.EnvironmentType,
      None, rootOrgId, dummyOwner)("test")
      
    ReferenceFactory.createReferenceType(
      References.EnvironmentType,
      None, rootOrgId, dummyOwner)("production")        
      
      
    /*
     * TASK STATUS TYPE      
     */
    ReferenceFactory.createReferenceType(
      References.TaskStatusType,
      None, rootOrgId, dummyOwner)("pending")
      
    ReferenceFactory.createReferenceType(
      References.TaskStatusType,
      None, rootOrgId, dummyOwner)("completed")
      
    ReferenceFactory.createReferenceType(
      References.TaskStatusType,
      None, rootOrgId, dummyOwner)("halted")
      
    ReferenceFactory.createReferenceType(
      References.TaskStatusType,
      None, rootOrgId, dummyOwner)("killed")
      
    ReferenceFactory.createReferenceType(
      References.TaskStatusType,
      None, rootOrgId, dummyOwner)("running")
      
    ReferenceFactory.createReferenceType(
      References.TaskStatusType,
      None, rootOrgId, dummyOwner)("scheduled")
  }  

  
  def createBaseTypes() = Try {

    TypeFactory.create(rootUserId)(
      GestaltResourceType(
        id = ResourceIds.ResourceType, 
        typeId = ResourceIds.ResourceType,
        orgId = rootOrgId,
        owner = dummyOwner,
        name = "Gestalt::Resource::Type"))
        
    TypeFactory.create(rootUserId)(
      GestaltResourceType(
        id = ResourceIds.Org, //orgTypeId,
        typeId = ResourceIds.ResourceType,
        orgId = rootOrgId,
        owner = dummyOwner,
        name = "Gestalt::Resource::Org"))

    /*
     * TypeProperty Resource - must be create first!
     */
    TypeFactory.create(rootUserId) {
      GestaltResourceType(
        id = ResourceIds.TypeProperty,
        typeId = ResourceIds.ResourceType,
        orgId = rootOrgId,
        owner = dummyOwner,
        name = "Gestalt::Resource::TypeProperty")
    }

    PropertyFactory.create(rootUserId) {
      GestaltTypeProperty(
        /*isSealed = true,*/ 
        isSystem = true,
        datatype = DataType.id("string"),
        requirementType = RequirementType.id("optional"),
        orgId = rootOrgId,
        owner = dummyOwner,
        appliesTo = ResourceIds.Org,
        name = "fqon" )
    }
    
    PropertyFactory.create(rootUserId) {
      GestaltTypeProperty( 
        isSystem = true,
        datatype = DataType.id("resource::uuid::link"),
        requirementType = RequirementType.id("optional"),
        orgId = rootOrgId,
        owner = dummyOwner,
        appliesTo = ResourceIds.Org,
        refersTo = Some(ResourceIds.Org),
        name = "parent" )
    }
    
    PropertyFactory.create(rootUserId) {
      GestaltTypeProperty( 
        isSystem = true,
        datatype = DataType.id("resource::uuid::link::list"),
        requirementType = RequirementType.id("optional"),
        orgId = rootOrgId,
        owner = dummyOwner,
        appliesTo = ResourceIds.Org,
        refersTo = Some(ResourceIds.Org),
        name = "children" )
    }        

    /*
     * BASE RESOURCE
     */
     TypeFactory.create(rootUserId)(
      GestaltResourceType(
        id = ResourceIds.Resource,
        typeId = ResourceIds.ResourceType,
        orgId = rootOrgId,
        owner = dummyOwner,
        name = "Gestalt::Resource"))
        
    PropertyFactory.create(rootUserId) {
      GestaltTypeProperty(
        isSealed = true, isSystem = true,
        datatype = DataType.id("resource::uuid::link"),
        requirementType = RequirementType.id("optional"),
        
        orgId = rootOrgId,
        owner = dummyOwner,
        appliesTo = ResourceIds.Resource,
        name = "extends" )      
    }
 
    /*
     * ORG Type and Properties
     */    

  }
  
  
//  
//  /**
//   * Type/Resource Helper Functions
//   */
//  def newResourceType
//      (org: UUID, owner: ResourceOwnerLink)
//      (id: UUID = uuid, name: String, description: Option[String] = None,
//        properties: Option[Hstore] = None, parent: Option[UUID] = None) = {
//
//    TypeFactory.create(
//      id = id,
//      extend = parent,
//      orgId = org,
//      owner = owner,
//      name = name,
//      description = description)
//  }
//
//  def newTestResourceType(typeId: UUID, name: String) = {
//    TypeFactory.create(
//      id = typeId,
//      orgId = org,
//      owner = ownerLink,
//      name = name)
//  }
//
//  def newTestInstance(typeId: UUID, name: String, props: Option[Hstore]) = {
//    ResourceFactory.create {
//      GestaltResourceInstance(
//        id = uuid(),
//        typeId = typeId,
//        orgId = org,
//        owner = ownerLink,
//        name = name,
//        properties = props)
//    }
//  }
//
//  
//  def newTestProperty(
//      name: String, datatype: String, appliesTo: UUID,
//      requirement: UUID, refersTo: Option[UUID] = None) = {
//
//    PropertyFactory.create(rootOwner) {
//      GestaltTypeProperty(
//        requirementType = requirement,
//        orgId = org,
//        owner = ownerLink,
//
//        datatype = DataType.id(datatype),
//        appliesTo = appliesTo,
//        refersTo = refersTo,
//        name = name)
//    }
//  }
//
//  
//  def newProperty
//      (org: UUID, owner: ResourceOwnerLink)
//      (name: String, datatype: String, requirement: UUID,
//        appliesTo: UUID, refersTo: Option[UUID] = None) = {
//
//    PropertyFactory.create(rootOwner) {
//      GestaltTypeProperty(
//        isSystem = false,
//        requirementType = requirement, //RequirementType.id(requirement),        
//        orgId = org,
//        owner = owner,
//
//        datatype = DataType.id(datatype),
//        appliesTo = appliesTo,
//        refersTo = refersTo,
//        name = name)
//    }
//  }
//
//  def newRequiredProperty
//      (org: UUID, owner: ResourceOwnerLink)
//      (name: String, datatype: String, appliesTo: UUID, refersTo: Option[UUID] = None) = {
//    newProperty(org, owner)(name, datatype, RequirementType.id("required"), appliesTo, refersTo)
//  }
//
//  def newOptionalProperty
//      (org: UUID, owner: ResourceOwnerLink)
//      (name: String, datatype: String, appliesTo: UUID, refersTo: Option[UUID] = None) = {
//    newProperty(org, owner)(name, datatype, RequirementType.id("required"), appliesTo, refersTo)
//  }
//
//  def newRequiredSystemProperty(
//      name: String, 
//      datatype: String,
//      appliesTo: UUID,
//      refersTo: Option[UUID] = None) = {
//    
//    PropertyFactory.create(rootOwner) {
//      GestaltTypeProperty(
//        isSystem = true,
//        requirementType = RequirementType.id("required"),
//        orgId = org,
//        owner = ownerLink,
//
//        datatype = DataType.id(datatype),
//        appliesTo = appliesTo,
//        refersTo = refersTo,
//        name = name)
//    }
//  }
//
//  def newRequiredTestProperty(
//      name: String, datatype: String, appliesTo: UUID,
//      refersTo: Option[UUID] = None) = {
//    newTestProperty(name, datatype, appliesTo, RequirementType.id("required"), refersTo)
//  }
//
//  def newOptionalTestProperty(
//      name: String, datatype: String,
//      appliesTo: UUID, refersTo: Option[UUID] = None) = {
//    newTestProperty(name, datatype, appliesTo, RequirementType.id("optional"), refersTo)
//  }

}

