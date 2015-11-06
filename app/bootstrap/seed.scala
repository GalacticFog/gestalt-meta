package bootstrap


import controllers.util.db._
import org.flywaydb.core.Flyway
import org.apache.commons.dbcp2.BasicDataSource
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.util._
import com.galacticfog.gestalt.data.models._

import org.joda.time.DateTime
import scalikejdbc._

import java.util.UUID

import scala.util.{Try,Success,Failure}


object seed extends App {

  
  refreshDb()
  createReferenceData()
  createBaseTypes()
  
  
  def datasource(info: ScalikePostgresInfo) = {
    val ds = new BasicDataSource();
    ds.setDriverClassName(info.driver);
    ds.setUsername(info.username.get);
    ds.setPassword(info.password.get);
    ds.setUrl(info.url());
    ds
  }
  
  
  def refreshDb() = {
    val flyway = new Flyway()
    flyway.setDataSource(datasource(ConnectionManager.config))
    flyway.clean()
    
    val n = flyway.migrate()
    println(s"Applied ${n} migrations.")
  }
  
  
  def createReferenceData() = {
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

  
  def createBaseTypes() = {

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
  
}