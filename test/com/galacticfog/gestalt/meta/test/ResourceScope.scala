package com.galacticfog.gestalt.meta.test


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._

import org.joda.time.DateTime

import org.specs2.specification.Scope
import java.util.UUID

import scala.util.{Try,Success,Failure}
import play.api.libs.json.Json
import com.galacticfog.gestalt.security.api.{GestaltDirectory,GestaltAccount, GestaltAPICredentials}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import controllers.util.db.ConnectionManager


trait ResourceScope extends Scope {
  
  val dummyRootOrgId = UUID.randomUUID()
  val dummyOwner = ResourceOwnerLink(ResourceIds.Org, dummyRootOrgId.toString)
  val adminUserId = UUID.randomUUID()
  
  def pristineDatabase() = {
    controllers.util.db.EnvConfig.getConnection()

    val owner = ResourceOwnerLink(ResourceIds.User, adminUserId)
    val db = new Bootstrap(ResourceIds.Org, 
        dummyRootOrgId, dummyRootOrgId, owner, 
        ConnectionManager.currentDataSource())

    for {
      a <- db.clean
      b <- db.migrate
      c <- db.loadReferenceData
      d <- db.loadSystemTypes
      e <- db.initialize("root")
    } yield e    
  } 
  
  def newOrg(
      id: UUID = uuid(), 
      name: String = uuid.toString, 
      parent: UUID = dummyRootOrgId,
      properties: Option[Map[String,String]] = None) = {
    
    val fqprop = Map("fqon" -> name)
    val props = properties.fold(Option(fqprop))(
        p => Option(p ++ fqprop )
    )
    
    createInstance(ResourceIds.Org, name,
        parent = Option(parent),
        properties = props)
  }
  
  def newDummyEnvironment(
      org: UUID = dummyRootOrgId, 
      children: Seq[UUID] = Seq(),
      properties: Map[String,Map[String,String]] = Map()): Map[String,UUID] = {
    
    val (wid, eid) = createWorkspaceEnvironment(org,
        workspaceProps   = if (properties.contains("workspace")) properties("workspace") else Map.empty,
        environmentProps = if (properties.contains("environment")) properties("environment") else Map.empty)
        
    val container = newDummyContainer(eid)
    val lambda = newDummyLambda(eid)

    Map(
      "workspace" -> wid,
      "environment" -> eid,
      "container" -> container.get.id,
      "lambda" -> lambda.get.id)
  }
  
  def newDummyGateway(env: UUID, name: String = uuid()) = {
    createInstance(ResourceIds.ApiGatewayProvider, 
        name,
        parent = Some(env),
        properties = Some(Map(
            "parent" -> getParent(env))))
  }
  
  def createEventRule(
      parent: UUID,  
      lambda: UUID,
      action: String = "foo.bar",
      org: UUID = dummyRootOrgId) = {
    
    val output = createInstance(ResourceIds.Policy,
        org = org,
        name = uuid,
        parent = Option(parent)) map { policy =>
        val rule = createInstance(ResourceIds.RuleEvent,
            org = org,
            name = uuid,
            parent = Option(policy.id),
            properties = Option(Map(
                "actions"    -> Json.toJson(List(action)).toString,
                "defined_at" -> "foo",
                "lambda" -> lambda.toString,
                "parent"     -> policy.id.toString)))
        (policy.id, rule.get.id)
    }
    output.get
  }
  
  
  /*
  schema           : string        
  config           : json_object   
  locations        : array_json    
  linked_providers : array_json    
  environments     : array_string  
  parent           : json_object   : [required]
   */
  
  
  def createMarathonProvider(parent: UUID, name: String = uuid.toString) = {
    createInstance(ResourceIds.MarathonProvider, name,
        parent = Option(parent),
        properties = Option(Map("parent" -> "{}")))
  }
  
  def newDummyLambda(env: UUID, gateway: UUID = uuid()) = {
    createInstance(ResourceIds.Lambda, uuid(),
      parent = Some(env),
      properties = Some(Map(
          "providers" -> Json.stringify(Json.toJson(List(Json.obj("id" -> gateway.toString)))),
          "public" -> "false",
          "runtime" -> "none",
          "code_type" -> "none",
          "memory"  -> "1024",
          "cpus"    -> "1.0",
          "timeout" -> "0" )))
  }
  
  val defaultContainerProps = Map(
        "container_type" -> "foo",
        "image" -> "bar",
        "provider" -> "{}")
        
  def newDummyContainer(env: UUID, props: Map[String,String] = defaultContainerProps) = {
    createInstance(ResourceIds.Container, uuid(),
      parent = Some(env),
      properties = Option(props))
  }  
  
  def dummyAuthAccountWithCreds(
      userInfo: Map[String,String] = Map(), 
      authHeader: Option[String] = None): AuthAccountWithCreds = {
    
    val defaultStr  = "foo"
    val header      = authHeader getOrElse s"Bearer ${uuid()}"
    val credentials = GestaltAPICredentials.getCredentials(header).get    
    val directory   = GestaltDirectory(uuid(), defaultStr, None, uuid())

    val account = GestaltAccount(
        userInfo.get("id") map (UUID.fromString(_)) getOrElse uuid(),
        userInfo.getOrElse("username",  defaultStr), 
        userInfo.getOrElse("firstName", defaultStr),
        userInfo.getOrElse("lastName",  defaultStr),
        userInfo.get("description") orElse Option(defaultStr),
        userInfo.get("email")       orElse Option(defaultStr),
        userInfo.get("phoneNumber") orElse Option(defaultStr),
        directory)
        
    AuthAccountWithCreds(account, Seq(), Seq(), credentials, uuid())
  }
  
  def createPolicyRule(parent: UUID, ruleType: UUID = ResourceIds.RuleLimit, action: String = "foo.bar", org: UUID = dummyRootOrgId) = {
    val output = createInstance(ResourceIds.Policy,
        org = org,
        name = uuid,
        parent = Option(parent)) map { policy =>
        val rule = createInstance(ruleType,
            org = org,
            name = uuid,
            parent = Option(policy.id),
            properties = Option(Map(
                "actions"    -> Json.toJson(List(action)).toString,
                "defined_at" -> "foo",
                "parent"     -> policy.id.toString)))
        (policy.id, rule.get.id)
    }
    output.get
  }
  
  def createDummyGateway(env: UUID, name: String = uuid(), org: UUID = dummyRootOrgId) = {
    createInstance(ResourceIds.ApiGatewayProvider, 
        name,
        org = org,
        parent = Some(env),
        properties = Some(Map(
            "parent" -> getParent(env))))
  }
  
  def getParent(id: UUID) = {
    Json.stringify(Json.obj("id" -> id.toString))
  }
  
  
  def getOrg(fqon: String) = ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon)

  def createWorkEnv(org: UUID = dummyRootOrgId, workspaceProps: Map[String,String] = Map(), environmentProps: Map[String,String] = Map(), wrkName: String = uuid(), envName: String = uuid()): Try[(Instance, Instance)] = {
    for {
      w <- createInstance(ResourceIds.Workspace, wrkName,
        org = org,
        parent = Option(org),
        properties = Option(workspaceProps))
      e <- createInstance(ResourceIds.Environment, envName,
        org = org,
        parent = Option(createInstance(ResourceIds.Workspace, wrkName,
          org = org,
          parent = Option(org),
          properties = Option(workspaceProps)).get.id),
        properties = Option(
          (environmentProps ++ Map("workspace" -> createInstance(ResourceIds.Workspace, wrkName,
            org = org,
            parent = Option(org),
            properties = Option(workspaceProps)).get.id.toString,
            "environment_type" -> EnvironmentType.id("test").toString))))
    } yield (w, e)
  }

  def createWorkspaceEnvironment(org: UUID = dummyRootOrgId, workspaceProps: Map[String,String] = Map(), environmentProps: Map[String,String] = Map(), wrkName: String = uuid(), envName: String = uuid()): (UUID,UUID) = {

    val wrk1 = createInstance(ResourceIds.Workspace, wrkName,
        org = org,
        parent = Option(org),
        properties = Option(workspaceProps))

    val env1 = createInstance(ResourceIds.Environment, envName,
        org = org,
        parent = Option(wrk1.get.id),
        properties = Option(
            (environmentProps ++ Map("workspace" -> wrk1.get.id.toString,
                "environment_type" -> EnvironmentType.id("test").toString))))

    val result = for {
      w <- wrk1
      e <- env1
    } yield (w.id, e.id)
    result.get

  }  
  
  def testUser(org: UUID, name: String = uuid.toString) = {
    newInstance(ResourceIds.User, name, org = org, properties = Some(Map(
        "email" -> "foo",
        "firstName" -> "alpha",
        "lastName" -> "omega",
        "phoneNumber" -> "555-555-5555",
        "password" -> "secret")))
  }  
  
  def createNewUser(org: UUID, id: UUID = uuid(), name: String = uuid.toString) = {
    ResourceFactory.create(ResourceIds.Org, org)(
      GestaltResourceInstance(
        id = id,
        typeId = ResourceIds.User,
        orgId = org,
        owner = ResourceOwnerLink(ResourceIds.Org, org.toString),
        name = name,
        state = ResourceState.id(ResourceStates.Active),
        properties = Some(Map( 
          "email" -> s"$name@example.com", "phoneNumber" -> "555", "firstName" -> "foo", "lastName" -> "bar"
      )))).get
  }
  

  def createInstance(typeId: UUID, name: String, id: UUID = uuid(), owner: ResourceOwnerLink = dummyOwner, org: UUID = dummyRootOrgId, properties: Option[Hstore] = None, parent: Option[UUID] = None): Try[GestaltResourceInstance] = {
    ResourceFactory.create(ResourceIds.Org, org)(
      newInstance(id = id, owner = owner, org = org, typeId = typeId, name = name, properties = properties), parent
    )
  }  
  
  def newInstance(typeId: UUID, name: String, id: UUID = uuid(), 
      owner: ResourceOwnerLink = dummyOwner, 
      org: UUID = dummyRootOrgId, 
      properties: Option[Hstore] = None) = {
    
    GestaltResourceInstance(
      id = id,
      typeId = typeId,
      orgId = org,
      owner = owner,
      name = name,
      state = ResourceState.id(ResourceStates.Active),
      properties = properties)
  }
  
  def uuid() = UUID.randomUUID
  
}