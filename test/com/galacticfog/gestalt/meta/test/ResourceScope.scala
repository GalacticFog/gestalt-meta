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

trait ResourceScope extends Scope {
  
  
  val dummyRootOrgId = UUID.randomUUID()
  val dummyOwner = ResourceOwnerLink(ResourceIds.Org, dummyRootOrgId.toString)
  
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
  
  
  def createWorkspaceEnvironment(org: UUID = dummyRootOrgId, workspaceProps: Map[String,String] = Map(), environmentProps: Map[String,String] = Map()): (UUID,UUID) = {
    
    val wrk1 = createInstance(ResourceIds.Workspace, uuid(),
        org = org,
        parent = Option(org),
        properties = Option(workspaceProps))
    
    val env1 = createInstance(ResourceIds.Environment, uuid(),
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
      properties = properties)
  }
  
  def uuid() = UUID.randomUUID
  

}