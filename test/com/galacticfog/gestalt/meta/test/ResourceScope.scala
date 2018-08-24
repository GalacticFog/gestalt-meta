package com.galacticfog.gestalt.meta.test


import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.auth._
import com.galacticfog.gestalt.security.api.{DIRECTORY_TYPE_INTERNAL, GestaltAPICredentials, GestaltAccount, GestaltDirectory}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import modules.ProdSecurityModule
import org.specs2.specification.Scope
import play.api.libs.json.Json
import play.api.libs.json.Json.JsValueWrapper

import scala.util.{Failure, Success, Try}
import controllers.util.DataStore
import org.specs2.mock.Mockito
import play.api.inject.guice.GuiceApplicationBuilder
import migrations._

trait ResourceScope extends Scope with Mockito {
  println("*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*")
  println("RESOURCE SCOPE INIT")
  println("*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*~*")
  val dummyRootOrgId = UUID.randomUUID()
  val dummyOwner = ResourceOwnerLink(ResourceIds.Org, dummyRootOrgId.toString)
  val adminUserId = UUID.randomUUID()

  object Entitlements extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods

  lazy val appBuilder = new GuiceApplicationBuilder().disable(
    classOf[modules.HealthModule],
    classOf[ProdSecurityModule],
    classOf[modules.MetaDefaultServices]
  )
  private[this] lazy val injector = appBuilder.injector()
 
  def pristineDatabase() = {
    val dataStore = injector.instanceOf(classOf[DataStore])
    val owner = ResourceOwnerLink(ResourceIds.User, adminUserId)
    val db = new Bootstrap(ResourceIds.Org,
        dummyRootOrgId, dummyRootOrgId, owner, dataStore.dataSource)
    
    for {
      _ <- db.clean
      _ <- db.migrate
      _ <- db.loadReferenceData
      _ <- db.loadSystemTypes
      _ <- db.initialize("root")
      admin <- {
        val gestaltAdmin = GestaltResourceInstance(
          id     = adminUserId,
          typeId = ResourceIds.User,
          orgId  = dummyRootOrgId,
          owner  = ResourceOwnerLink(ResourceIds.User, adminUserId),
          name   = "root",
          state  = ResourceState.id(ResourceStates.Active),
          properties = Some(Map(
            "firstName"    -> "Rooty",
            "lastName"     -> "McRootFace",
            "email"        -> "",
            "phoneNumber"   -> "",
            "gestalt_home" -> "root"))
        )
        ResourceFactory.create(ResourceIds.User, adminUserId)(gestaltAdmin, Some(dummyRootOrgId))
      }
      _ <- {
        println("*** Applying migrations ***")
        /*
         * 
         * TODO: This temporarily hard-codes the migration to call. The process of automatically evolving
         * the meta-schema is very much 'in-progress'. Obviously this needs to be more flexible and will
         * need to follow the final pattern once that's established.
         * 
         */

        val migrations = Seq(
          new V1(),
          new V2(),
          new V3(),
          new V4(),
          new V5(),
          new V6(),
//          new V7(),
//          new V8(),
          new V9(),
          new V10(),
          new V12(),
          new V13()
        )

        val tries = migrations.map {
          m => m.migrate(admin.id, None) match {
            case Left(x) => Failure(new GenericApiException(500, s"Error running migration '${m.getClass.getSimpleName}': ${x}"))
            case Right(y) => Success(y)
          }
        }

        Try {
          tries.map(_.get)
        }
      }
    } yield ()
  }
  
  
  import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
  import com.galacticfog.gestalt.meta.api.Resource
  
  def rootOwnerLink() = {
    val rootid = Resource.findFqon("root").fold {
      throw new RuntimeException("Could not find Root Org.")
    }{ r => r.id }
    
    ResourceOwnerLink(ResourceIds.Org, rootid)
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
    createInstance(ResourceIds.GatewayManager, 
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
                "match_actions"    -> Json.toJson(List(action)).toString,
                "defined_at" -> policy.id.toString,
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


  def createDockerProvider(parent: UUID, name: String = uuid.toString) = {
    createInstance(ResourceIds.DockerProvider, name,
      parent = Option(parent),
      properties = Option(Map("parent" -> "{}")))
  }

   def createMarathonProvider(parent: UUID,
                              name: String = uuid.toString,
                              config: Seq[(String,JsValueWrapper)] = Seq.empty) = {
    createInstance(ResourceIds.DcosProvider, name,
      parent = Some(parent),
      properties = Some(Map(
        "parent" -> "{}",
        "config" -> Json.obj(config:_*).toString
      )))
  }

  def createKubernetesProvider(parent: UUID, name: String = uuid.toString, config: Seq[(String,JsValueWrapper)] = Seq.empty) = {
    createInstance(
      typeId = ResourceIds.KubeProvider,
      name = name,
      parent = Option(parent),
      properties = Option(Map(
        "parent" -> "{}",
        "config" -> Json.obj(config:_*).toString
      ))
    )
  }
  
  def newDummyLambda(env: UUID, gateway: UUID = uuid()) = {
    createInstance(ResourceIds.Lambda, uuid(),
      parent = Some(env),
      properties = Some(Map(
          "provider" -> Json.stringify(Json.toJson(Json.obj("id" -> gateway.toString))),
          "public" -> "false",
          "runtime" -> "none",
          "code_type" -> "none",
          "memory"  -> "1024",
          "cpus"    -> "1.0",
          "timeout" -> "0",
          "handler" -> "foo.js;foo",
          "compressed" -> "true")))
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
    val directory   = GestaltDirectory(uuid(), defaultStr, None, uuid(), directoryType = DIRECTORY_TYPE_INTERNAL.label)

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
        properties = Option(Map("parent" -> 
          Json.stringify(Json.obj("parent" -> Json.obj("id" -> parent.toString))))),
        parent = Option(parent)) map { policy =>
        val rule = createInstance(ruleType,
            org = org,
            name = uuid,
            parent = Option(policy.id),
            properties = Option(Map(
                "match_actions"    -> Json.toJson(List(action)).toString,
                "defined_at" -> "foo",
                "parent"     -> policy.id.toString)))
        (policy.id, rule.get.id)
    }
    output.get
  }

  def createDummyGateway(env: UUID, name: String = uuid(), org: UUID = dummyRootOrgId) = {
    createInstance(ResourceIds.GatewayManager,
      name,
      org = org,
      parent = Some(env),
      properties = Some(Map(
        "parent" -> getParent(env))))
  }

  def createDummyKong(env: UUID, name: String = uuid(), org: UUID = dummyRootOrgId, props: Map[String,String] = Map.empty) = {
    createInstance(ResourceIds.KongGateway,
        name,
        org = org,
        parent = Some(env),
        properties = Some(Map(
            "parent" -> getParent(env)
        ) ++ props)
    )
  }
  
  def getParent(id: UUID): String = {
    Json.stringify(Json.obj("id" -> id.toString))
  }

  def getOrg(fqon: String): Option[Instance] = Resource.findFqon(fqon)

  def createWorkEnv(org: UUID = dummyRootOrgId, workspaceProps: Map[String,String] = Map(), environmentProps: Map[String,String] = Map(), wrkName: String = uuid(), envName: String = uuid()): Try[(Instance, Instance)] = {
    for {
      w <- createInstance(ResourceIds.Workspace, wrkName,
        org = org,
        parent = Option(org),
        properties = Option(workspaceProps))
      e <- createInstance(ResourceIds.Environment, envName,
        org = org,
        parent = Option(w.id),
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
  
  def createMetaUser(auth: AuthAccountWithCreds): GestaltResourceInstance = {
    val org = auth.authenticatingOrgId
    val u = createNewUser(org, auth.account.id)
    Entitlements.setNewResourceEntitlements(org, u.id, auth, Some(org))
    u
  }
  
  def createNewUser(
      org: UUID = dummyRootOrgId, 
      id: UUID = uuid(), 
      name: String = uuid.toString, 
      properties: Option[Map[String,String]] = None) = {
    
    val props = properties getOrElse {
      Map( 
          "email" -> s"$name@${uuid.toString}.com", 
          "phoneNumber" -> "555", 
          "firstName" -> "foo", 
          "lastName" -> "bar")
    }
          
    ResourceFactory.create(ResourceIds.Org, org)(
      GestaltResourceInstance(
        id = id,
        typeId = ResourceIds.User,
        orgId = org,
        owner = ResourceOwnerLink(ResourceIds.Org, org.toString),
        name = name,
        state = ResourceState.id(ResourceStates.Active),
        properties = Some(props))).get
  }
  
  def createEntitlement(
      action: String,
      parent: UUID,
      identities: Option[Seq[UUID]] = None,
      name: String  = uuid.toString,
      org: UUID     = dummyRootOrgId,
      creator: UUID = dummyOwner.id) = {
    
    val e = Entitlements.newEntitlement(
        creator, 
        org, 
        parent, 
        action, 
        identities, 
        Some(name), 
        value = None, 
        description = None)
        
    Entitlement.toGestalt(creator, e)
  }
  
  
  
  /**
   * Create new entitlement in memory
   */
  def newEntitlement(
      action: String,
      parent: UUID,
      identities: Option[Seq[UUID]] = None,
      name: String  = uuid.toString,
      org: UUID     = dummyRootOrgId,
      creator: UUID = dummyOwner.id) = {
    
    Entitlements.newEntitlement(
        creator, 
        org, 
        parent, 
        action, 
        identities, 
        Some(name), 
        value = None, 
        description = None)
  }  

  def createApi(
     name: String = uuid.toString,
     id: UUID = uuid(),
     owner: ResourceOwnerLink = dummyOwner,
     org: UUID = dummyRootOrgId,
     properties: Option[Hstore] = None,
     parent: Option[UUID] = None,
     description: Option[String] = None): Try[GestaltResourceInstance] = {
    
    createInstance(ResourceIds.Api, name, id, owner, org, properties, parent, description)
  }

  def createApiEndpoint(
     parentApi: UUID,
     name: String = uuid.toString,
     id: UUID = uuid(),
     owner: ResourceOwnerLink = dummyOwner,
     org: UUID = dummyRootOrgId,
     properties: Option[Hstore] = None,
     description: Option[String] = None): Try[GestaltResourceInstance] = {
    
    createInstance(ResourceIds.ApiEndpoint, name, id, owner, org, properties, Some(parentApi), description)
  }  

  def createInstance(typeId: UUID,
                     name: String,
                     id: UUID = uuid(),
                     owner: ResourceOwnerLink = dummyOwner,
                     org: UUID = dummyRootOrgId,
                     properties: Option[Hstore] = None,
                     parent: Option[UUID] = None,
                     description: Option[String] = None): Try[GestaltResourceInstance] = {
    ResourceFactory.create(ResourceIds.Org, org)(
      newInstance(id = id, owner = owner, org = org, typeId = typeId, name = name, properties = properties, description = description), parent
    )
  }
  
  def newInstance(typeId: UUID, name: String, id: UUID = uuid(), 
      owner: ResourceOwnerLink = dummyOwner, 
      org: UUID = dummyRootOrgId, 
      properties: Option[Hstore] = None,
      description: Option[String] = None ): Instance = {

    GestaltResourceInstance(
      id = id,
      typeId = typeId,
      orgId = org,
      owner = owner,
      name = name,
      description = description,
      state = ResourceState.id(ResourceStates.Active),
      properties = properties)
  }
  
  def uuid() = UUID.randomUUID
  
  def save(r: GestaltResourceInstance, parentId: Option[UUID] = None): Try[GestaltResourceInstance] = {
    ResourceFactory.create(ResourceIds.Org, dummyRootOrgId)(r, parentId)
  }
  
  def createResourceType(
      name:        String, 
      org:         UUID = dummyRootOrgId,
      owner:       ResourceOwnerLink = dummyOwner,
      typeId:      UUID = UUID.randomUUID(), 
      description: Option[String] = None, 
      extend:      Option[UUID] = None,
      selfProps:   Map[String,String] = Map.empty,
      isAbstract:  Boolean = false) = {

    TypeFactory.create(org)(
      GestaltResourceType(
        extend = extend,
        typeId = ResourceIds.ResourceType,
        orgId = org,
        owner = owner,
        id = typeId,
        name = name,
        state = ResourceState.id(ResourceStates.Active),
        description = description,
        properties = Option(selfProps)),
        validate = false
    ).get
  }  
  
  /**
   * Create a new Org in Meta
   */
  def createOrg(name: String, id: UUID = uuid(), org: UUID = dummyRootOrgId, properties: Option[Hstore] = None, parent: Option[UUID] = None): Try[GestaltResourceInstance] = {
    createInstance(ResourceIds.Org, 
        name, id = id, org = org, properties = Some(properties.getOrElse(Map.empty) ++ Map(
        "fqon" -> name
      )), parent = parent)
  }
  
  def actionValue(res: GestaltResourceInstance): String = {
    res.properties.get("action")
  }
  
  def setEntitlements(parent: UUID, actions: (String,String)*): Try[Seq[UUID]] = Try {
    def go(acts: Seq[(String,String)], acc: Seq[UUID]): Seq[UUID] = {
      acts match {
        case Nil => acc
        case h :: t => {
          val e = createInstance(ResourceIds.Entitlement, uuid(),
                parent = Option(parent),
                properties = Option(Map(h._1 -> h._2)))
          go(t, e.get.id +: acc)
        }
      }
    }
    go(actions.toList, Seq.empty)
  }
  
  def setEntitlements2(parent: UUID, properties: (String, Seq[UUID])*): Try[Seq[UUID]] = Try {

    def go(props: Seq[(String, Seq[UUID])], acc: Seq[UUID]): Seq[UUID] = {
      
      props match {
        case Nil => acc
        case h :: t => {
          val actionprops: Map[String, String] = Map(
              "action" -> h._1, 
              "identities" -> Json.stringify(Json.toJson(h._2))
          )
          val e = createInstance(ResourceIds.Entitlement, 
                name = uuid.toString,
                parent = Option(parent),
                properties = Option(actionprops))
          go(t, e.get.id +: acc)
        }
      }
    }
    go(properties.toList, Seq.empty)
  }   
  
}
