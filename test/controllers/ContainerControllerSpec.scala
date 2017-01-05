package controllers

import java.util.UUID

import scala.concurrent.Future
import scala.util.Success
import scala.util.Try

import org.joda.time.DateTime
import org.mockito.Matchers.{ eq => meq }
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util.GestaltSecurityMocking
import javax.inject.Singleton
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification
import play.api.test.WithApplication


class ContainerControllerSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers {

  override def beforeAll(): Unit = pristineDatabase()

  sequential

  abstract class FakeSecurity extends WithApplication(containerApp()) {
  }
  import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
  object Ents extends AuthorizationMethods
  trait TestApplication extends FakeSecurity {

    val containerController = spy(app.injector.instanceOf[ContainerController])
    val resourceController = spy(app.injector.instanceOf[ResourceController])
    val deleteController = spy(app.injector.instanceOf[DeleteController])
    
    var Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")

    //containerController.setNewEnvironmentEntitlements(dummyRootOrgId, testEnv.id, user, testWork.id)
    Ents.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))
    
    var testProvider = createMarathonProvider(testEnv.id, "test-provider").get
    var mockMarathonClient = mock[MarathonClient]
    
    containerService.findWorkspaceEnvironment(testEnv.id) returns Try((testWork, testEnv))
    containerService.marathonProvider(testProvider.id) returns testProvider
    containerService.marathonClient(testProvider) returns mockMarathonClient    
    
//    override def around[T: AsResult](t: => T): org.specs2.execute.Result = super.around {
//
//      containerController.setNewEnvironmentEntitlements(dummyRootOrgId, testEnv.id, user, testWork.id)
//
//      testProvider = createMarathonProvider(testEnv.id, "test-provider").get
//      mockMarathonClient = mock[MarathonClient]
//
//      containerService.findWorkspaceEnvironment(testEnv.id) returns Try((testWork, testEnv))
//      containerService.marathonProvider(testProvider.id) returns testProvider
//      containerService.marathonClient(testProvider) returns mockMarathonClient
//      t
//    }
    
  }  
  
  //val in = new Loader(testAuthResponse, mock[GestaltSecurityConfig], mock[GestaltSecurityClient]) 

 
    "findMigrationRule" should {
  
      "find any container.migrate.* rules that are within the given scope" in new TestApplication {
        val org = newOrg(id = dummyRootOrgId)
        org must beSuccessfulTry
  
        val data = newDummyEnvironment(dummyRootOrgId)
  
        val controller = containerController
        controller.findMigrationRule(data("environment")).isEmpty must beTrue
  
        val (_, rule) = createEventRule(data("environment"), data("lambda"), "container.migrate.pre")
        ResourceFactory.findById(ResourceIds.RuleEvent, rule) must beSome
  
        val event = controller.findMigrationRule(data("environment"))
  
        event.isEmpty must beFalse
      }
    }
  
    "ContainerSpec" should {
  
      "be convertible to GestaltResourceInput with all fields" in {
        val testProps = ContainerSpec(
          name = "test-container",
          container_type = "DOCKER",
          image = "nginx:alpine",
          provider = ContainerSpec.InputProvider(id = uuid, name = Some("test-provider")),
          port_mappings = Seq(
            ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(80), name = Some("web")),
            ContainerSpec.PortMapping(protocol = "tcp", container_port = Some(443), name = Some("secure-web"))
          ),
          cpus = 1.0,
          memory = 128,
          disk = 0.0,
          num_instances = 1,
          network = Some("some-network"),
          cmd = Some("echo I am Groot! && sleep 3600"),
          constraints = Seq("hostname:UNIQUE"),
          accepted_resource_roles = Some(Seq("*")),
          args = Some(Seq("args","are","not","normally","valid","with","cmd")),
          force_pull = true,
          health_checks = Seq(),
          volumes = Seq(),
          labels = Map("blah" -> "blergh"),
          env = Map("foo" -> "bar"),
          user = Some("root")
        )
        val resourceInput = ContainerSpec.toResourcePrototype(testProps)
        val protoProps = resourceInput.properties.get
        protoProps.get("image") must beSome(Json.toJson(testProps.image))
        protoProps.get("container_type") must beSome(Json.toJson(testProps.container_type))
        protoProps.get("cpus") must beSome(Json.toJson(testProps.cpus))
        protoProps.get("memory") must beSome(Json.toJson(testProps.memory))
        protoProps.get("disk") must beSome(Json.toJson(testProps.disk))
        protoProps.get("num_instances") must beSome(Json.toJson(testProps.num_instances))
        protoProps.get("network") must beSome(Json.toJson(testProps.network.get))
        protoProps.get("cmd") must beSome(Json.toJson(testProps.cmd.get))
        protoProps.get("constraints") must beSome(Json.toJson(testProps.constraints))
        protoProps.get("port_mappings") must beSome(Json.toJson(testProps.port_mappings))
        protoProps.get("accepted_resource_roles") must beSome(Json.toJson(testProps.accepted_resource_roles.get))
        protoProps.get("args") must beSome(Json.toJson(testProps.args.get))
        protoProps.get("force_pull") must beSome(Json.toJson(testProps.force_pull))
        protoProps.get("health_checks") must beSome(Json.toJson(testProps.health_checks))
        protoProps.get("volumes") must beSome(Json.toJson(testProps.volumes))
        protoProps.get("labels") must beSome(Json.toJson(testProps.labels))
        protoProps.get("env") must beSome(Json.toJson(testProps.env))
        protoProps.get("user") must beSome(Json.toJson(testProps.user.get))
      }
  
      "be convertible to GestaltResourceInput with default fields" in {
        val testProps = ContainerSpec(
          name = "test-container",
          container_type = "DOCKER",
          image = "nginx:alpine",
          provider = ContainerSpec.InputProvider(id = uuid, name = Some("test-provider"))
        )
        val resourceInput = ContainerSpec.toResourcePrototype(testProps)
        val protoProps = resourceInput.properties.get
        protoProps.get("image") must beSome(Json.toJson(testProps.image))
        protoProps.get("container_type") must beSome(Json.toJson(testProps.container_type))
        protoProps.get("cpus") must beSome(Json.toJson(testProps.cpus))
        protoProps.get("memory") must beSome(Json.toJson(testProps.memory))
        protoProps.get("disk") must beSome(Json.toJson(testProps.disk))
        protoProps.get("num_instances") must beSome(Json.toJson(testProps.num_instances))
        protoProps.get("network") must_== testProps.network
        protoProps.get("cmd") must_== testProps.cmd
        protoProps.get("constraints") must beSome(Json.toJson(testProps.constraints))
        protoProps.get("accepted_resource_roles") must_== testProps.accepted_resource_roles
        protoProps.get("args") must_== testProps.args
        protoProps.get("force_pull") must beSome(Json.toJson(testProps.force_pull))
        protoProps.get("health_checks") must beSome(Json.toJson(testProps.health_checks))
        protoProps.get("volumes") must beSome(Json.toJson(testProps.volumes))
        protoProps.get("labels") must beSome(Json.toJson(testProps.labels))
        protoProps.get("env") must beSome(Json.toJson(testProps.env))
        protoProps.get("user") must_== testProps.user
      }
  
      "be convertible from GestaltResourceInstance container representation" in new TestApplication {
        val testContainerName = "test-container"
        val testProps = ContainerSpec(
          name = testContainerName,
          container_type = "DOCKER",
          image = "nginx:alpine",
          provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
          port_mappings = Seq(),
          cpus = 1.0,
          memory = 128,
          disk = 0.0,
          num_instances = 1,
          network = None,
          cmd = None,
          constraints = Seq(),
          accepted_resource_roles = None,
          args = None,
          force_pull = false,
          health_checks = Seq(),
          volumes = Seq(),
          labels = Map("SOME" -> "LABEL"),
          env = Map(),
          user = None
        )
        val createdResource = createInstance(ResourceIds.Container, testContainerName,
          parent = Some(testEnv.id),
          properties = Some(Map(
            "container_type" -> "DOCKER",
            "provider" -> Output.renderInstance(testProvider).toString,
            "image" -> testProps.image,
            "labels" -> Json.obj("SOME" -> "LABEL").toString
          ))
        ).get
        val created = DateTime.parse(createdResource.created.get("timestamp"))
        ContainerSpec.fromResourceInstance(createdResource) must beSuccessfulTry(testProps.copy(
          created = Some(created)
        ))
      }
  
      "throw an exception if attempting to create from non-Container resource" in new TestApplication {
        val testResourceName = "some-name"
        val createdResource = createInstance(ResourceIds.Org, testResourceName,
          parent = Some(testEnv.id),
          properties = None
        ).get
        ContainerSpec.fromResourceInstance(createdResource) must beFailedTry.withThrowable[RuntimeException]("cannot convert non-Container resource into ContainerSpec")
      }
  
    }


  "ContainerController" should {

    "get a container via the ContainerService interface" in new TestApplication {

      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = Some("BRIDGE"),
        cmd = None,
        constraints = Seq(),
        accepted_resource_roles = None,
        args = None,
        force_pull = false,
        health_checks = Seq(),
        volumes = Seq(),
        labels = Map(),
        env = Map(),
        user = None)

      val testContainerName = "test-container"
      val testContainer = createInstance(
        ResourceIds.Container,
        testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get))).get
      
        containerService.findEnvironmentContainerByName("root", testEnv.id, testContainer.name) returns Future(Some(testContainer -> Seq.empty))
      
      // Set entitlements on new container
      val ces = Ents.setNewEntitlements(dummyRootOrgId, testContainer.id, user, Some(testEnv.id))
      ces exists { _.isFailure } must beFalse

      containerService.findEnvironmentContainerByName(
        any[String],
        any[UUID],
        any[String]) returns Future(Some(testContainer -> Seq.empty))
      
      val path = s"/root/environments/${testEnv.id}/containers/${testContainer.id}"
      val request = fakeAuthRequest(GET, path, testCreds)

      val Some(result) = route(request)
      
      contentAsString(result) must /("id" -> testContainer.id.toString)
      status(result) must equalTo(OK)
    }


    
    "list containers via the ContainerService interface" in new TestApplication {

        
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = "",
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = Some("BRIDGE"),
        cmd = None,
        constraints = Seq(),
        accepted_resource_roles = None,
        args = None,
        force_pull = false,
        health_checks = Seq(),
        volumes = Seq(),
        labels = Map(),
        env = Map(),
        user = None)        
      val testContainer = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get))).get

      val ces = Ents.setNewEntitlements(
        dummyRootOrgId, testContainer.id, user, Some(testEnv.id))
      ces exists { _.isFailure } must beFalse

      containerService.listEnvironmentContainers(
        "root",
        testEnv.id) returns Future(Seq(testContainer -> Seq.empty))

      val path = s"/root/environments/${testEnv.id}/containers?expand=true"
      val request = fakeAuthRequest(GET, path, testCreds)

      val Some(result) = route(request)

      contentAsString(result) must /#(0) / ("id" -> testContainer.id.toString)
      status(result) must equalTo(OK)

    }

    "create containers using the ContainerService interface" in new TestApplication {
      val testContainerName = "test-container"
      val testProps = ContainerSpec(
        name = testContainerName,
        container_type = "DOCKER",
        image = "nginx",
        provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
        port_mappings = Seq(ContainerSpec.PortMapping("tcp", Some(80), None, None, None, None)),
        cpus = 1.0,
        memory = 128,
        disk = 0.0,
        num_instances = 1,
        network = Some("BRIDGE"),
        cmd = None,
        constraints = Seq(),
        accepted_resource_roles = None,
        args = None,
        force_pull = false,
        health_checks = Seq(),
        volumes = Seq(),
        labels = Map(),
        env = Map(),
        user = None)

      val createdResource = createInstance(ResourceIds.Container, testContainerName,
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> testProps.container_type,
          "image" -> testProps.image,
          "provider" -> Output.renderInstance(testProvider).toString,
          "cpus" -> testProps.cpus.toString,
          "memory" -> testProps.memory.toString,
          "disk" -> testProps.disk.toString,
          "num_instances" -> testProps.num_instances.toString,
          "force_pull" -> testProps.force_pull.toString,
          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
          "network" -> testProps.network.get))).get

      containerService.launchContainer(
        meq("root"),
        meq(testWork),
        meq(testEnv),
        any[AuthAccountWithCreds],
        meq(testProps),
        any[Option[UUID]]) returns Future(createdResource -> Seq.empty)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
        Output.renderInstance(createdResource))

      val Some(result) = route(request)

      status(result) must equalTo(CREATED)

      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(createdResource.id)
      (json \ "name").asOpt[String] must beSome(testContainerName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Container")
      (json \ "properties").asOpt[ContainerSpec] must beSome(testProps.copy(name = ""))

      //there was one(containerController).createContainer(anyString, any[UUID])
      there was one(containerService).launchContainer(anyString, meq(testWork), meq(testEnv), any[AuthAccountWithCreds], any[ContainerSpec], any[Option[UUID]])
    }
    
        "create containers using the ContainerService interface with specific ID" in new TestApplication {
          val testContainerName = "test-container"
          val testProps = ContainerSpec(
            name = testContainerName,
            container_type = "DOCKER",
            image = "nginx",
            provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
            port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
            cpus = 1.0,
            memory = 128,
            disk = 0.0,
            num_instances = 1,
            network = Some("BRIDGE"),
            cmd = None,
            constraints = Seq(),
            accepted_resource_roles = None,
            args = None,
            force_pull = false,
            health_checks = Seq(),
            volumes = Seq(),
            labels = Map(),
            env = Map(),
            user = None
          )
          
          val testUUID = uuid()
          val createdResource = createInstance(ResourceIds.Container, testContainerName,
            parent = Some(testEnv.id),
            properties = Some(Map(
              "container_type" -> testProps.container_type,
              "image" -> testProps.image,
              "provider" -> Output.renderInstance(testProvider).toString,
              "cpus" -> testProps.cpus.toString,
              "memory" -> testProps.memory.toString,
              "disk" -> testProps.disk.toString,
              "num_instances" -> testProps.num_instances.toString,
              "force_pull" -> testProps.force_pull.toString,
              "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
              "network" -> testProps.network.get
            )),
            id = testUUID
          ).get
          containerService.launchContainer(
            meq("root"),
            meq(testWork),
            meq(testEnv),
            any[AuthAccountWithCreds],
            meq(testProps),
            meq(Some(testUUID))
          ) returns Future(createdResource -> Seq.empty)
    
          val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/containers", testCreds).withBody(
            Output.renderInstance(createdResource)
          )
          val Some(result) = route(request)
          status(result) must equalTo(CREATED)
          val json = contentAsJson(result)
          (json \ "id").asOpt[UUID] must beSome(testUUID)
          (json \ "name").asOpt[String] must beSome(testContainerName)
          (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Container")
          (json \ "properties").asOpt[ContainerSpec] must beSome(testProps.copy(name = ""))
    
          //there was one(containerController).createContainer(anyString, any[UUID])
          there was one(containerService).launchContainer(anyString, meq(testWork), meq(testEnv), any[AuthAccountWithCreds], any[ContainerSpec], meq(Some(testUUID)))
        }
        
    
        "delete containers using the ContainerService interface" in new TestApplication {
          val testContainerName = "test-container"
          val testProps = ContainerSpec(
            name = testContainerName,
            container_type = "DOCKER",
            image = "nginx",
            provider = ContainerSpec.InputProvider(id = testProvider.id, name = Some(testProvider.name)),
            port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
            cpus = 1.0,
            memory = 128,
            disk = 0.0,
            num_instances = 1,
            network = Some("BRIDGE"),
            cmd = None,
            constraints = Seq(),
            accepted_resource_roles = None,
            args = None,
            force_pull = false,
            health_checks = Seq(),
            volumes = Seq(),
            labels = Map(),
            env = Map(),
            user = None
          )
          val extId = s"/some/dummy/external/id"
          val createdResource = createInstance(ResourceIds.Container, testContainerName,
            parent = Some(testEnv.id),
            properties = Some(Map(
              "container_type" -> testProps.container_type,
              "image" -> testProps.image,
              "provider" -> Output.renderInstance(testProvider).toString,
              "cpus" -> testProps.cpus.toString,
              "memory" -> testProps.memory.toString,
              "disk" -> testProps.disk.toString,
              "num_instances" -> testProps.num_instances.toString,
              "force_pull" -> testProps.force_pull.toString,
              "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
              "network" -> testProps.network.get,
              "external_id" -> s"${extId}"
            ))
          ).get
    
          containerService.deleteContainer(
            createdResource
          ) returns Future(())
    
          val request = fakeAuthRequest(DELETE, 
              s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds)
    
          val Some(result) = route(request)
    
          status(result) must equalTo(NO_CONTENT)
    
          //there was one(deleteController).hardDeleteResource("root", s"environments/${testEnv.id}/containers/${createdResource.id}")
          there was one(containerService).deleteContainer(
            argThat(
              (r: GestaltResourceInstance) => r.id == createdResource.id && r.properties.flatMap(_.get("external_id")).contains(extId)
            )
          )
        }

  }

}