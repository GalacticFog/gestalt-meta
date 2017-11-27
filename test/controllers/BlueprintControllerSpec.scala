package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.actions.{ActionContext, ActionInvocation, ActionProvider, ActionProviderManager}
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test._
import org.mockito.Matchers.{eq => meq}
import org.specs2.execute.{AsResult, Result}
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.matcher.JsonMatchers
import play.api.inject.bind
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.Json
import play.api.test.{PlaySpecification, WithApplication}

import scala.concurrent.Future
import scala.util.Success

class BlueprintControllerSpec extends PlaySpecification with MetaRepositoryOps with JsonMatchers {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(_) = Ents.createNewMetaUser(user, dummyRootOrgId, user.account,
      Some(Map(
        "firstName" -> user.account.firstName,
        "lastName" -> user.account.lastName,
        "email" -> user.account.email.getOrElse(""),
        "phoneNumber" -> user.account.phoneNumber.getOrElse("")
      )),
      user.account.description
    )
  }

  sequential

  def appWithMocks() = application(additionalBindings = Seq(
    bind(classOf[ActionProviderManager]).toInstance(mock[ActionProviderManager])
  ))

  abstract class TestContainerController extends WithApplication(appWithMocks()) {
    var testWork: GestaltResourceInstance = null
    var testEnv: GestaltResourceInstance = null
    var providerManager: ActionProviderManager = null

    var testProvider: GestaltResourceInstance = null
    var mockActionProvider: ActionProvider = null

    override def around[T: AsResult](t: => T): Result = super.around {
      scalikejdbc.config.DBs.setupAll()

      val Success(wrkEnv) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
      testWork = wrkEnv._1
      testEnv = wrkEnv._2

      Ents.setNewEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

      val injector = app.injector
      providerManager = injector.instanceOf[ActionProviderManager]

      testProvider = createKubernetesProvider(testEnv.id, "test-kube-provider").get
      mockActionProvider = mock[ActionProvider]
      providerManager.getProvider(testProvider) returns Success(mockActionProvider)
      t
    }
  }

  stopOnFail

  "ActionProviderManager" should {

    "return the right class or something" in {
      ko("write me")
    }

  }

  "ActionProviderImpl" in {

    "post to lambdas appropriately" in {
      ko("write me")
    }

  }

  "BlueprintController" should {

    import com.galacticfog.gestalt.meta.api.errors._

    "create containers with non-existent provider should 400" in new TestContainerController {
      val testBlueprintName = "test-blueprint"

      val newResource = newInstance(
        typeId = ResourceIds.Blueprint,
        name = testBlueprintName,
        properties = Some(Map(
          "blueprint_type" -> "docker-compose",
          "native_form" -> "",
          "canonical_form" -> "",
          "provider" -> Json.obj("id" -> UUID.randomUUID()).toString
        ))
      )

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/blueprints", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(request)

      status(result) must equalTo(BAD_REQUEST)
      contentAsString(result) must contain("not found")
    }

    "create blueprints using the ActionProvider interface" in new TestContainerController {
      val testBlueprintName = "test-blueprint"

      val newResource = newInstance(
        typeId = ResourceIds.Blueprint,
        name = testBlueprintName,
        properties = Some(Map(
          "blueprint_type" -> "docker-compose",
          "provider" -> Json.obj("id" -> testProvider.id).toString,
          "native_form" -> "blueprint data goes here"
        ))
      )

      val returnedResource = newResource.copy(
        properties = Some(newResource.properties.get ++ Map(
          "canonical_form" -> "meta canonical form"
        ))
      )

      mockActionProvider.invokeAction(any) returns Future.successful(returnedResource)

      val request = fakeAuthRequest(POST, s"/root/environments/${testEnv.id}/blueprints", testCreds).withBody(
        Output.renderInstance(newResource)
      )

      val Some(result) = route(request)

      status(result) must equalTo(CREATED)

      val json = contentAsJson(result)
      (json \ "id").asOpt[UUID] must beSome(newResource.id)
      (json \ "name").asOpt[String] must beSome(testBlueprintName)
      (json \ "resource_type").asOpt[String] must beSome("Gestalt::Resource::Blueprint")
      (json \ "properties" \ "provider" \ "id").asOpt[UUID] must beSome(testProvider.id)
      (json \ "properties" \ "blueprint_type").asOpt[String] must beSome("docker-compose")
      (json \ "properties" \ "native_form").asOpt[String] must beSome("bluprint data goes here")
      (json \ "properties" \ "canonical_form").asOpt[String] must beSome("meta canonical form")

      there was one(mockActionProvider).invokeAction(meq(ActionInvocation(
        action = "blueprint.create",
        context = ActionContext(
          org = ResourceFactory.findById(dummyRootOrgId).get,
          workspace = Some(testWork),
          environment = Some(testEnv),
          originalResource = None
        ),
        provider = testProvider,
        resource = Some(newResource)
      )))
    }

//    "delete containers using the ContainerService interface" in new TestContainerController {
//      val testContainerName = "test-container"
//      val testProps = ContainerSpec(
//        name = testContainerName,
//        container_type = "DOCKER",
//        image = "nginx",
//        provider = ContainerSpec.InputProvider(id = testProvider.id),
//        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
//        cpus = 1.0,
//        memory = 128,
//        disk = 0.0,
//        num_instances = 1,
//        network = Some("BRIDGE"),
//        cmd = None,
//        constraints = Seq(),
//        accepted_resource_roles = None,
//        args = None,
//        force_pull = false,
//        health_checks = Seq(),
//        volumes = Seq(),
//        labels = Map(),
//        env = Map(),
//        user = None
//      )
//      val extId = s"/${testEnv.id}/test-container"
//      val createdResource = createInstance(ResourceIds.Container, testContainerName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "container_type" -> testProps.container_type,
//          "image" -> testProps.image,
//          "provider" -> Output.renderInstance(testProvider).toString,
//          "cpus" -> testProps.cpus.toString,
//          "memory" -> testProps.memory.toString,
//          "disk" -> testProps.disk.toString,
//          "num_instances" -> testProps.num_instances.toString,
//          "force_pull" -> testProps.force_pull.toString,
//          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
//          "network" -> testProps.network.get,
//          "external_id" -> s"${extId}"
//        ))
//      ).get
//
//      mockActionProvider.destroy(
//        hasId(createdResource.id)
//      ) returns Future(())
//
//      val request = fakeAuthRequest(DELETE,
//        s"/root/environments/${testEnv.id}/containers/${createdResource.id}", testCreds)
//
//      val Some(result) = route(request)
//
//      status(result) must equalTo(NO_CONTENT)
//
//      there was one(mockActionProvider).destroy(
//        hasId(createdResource.id) and hasProperties("external_id" -> extId)
//      )
//      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
//    }

//    "deploy blueprints using the ActionProvider interfaces" in new TestContainerController {
//      val testContainerName = "test-container"
//      val testProps = ContainerSpec(
//        name = testContainerName,
//        container_type = "DOCKER",
//        image = "nginx",
//        provider = ContainerSpec.InputProvider(id = testProvider.id),
//        port_mappings = Seq(ContainerSpec.PortMapping("tcp",Some(80),None,None,None,None)),
//        cpus = 1.0,
//        memory = 128,
//        disk = 0.0,
//        num_instances = 1,
//        network = Some("BRIDGE"),
//        cmd = None,
//        constraints = Seq(),
//        accepted_resource_roles = None,
//        args = None,
//        force_pull = false,
//        health_checks = Seq(),
//        volumes = Seq(),
//        labels = Map(),
//        env = Map(),
//        user = None
//      )
//      val extId = s"/${testEnv.id}/test-container"
//      val createdResource = createInstance(ResourceIds.Container, testContainerName,
//        parent = Some(testEnv.id),
//        properties = Some(Map(
//          "container_type" -> testProps.container_type,
//          "image" -> testProps.image,
//          "provider" -> Output.renderInstance(testProvider).toString,
//          "cpus" -> testProps.cpus.toString,
//          "memory" -> testProps.memory.toString,
//          "disk" -> testProps.disk.toString,
//          "num_instances" -> testProps.num_instances.toString,
//          "force_pull" -> testProps.force_pull.toString,
//          "port_mappings" -> Json.toJson(testProps.port_mappings).toString,
//          "network" -> testProps.network.get,
//          "external_id" -> s"${extId}"
//        ))
//      ).get
//
//      mockActionProvider.scale(
//        any, any, any
//      ) returns Future(createdResource.copy(
//        properties = Some(createdResource.properties.get ++ Map("num_instances" -> "4"))
//      ))
//
//      val request = fakeAuthRequest(POST,
//        s"/root/containers/${createdResource.id}/scale?numInstances=4", testCreds
//      )
//
//      val Some(result) = route(request)
//
//      status(result) must equalTo(ACCEPTED)
//
//      there was one(mockActionProvider).scale(
//        context = matchesProviderContext(
//          provider = testProvider,
//          workspace = testWork,
//          environment = testEnv
//        ),
//        container = hasId(createdResource.id),
//        numInstances = meq(4)
//      )
//      there was atLeastOne(providerManager).getProviderImpl(ResourceIds.KubeProvider)
//    }

  }

}