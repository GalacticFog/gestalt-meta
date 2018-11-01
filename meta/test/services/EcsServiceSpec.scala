package services

import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.{ContainerStats,ContainerSpec}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import controllers.util.GestaltSecurityMocking
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.specs2.matcher.JsonMatchers
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAfterEach, BeforeAll, Scope}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.api.libs.json.Json.JsValueWrapper
import play.api.test.PlaySpecification
import com.amazonaws.services.ecs.AmazonECS
import com.amazonaws.services.ecs.model._
import scala.collection.JavaConversions._
import scala.concurrent.Future
import scala.util.Success
import com.galacticfog.gestalt.integrations.ecs._

@RunWith(classOf[JUnitRunner])
class EcsServiceSpec extends PlaySpecification with ResourceScope with BeforeAll with BeforeAfterEach with JsonMatchers {
  override def beforeAll(): Unit = pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential

  case class TestSetup(ecsService: EcsService, ecs: AmazonECS)

  abstract class MockScope( providerConfig: Seq[(String,JsValueWrapper)] = Seq.empty ) extends Scope {
    lazy val testAuthResponse = GestaltSecurityMocking.dummyAuthResponseWithCreds()
    lazy val testCreds = testAuthResponse.creds
    lazy val user = AuthAccountWithCreds(testAuthResponse.account, Seq.empty, Seq.empty, testCreds, dummyRootOrgId)

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewResourceEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      (tw,te)
    }

    lazy val Success(testProvider) = createEcsProvider(testEnv.id, "test-provider", "FARGATE", providerConfig)

    lazy val Success(testVolume) = createVolume(testEnv.id, "test-volume", Seq("host_path" -> "/tmp"))

    lazy val testSetup = {
      val mockAmazonECS = mock[AmazonECS]
      val mockClient = mock[EcsClient]
      mockClient.client returns mockAmazonECS
      mockClient.cluster returns "test_cluster"
      mockClient.launchType returns "FARGATE"
      mockClient.region returns ""
      mockClient.kongConfigureUrl returns None
      mockClient.taskRoleArn returns Some("")
      val mockAwsSdkFactory = mock[AwsSdkFactory]
      mockAwsSdkFactory.getEcsClient(any)(any) returns Future.successful(mockClient)
      val es = new EcsService(mockAwsSdkFactory)
      TestSetup(es, mockAmazonECS)
    }
  }

  "EcsService" should {

    "create container (FARGATE)" in new MockScope {
      // val volumeSpec = ContainerSpec.InlineVolumeMountSpec(
      //   mount_path = "/mnt/path",
      //   volume_resource = VolumeSpec.toResourcePrototype(VolumeSpec(
      //     name = "test-volume-name",
      //     provider = Some(ContainerSpec.InputProvider(id = uuid())),
      //     `type` = VolumeSpec.HostPath,
      //     size = 0,     // doesn't make sense with HostPath
      //     access_mode = VolumeSpec.ReadWriteMany,     // the only one that makes sense with HostPath
      //     config = Json.obj(
      //       "host_path" -> "/tmp"
      //     )
      //   ))
      // )
      // val volumesSerialized = Json.toJson[Seq[ContainerSpec.VolumeMountSpec]](Seq(volumeSpec)).toString
      val volumesSerialized = s"""[{"volume_id": "${testVolume.id}", "mount_path": "/mnt"}]"""

      val portMapping = ContainerSpec.PortMapping(
        protocol = "tcp",
        container_port = Some(80),
        name = Some("test"),
        lb_port = Some(80)
      )
      val portMappingsSerialized = Json.toJson[Seq[ContainerSpec.PortMapping]](Seq(portMapping)).toString
      // val portMappingsSerialized = "[]"

      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testProvider.id
          ).toString,
          "cpus" -> "2.0",
          "memory" -> "4096.0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> portMappingsSerialized,
          "env" -> Json.obj(
            "VAR1" -> "VAL1",
            "VAR2" -> "VAL2"
          ).toString,
          "network" -> "subnet-66d74a1c",
          "volumes" -> volumesSerialized
        ))
      )
      val mockTaskDefinition = mock[TaskDefinition]
      mockTaskDefinition.getFamily() returns s"${testEnv.id}-test"
      mockTaskDefinition.getRevision() returns 1
      val mockRegisterTaskDefinitionResult = mock[RegisterTaskDefinitionResult]
      mockRegisterTaskDefinitionResult.getTaskDefinition() returns mockTaskDefinition
      testSetup.ecs.registerTaskDefinition(any) returns mockRegisterTaskDefinitionResult
      val mockService = mock[Service]
      mockService.getServiceArn() returns "service arn"
      val mockCreateServiceResult = mock[CreateServiceResult]
      mockCreateServiceResult.getService() returns mockService
      testSetup.ecs.createService(any) returns mockCreateServiceResult

      val Some(updatedContainerProps) = await(testSetup.ecsService.create(
        context = ProviderContext(play.api.test.FakeRequest("POST", s"/root/environments/${testEnv.id}/containers"), testProvider.id, None),
        container = metaContainer
      )).properties

      updatedContainerProps.get("external_id") must_==(Some("service arn"))
    }
    "list containers" in new MockScope {
      val mockListServicesResult = mock[ListServicesResult]
      mockListServicesResult.getNextToken() returns null
      mockListServicesResult.getServiceArns() returns new java.util.ArrayList(Seq(
        s"arn:aws:ecs:us-east-2:326625211408:service/${testEnv.id}-test",
        "arn:aws:ecs:us-east-2:326625211408:service/other-env-test"
      ))
      testSetup.ecs.listServices(any) returns mockListServicesResult
      val mockDescribeServicesResult = mock[DescribeServicesResult]
      mockDescribeServicesResult.getFailures() returns new java.util.ArrayList(Seq.empty[Failure])
      val mockService = mock[Service]
      mockService.getServiceArn() returns "service arn"
      mockService.getStatus() returns "status"
      val now = new java.util.Date()
      mockService.getCreatedAt() returns now
      mockService.getDesiredCount() returns 1
      mockService.getPendingCount() returns 2
      mockService.getRunningCount() returns 3
      mockService.getTaskDefinition() returns "task definition"
      mockDescribeServicesResult.getServices() returns new java.util.ArrayList(Seq(mockService))
      testSetup.ecs.describeServices(any) returns mockDescribeServicesResult
      val mockDescribeTaskDefinitionResult = mock[DescribeTaskDefinitionResult]
      val mockTaskDefinition = mock[TaskDefinition]
      val mockContainerDefinition = mock[ContainerDefinition]
      mockContainerDefinition.getCpu() returns 1024
      mockContainerDefinition.getMemory() returns 2048
      mockContainerDefinition.getImage() returns "container image"
      mockTaskDefinition.getContainerDefinitions() returns new java.util.ArrayList(Seq(mockContainerDefinition))
      mockDescribeTaskDefinitionResult.getTaskDefinition() returns mockTaskDefinition
      testSetup.ecs.describeTaskDefinition(any) returns mockDescribeTaskDefinitionResult
      val mockListTasksResult = mock[ListTasksResult]
      mockListTasksResult.getTaskArns() returns new java.util.ArrayList(Seq("task a", "task b"))
      testSetup.ecs.listTasks(any) returns mockListTasksResult
      val mockDescribeTasksResult = mock[DescribeTasksResult]
      val mockTask = mock[Task]
      mockTask.getTaskArn() returns "task arn"
      mockTask.getStartedAt() returns new java.util.Date(0)
      mockDescribeTasksResult.getTasks() returns new java.util.ArrayList(Seq(mockTask))
      testSetup.ecs.describeTasks(any) returns mockDescribeTasksResult

      val containerStats = await(testSetup.ecsService.listInEnvironment(
        context = ProviderContext(play.api.test.FakeRequest("GET",
         s"/root/environments/${testEnv.id}/containers"), testProvider.id, None)
      ))

      containerStats must_==(Seq(ContainerStats(
        external_id = "service arn",
        containerType = "DOCKER",
        status = "status",
        cpus = 1.0,
        memory = 2048.0,
        image = "container image",
        age = new DateTime(now),
        numInstances = 1,
        tasksStaged = 2,
        tasksRunning = 3,
        tasksHealthy = 0,
        tasksUnhealthy = 0,
        taskStats = Some(Seq()),
        lb_address = None
      )))
    }
    "destroy container" in new MockScope {
      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testProvider.id
          ).toString,
          "cpus" -> "2.0",
          "memory" -> "4096.0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "env" -> Json.obj(
            "VAR1" -> "VAL1",
            "VAR2" -> "VAL2"
          ).toString,
          "network" -> "subnet-66d74a1c",
          "external_id" -> "external id"
        ))
      )
      val mockService = mock[Service]
      mockService.getLaunchType() returns "FARGATE"
      mockService.getTaskDefinition() returns "task defn arn"
      val mockDescribeServicesResult = mock[DescribeServicesResult]
      mockDescribeServicesResult.getServices() returns new java.util.ArrayList(Seq(mockService))
      testSetup.ecs.describeServices(any) returns mockDescribeServicesResult

      await(testSetup.ecsService.destroy(metaContainer))
    }
    "destroy invalid container" in new MockScope {
      val Success(metaContainer) = createInstance(
        ResourceIds.Container,
        "test",
        parent = Some(testEnv.id),
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "image" -> "nginx:alpine",
          "provider" -> Json.obj(
            "id" -> testProvider.id
          ).toString,
          "cpus" -> "2.0",
          "memory" -> "4096.0",
          "num_instances" -> "1",
          "force_pull" -> "true",
          "port_mappings" -> "[]",
          "env" -> Json.obj(
            "VAR1" -> "VAL1",
            "VAR2" -> "VAL2"
          ).toString,
          "network" -> "subnet-66d74a1c"
        ))
      )
      val mockService = mock[Service]
      mockService.getLaunchType() returns "FARGATE"
      mockService.getTaskDefinition() returns "task defn arn"
      val mockDescribeServicesResult = mock[DescribeServicesResult]
      mockDescribeServicesResult.getServices() returns new java.util.ArrayList(Seq(mockService))
      testSetup.ecs.describeServices(any) returns mockDescribeServicesResult

      await(testSetup.ecsService.destroy(metaContainer))
    }
  }
}