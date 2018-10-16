package com.galacticfog.gestalt.container

import scala.util.Success
import scala.collection.JavaConversions._
import org.specs2.specification.{BeforeAfterEach,BeforeAll}
import mockws.MockWS
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.test.{MetaRepositoryOps,WithDb}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltSecurityModule
import play.api.inject.bind
import play.api.mvc._
import play.api.mvc.Results._
import play.api.libs.json._
import play.api.libs.json.Json.JsValueWrapper
import play.api.test.PlaySpecification
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.ws.ahc.AhcWSModule
import play.api.libs.ws.WSClient
import controllers.util._
import modules._
import com.amazonaws.services.ecs.AmazonECS
import com.amazonaws.services.ecs.model._
import com.galacticfog.gestalt.integrations.ecs._

class EcsContainerImportSpec extends PlaySpecification with BeforeAll with BeforeAfterEach with MetaRepositoryOps {


  override def beforeAll(): Unit = { pristineDatabase(); () }

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential

  def appWithMocks(ws: WSClient) = {
    val defaultDisabled = Seq(
      classOf[ProdSecurityModule],
      classOf[HealthModule],
      classOf[AhcWSModule]
    )

    val sc: Seq[GuiceableModule] = Seq(
      bind[WSClient].toInstance(ws),
      new UpgraderServiceModule,
      new MetaConfigModule,
      new MetaDefaultDocker,
      new ActorsModule,
      FakeGestaltSecurityModule(fakeSecurityEnvironment()),
      new SystemConfigModule,
      bind[SecureController].toInstance(mockSecureController),
      bind[SecurityClientProvider].toInstance(mock[SecurityClientProvider]),
      bind[SecurityKeyInit].toInstance(mock[SecurityKeyInit]),
      bind[MetaHealth].toInstance(mock[MetaHealth]),
      bind[MetaServiceStatus].toInstance(mock[MetaServiceStatus]),
      bind[GenericResourceMethods].to[GenericResourceMethodsImpl]
    )

    new GuiceApplicationBuilder()
      .disable(defaultDisabled: _*)
      .bindings(sc: _*)
      .build
  }

  abstract class MockScope(providerConfig: Seq[(String,JsValueWrapper)] = Seq.empty, ws: WSClient) extends WithDb(appWithMocks(ws)) {

    lazy val (testWork, testEnv) = {
      val (tw, te) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment").get
      Entitlements.setNewResourceEntitlements(dummyRootOrgId, te.id, user, Some(tw.id))
      (tw,te)
    }

    lazy val Success(testProvider) = createEcsProvider(testEnv.id, "test-provider", "FARGATE", providerConfig)
  }

  class MockEcsClientFactory(mockClient: AmazonECS) extends EcsClientFactory {
    def getEcsClient(launchType: String, rawProperties: String): Either[String,EcsClient] = {
      val realClient = new DefaultEcsClientFactory().getEcsClient(launchType, rawProperties)
      realClient.right.map(_.copy(client=mockClient))
    }
  }

  "EcsContainerImport" should {
    val ecsProviderConfig: Seq[(String,JsValueWrapper)] = Seq(
      "access_key" -> "",
      "secret_key" -> "",
      "cluster" -> "test",
      "region" -> "us-east-2",
      "endpoints" -> Json.arr(
        Json.obj(
          "actions" -> Json.arr("container.import"),
          "default" -> false,
          "http" -> Json.obj(
            "method" -> "POST",
            "url" -> "http://localhost:8090"
          )
        )
      )
    )
    var lastLambdaRequestBody: JsValue = null
    val mockWs = MockWS {
      case (POST, "http://localhost:8090") => Action { request =>
        lastLambdaRequestBody = request.body.asJson.get
        Ok("http response")
      }
    }
    "import container" in new MockScope(ecsProviderConfig, mockWs) {
      val metaContainer: GestaltResourceInstance = newInstance(
        ResourceIds.Container,
        "test",
        properties = Some(Map(
          "container_type" -> "DOCKER",
          "description" -> "description",
          "image" -> "nginx:alpine",
          "provider" -> Output.renderInstance(testProvider).toString,
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
          "external_id" -> "arn:aws:ecs:us-east-2:326625211408:service/test-WebsiteService-1TIWL47KKR37L-Service-8HFNG8QFP8KK"
        ))
      )

      val json0: JsObject = Output.renderInstance(metaContainer).as[JsObject]
      val json = json0 ++ JsObject(Seq("resource_type" -> JsString(ResourceIds.Container.toString)))

      val importRequest = fakeAuthRequest("POST", s"/root/environments/${testEnv.id}/containers?action=import", testCreds).withBody(
        json
      )
      await(route(app, importRequest).get)

      val mockClient = mock[AmazonECS]
      val mockDescribeServicesResult = mock[DescribeServicesResult]
      val mockService = mock[Service]
      mockService.getLaunchType() returns "FARGATE"
      mockService.getTaskDefinition() returns ""
      mockService.getDesiredCount() returns 1
      val mockNetworkConfiguration = mock[NetworkConfiguration]
      val mockAwsvpcConfiguration = mock[AwsVpcConfiguration]
      mockAwsvpcConfiguration.getSubnets() returns new java.util.ArrayList(Seq("subnet-a", "subnet-b"))
      mockNetworkConfiguration.getAwsvpcConfiguration() returns mockAwsvpcConfiguration
      mockService.getNetworkConfiguration() returns mockNetworkConfiguration
      mockDescribeServicesResult.getServices() returns new java.util.ArrayList(Seq(mockService))
      mockClient.describeServices(any) returns mockDescribeServicesResult
      val mockDescribeTaskDefinitionResult = mock[DescribeTaskDefinitionResult]
      val mockTaskDefinition = mock[TaskDefinition]
      val mockVolume = mock[Volume]
      val mockHostPathProperties = mock[HostVolumeProperties]
      mockHostPathProperties.getSourcePath() returns "/tmp"
      mockVolume.getHost() returns mockHostPathProperties
      mockVolume.getName() returns "test-volume"
      mockTaskDefinition.getVolumes() returns new java.util.ArrayList(Seq(mockVolume))
      val mockContainerDefn = mock[ContainerDefinition]
      mockContainerDefn.getName() returns "name"
      mockContainerDefn.getCpu() returns 1024
      mockContainerDefn.getEnvironment() returns new java.util.ArrayList()
      mockContainerDefn.getPortMappings() returns new java.util.ArrayList()
      val mockMountPoint = mock[MountPoint]
      mockMountPoint.getSourceVolume() returns "test-volume"
      mockMountPoint.getContainerPath() returns "/mnt"
      mockContainerDefn.getMountPoints() returns new java.util.ArrayList(Seq(mockMountPoint))
      mockTaskDefinition.getContainerDefinitions() returns new java.util.ArrayList(Seq(mockContainerDefn))
      mockDescribeTaskDefinitionResult.getTaskDefinition() returns mockTaskDefinition
      mockClient.describeTaskDefinition(any) returns mockDescribeTaskDefinitionResult

      class ECI extends EcsContainerImport {
        override val clientFactory = new MockEcsClientFactory(mockClient)
      }

      val response = new ECI().run(lastLambdaRequestBody.toString, "{}")
      val container = (Json.parse(response) \ "properties").as[ContainerSpec]

      container must_== ContainerSpec(
        "",
        None,     // tis field is lost on the way to EcsContainerImport
        "DOCKER",
        "nginx:alpine",
        ContainerSpec.InputProvider(
          testProvider.id,
          Some("test-provider"),
          None
        ),
        List(),
        1.0,
        4096.0,
        0.0,
        1,
        Some("subnet-a;subnet-b"),
        None,
        List(),
        None,
        None,
        false,
        List(),
        List(
          ContainerSpec.InlineVolumeMountSpec(
            "/mnt",
            GestaltResourceInput(
              "test-volume",
              None,
              None,
              None,
              None,
              None,
              Some(Map(
                "type" -> JsString("host_path"),
                "access_mode" -> JsString("ReadWriteMany"),
                "size" -> JsNumber(1),
                "config" -> JsString("{\n              \"host_path\": \"/tmp\"\n            }")
              )),
              None,
              None,
              None
            )
          )
        ),
        Map(),
        Map(),
        None,
        None,     // not deserialized
        None,
        List()
      )
    }
    "import container (small payload)" in new MockScope(ecsProviderConfig, mockWs) {
      val metaContainer: GestaltResourceInstance = newInstance(
        ResourceIds.Container,
        "test",
        properties = Some(Map(
          "provider" -> Output.renderInstance(testProvider).toString,
          "image" -> "n/a",
          "container_type" -> "n/a",
          "external_id" -> "arn:aws:ecs:us-east-2:326625211408:service/nginx-service"
        ))
      )

      val json0: JsObject = Output.renderInstance(metaContainer).as[JsObject]
      val json = json0 ++ JsObject(Seq("resource_type" -> JsString(ResourceIds.Container.toString)))

      val importRequest = fakeAuthRequest("POST", s"/root/environments/${testEnv.id}/containers?action=import", testCreds).withBody(
        json
      )
      await(route(app, importRequest).get)

      val mockClient = mock[AmazonECS]
      val mockDescribeServicesResult = mock[DescribeServicesResult]
      val mockService = mock[Service]
      mockService.getLaunchType() returns "FARGATE"
      mockService.getTaskDefinition() returns ""
      mockService.getDesiredCount() returns 1
      val mockNetworkConfiguration = mock[NetworkConfiguration]
      val mockAwsvpcConfiguration = mock[AwsVpcConfiguration]
      mockAwsvpcConfiguration.getSubnets() returns new java.util.ArrayList(Seq("subnet-a", "subnet-b"))
      mockNetworkConfiguration.getAwsvpcConfiguration() returns mockAwsvpcConfiguration
      mockService.getNetworkConfiguration() returns mockNetworkConfiguration
      mockDescribeServicesResult.getServices() returns new java.util.ArrayList(Seq(mockService))
      mockClient.describeServices(any) returns mockDescribeServicesResult
      val mockDescribeTaskDefinitionResult = mock[DescribeTaskDefinitionResult]
      val mockTaskDefinition = mock[TaskDefinition]
      val mockContainerDefn = mock[ContainerDefinition]
      mockContainerDefn.getName() returns "name"
      mockContainerDefn.getImage() returns "nginx:alpine"
      mockContainerDefn.getCpu() returns 1024
      mockContainerDefn.getEnvironment() returns new java.util.ArrayList()
      mockContainerDefn.getPortMappings() returns new java.util.ArrayList()
      mockTaskDefinition.getContainerDefinitions() returns new java.util.ArrayList(Seq(mockContainerDefn))
      mockDescribeTaskDefinitionResult.getTaskDefinition() returns mockTaskDefinition
      mockClient.describeTaskDefinition(any) returns mockDescribeTaskDefinitionResult

      class ECI extends EcsContainerImport {
        override val clientFactory = new MockEcsClientFactory(mockClient)
      }

      val response = new ECI().run(lastLambdaRequestBody.toString, "{}")
      val container = (Json.parse(response) \ "properties").as[ContainerSpec]

      container must_== ContainerSpec(
        "",
        None,     // this field is lost on the way to EcsContainerImport
        "DOCKER",
        "nginx:alpine",
        ContainerSpec.InputProvider(
          testProvider.id,
          Some("test-provider"),
          None
        ),
        List(),
        1.0,
        0.0,
        0.0,
        1,
        Some("subnet-a;subnet-b"),
        None,
        List(),
        None,
        None,
        false,
        List(),
        List(),
        Map(),
        Map(),
        None,
        None,     // not deserialized
        None,
        List()
      )
    }
  }
}