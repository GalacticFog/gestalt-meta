package com.galacticfog.gestalt.container

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.{MetaRepositoryOps, WithDb}
import com.galacticfog.gestalt.security.play.silhouette.fakes.FakeGestaltSecurityModule
import controllers.util._
import mockws.MockWS
import modules._
import org.specs2.specification.{BeforeAfterEach, BeforeAll}
import org.mockito.Matchers.{eq => meq}
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._
import play.api.libs.ws.WSClient
import play.api.libs.ws.ahc.AhcWSModule
import play.api.mvc.Results._
import play.api.mvc._
import play.api.test.PlaySpecification
import skuber.{Container, Pod}
import skuber.api.client.RequestContext
import skuber.ext.Deployment

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

import scala.util.Success

class KubeContainerImportSpec extends PlaySpecification with BeforeAll with BeforeAfterEach with MetaRepositoryOps {


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

    lazy val Success(testProvider) = createKubernetesProvider(testEnv.id, "test-provider", providerConfig)
  }

  "KubeContainerImport" should {
    val kubeProviderConfig: Seq[(String,JsValueWrapper)] = Seq(
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

    "import container" in new MockScope(kubeProviderConfig, mockWs) {
      val metaContainer: GestaltResourceInstance = newInstance(
        ResourceIds.Container,
        "test",
        properties = Some(Map(
          "provider" -> Output.renderInstance(testProvider).toString,
          "image" -> "n/a",
          "container_type" -> "n/a",
          "external_id" -> "test-namespace/test-deployment"
        ))
      )

      val json0: JsObject = Output.renderInstance(metaContainer).as[JsObject]
      val json = json0 ++ JsObject(Seq("resource_type" -> JsString(ResourceIds.Container.toString)))

      val importRequest = fakeAuthRequest("POST", s"/root/environments/${testEnv.id}/containers?action=import", testCreds).withBody(
        json
      )
      await(route(app, importRequest).get)

      val skuberContextMock = mock[skuber.api.client.RequestContext]
      val testDeployment = Deployment(
        spec = Some(Deployment.Spec(
          template = Some(Pod.Template.Spec(
            spec = Some(Pod.Spec(
              containers = List(Container(
                name = "testContainer",
                image = "testImage"
              ))
            ))
          ))
        ))
      )
      skuberContextMock.getOption[skuber.ext.Deployment](meq("test-deployment"))(any, any, any) returns Future.successful(Some(testDeployment))

      class KCI extends KubeContainerImport {
        override def initializeKube( provider: JsObject, namespace: String )
                                   ( implicit ec: ExecutionContext ): Try[RequestContext] = Success(skuberContextMock)
      }

      val response = new KCI().run(lastLambdaRequestBody.toString, "{}")
      val container = (Json.parse(response) \ "properties").as[ContainerSpec]

      container must_== ContainerSpec(
        "",
        None,     // this field is lost on the way to EcsContainerImport
        "DOCKER",
        "testImage",
        ContainerSpec.InputProvider(
          testProvider.id,
          Some("test-provider"),
          None
        ),
        List(),
        0.0,
        0.0,
        0.0,
        1,
        None,
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