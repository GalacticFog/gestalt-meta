package com.galacticfog.gestalt.container

import java.util.UUID
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
// import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
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
import skuber.api.client.RequestContext

import scala.concurrent.Future

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
        // Json.obj(
        //   "actions" -> Json.arr("container.import", "secret.import"),
        //   "default" -> false,
        //   "http" -> Json.obj(
        //     "method" -> "POST",
        //     "url" -> "http://localhost:8090"
        //   )
        // )
        Json.obj(
          "kind" -> "",
          "url" -> "http://localhost:8090",
          "actions" -> Json.arr(
            Json.obj("name" -> "container.import", "post" -> Json.obj("responses" -> Json.arr(Json.obj("code" -> 200)))),
            Json.obj("name" -> "secret.import", "post" -> Json.obj("responses" -> Json.arr(Json.obj("code" -> 200)))),
            Json.obj("name" -> "volume.import", "post" -> Json.obj("responses" -> Json.arr(Json.obj("code" -> 200))))
          )
        )
      )
   )
   var lastLambdaRequestBody = Map.empty[String,JsValue]
   val mockWs = MockWS {
     case (POST, "http://localhost:8090") => Action { request =>
       val body = request.body.asJson.get
       val action = (body \ "action").as[String]
       lastLambdaRequestBody = lastLambdaRequestBody ++ Map(action -> body)
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
         "external_id" -> "/namespaces/test-namespace/deployments/test-deployment"
       ))
     )

     val json0: JsObject = Output.renderInstance(metaContainer).as[JsObject]
     val json = json0 ++ JsObject(Seq("resource_type" -> JsString(ResourceIds.Container.toString)))

     val importRequest = fakeAuthRequest("POST", s"/root/environments/${testEnv.id}/containers?action=import", testCreds).withBody(
       json
     )
     await(route(app, importRequest).get)

     val skuberContextMock = mock[skuber.api.client.RequestContext]
     val testDeployment = skuber.ext.Deployment(
       spec = Some(skuber.ext.Deployment.Spec(
         template = Some(skuber.Pod.Template.Spec(
           spec = Some(skuber.Pod.Spec(
             containers = List(skuber.Container(
               name = "testContainer",
               image = "testImage",
               env = List(
                 skuber.EnvVar("TEST1", skuber.EnvVar.StringValue("1234")),
                 skuber.EnvVar("TEST2", skuber.EnvVar.SecretKeyRef("test-key", "test-secret"))
               ),
               volumeMounts = List(
                 skuber.Volume.Mount(
                   name = "test-secret-volume",
                   mountPath = "/tmp",
                   readOnly = true
                 ),
                 skuber.Volume.Mount(
                   name = "test-pvc-volume",
                   mountPath = "/tmp2",
                   readOnly = true
                 )//,
                 // skuber.Volume.Mount(
                 //   name = "test-host-volume",
                 //   mountPath = "/tmp3"
                 // )
               )
             )),
             volumes = List(
               skuber.Volume("test-secret-volume",
                 skuber.Volume.Secret(
                   secretName = "test-secret-2",
                   items = Some(List(
                     skuber.Volume.KeyToPath("test-key", "key.json"),
                     skuber.Volume.KeyToPath("test-key-2", "key2.json")
                   )),
                   defaultMode = Some(400)     // ignored
                 )
               ),
               skuber.Volume("test-pvc-volume",
                 skuber.Volume.PersistentVolumeClaimRef(
                   claimName = "test-pvc",
                   readOnly = true     // ignored
                 )
               )//,
               // skuber.Volume("test-host-volume",
               //   skuber.Volume.HostPath(
               //     path = "/host/path"
               //   )
               // )
             )
           ))
         )),
         selector = Some(new skuber.LabelSelector(skuber.LabelSelector.IsEqualRequirement("app", "test")))
       ))
     )
     val testSecret = skuber.Secret(
       kind = "Secret",
       apiVersion = skuber.v1,
       metadata = new skuber.ObjectMeta(labels = Map(
         "meta/secret" -> "93029c37-c054-4837-9d66-1c6af8f8d648",
         "meta/environment" -> s"${testEnv.id}",
         "meta/workspace" -> s"${testWork.id}",
         "meta/provider" -> s"${testProvider.id}"
       )),
       data = Map("test-key" -> "123".getBytes),
       `type` = ""
     )
     val testSecret2 = skuber.Secret(
       kind = "Secret",
       apiVersion = skuber.v1,
       metadata = new skuber.ObjectMeta(labels = Map(
         "meta/secret" -> "454e1069-0d83-4af0-8ce4-f5509e556fbe",
         "meta/environment" -> s"${testEnv.id}",
         "meta/workspace" -> s"${testWork.id}",
         "meta/provider" -> s"${testProvider.id}"
       )),
       data = Map("test-key" -> "123".getBytes),
       `type` = ""
     )
     val testPvc = new skuber.PersistentVolumeClaim(
       kind = "PersistentVolumeClaim",
       apiVersion = skuber.v1,
       metadata = new skuber.ObjectMeta(labels = Map(
         "meta/volume" -> "127b4668-9132-4f2a-8124-dd63ddb39bbd",
         "meta/environment" -> s"${testEnv.id}",
         "meta/workspace" -> s"${testWork.id}",
         "meta/provider" -> s"${testProvider.id}"
       )),
       spec = Some(skuber.PersistentVolumeClaim.Spec(
         volumeName = "test-pv"
       )),
       status = None
     )
     skuberContextMock.getInNamespace[skuber.ext.Deployment](meq("test-deployment"), meq("test-namespace"))(any, any, any) returns Future.successful(testDeployment)
     skuberContextMock.getInNamespace[skuber.Secret](meq("test-secret"), meq("test-namespace"))(any, any, any) returns Future.successful(testSecret)
     skuberContextMock.getInNamespace[skuber.Secret](meq("test-secret-2"), meq("test-namespace"))(any, any, any) returns Future.successful(testSecret2)
     skuberContextMock.getInNamespace[skuber.PersistentVolumeClaim](meq("test-pvc"), meq("test-namespace"))(any, any, any) returns Future.successful(testPvc)
     skuberContextMock.jsonMergePatch[skuber.ext.Deployment](any, any)(any, any, any) returns Future.successful(testDeployment)

     class KCI extends KubeContainerImport {
       override def initializeKube( provider: JsObject, namespace: String ): RequestContext = skuberContextMock
     }

     val response = new KCI().run(lastLambdaRequestBody("container.import").toString, "{}")
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
       List(
         ContainerSpec.ExistingVolumeMountSpec("/tmp2", UUID.fromString("127b4668-9132-4f2a-8124-dd63ddb39bbd"))//,
         // ContainerSpec.InlineVolumeMountSpec("/tmp2", GestaltResourceInput(
         //   name = "",
         //   resource_type = None,
         //   id = None,
         //   owner = None,
         //   resource_state = None,
         //   description = None,
         //   properties = None,
         //   variables = None,
         //   tags = None,
         //   auth = None
         // ))
       ),
       Map(),
       Map("TEST1" -> "1234"),
       None,
       None,     // not deserialized
       None,
       List(
         ContainerSpec.SecretEnvMount(UUID.fromString("93029c37-c054-4837-9d66-1c6af8f8d648"), "TEST2", "test-key"),
         ContainerSpec.SecretDirMount(UUID.fromString("454e1069-0d83-4af0-8ce4-f5509e556fbe"), "/tmp")
       )
     )
   }

   "import secret" in new MockScope(kubeProviderConfig, mockWs) {
     val metaSecret: GestaltResourceInstance = newInstance(
       ResourceIds.Secret,
       "test",
       properties = Some(Map(
         "provider" -> Output.renderInstance(testProvider).toString,
         "external_id" -> "/namespaces/test-namespace/secrets/test-secret"
       ))
     )

     val json0: JsObject = Output.renderInstance(metaSecret).as[JsObject]
     val json = json0 ++ JsObject(Seq("resource_type" -> JsString(ResourceIds.Secret.toString)))

     val importRequest = fakeAuthRequest("POST", s"/root/environments/${testEnv.id}/secrets?action=import", testCreds).withBody(
       json
     )
     val res = route(app, importRequest).get
     val _ = contentAsJson(res)

     val skuberContextMock = mock[skuber.api.client.RequestContext]
     val testSecret = skuber.Secret(
       kind = "Secret",
       apiVersion = skuber.v1,
       metadata = new skuber.ObjectMeta(),
       data = Map("test" -> "123".getBytes),
       `type` = ""
     )
     skuberContextMock.getInNamespace[skuber.Secret](meq("test-secret"), meq("test-namespace"))(any, any, any) returns Future.successful(testSecret)
     skuberContextMock.jsonMergePatch[skuber.Secret](any, any)(any, any, any) returns Future.successful(testSecret)

     class KCI extends KubeContainerImport {
       override def initializeKube( provider: JsObject, namespace: String ): RequestContext = skuberContextMock
     }

     val response = new KCI().run(lastLambdaRequestBody("secret.import").toString, "{}")
     val secret = (Json.parse(response) \ "properties").as[JsValue]

     (secret \ "items").as[JsValue] must_== Json.arr(Json.obj("key" -> "test", "value" -> "123"))
   }
 }
}