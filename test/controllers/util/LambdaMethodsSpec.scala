package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.{LaserEndpoint, LaserLambda}
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, HostConfig, JsonClient, ResourceIds, ResourceStates}
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp, PatchOps}
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.SecurityResources
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsBoolean, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.{FakeRequest, PlaySpecification}
import play.api.mvc._
import play.api.mvc.Results._
import play.api.http.HttpVerbs._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.ws.WSResponse
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.ContainerSpec.{SecretDirMount, SecretEnvMount, SecretFileMount}
import play.api.http.HttpVerbs

import scala.concurrent.Future
import scala.util.Success

class LambdaMethodsSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with JsonMatchers with JsonInput {

  object Ents extends com.galacticfog.gestalt.meta.auth.AuthorizationMethods with SecurityResources

  override def beforeAll(): Unit = {
    pristineDatabase()
    val Success(createdUser) = Ents.createNewMetaUser(user, dummyRootOrgId, rootOwnerLink(), user.account,
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

  abstract class FakeLambdaScope extends Scope {
    val Success(testOrg) = createOrg(name = uuid().toString)
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = uuid().toString, envName = uuid().toString, org = testOrg.id)
    Entitlements.setNewResourceEntitlements(dummyRootOrgId, testEnv.id, user, Some(testWork.id))

    val mockProviderMethods = mock[ProviderMethods]
    val injector =
      new GuiceApplicationBuilder()
        .disable[modules.ProdSecurityModule]
        .disable[modules.MetaDefaultSkuber]
        .disable[modules.MetaDefaultServices]
        .disable[modules.HealthModule]
        .bindings(
          bind(classOf[GestaltSecurityConfig]).toInstance(mock[GestaltSecurityConfig]),
          bind(classOf[ProviderMethods]).toInstance(mockProviderMethods)
        )
        .injector

    val lambdaMethods = injector.instanceOf[LambdaMethods]

    val caasProviderId = uuid()

    val Success(testLambdaProvider) = createInstance(ResourceIds.LambdaProvider, "test-lambda-provider", properties = Some(Map(
      "config" ->
        s"""{
          |  "env": {
          |     "public": {
          |       "SERVICE_HOST": "laser.service",
          |       "SERVICE_PORT": "1111",
          |       "GESTALT_SECURITY_KEY": "key",
          |       "GESTALT_SECURITY_SECRET": "secret",
          |       "META_COMPUTE_PROVIDER_ID": "${caasProviderId}"
          |     }
          |  }
          |}""".stripMargin
    )))

    def createLambda(name: String,
                     props: Map[String,String],
                     orgId: UUID = testOrg.id,
                     envId: UUID = testEnv.id) =
      createInstance(ResourceIds.Lambda, name = name, properties = Some(props), org = orgId, parent = Some(envId))

    def createSecret(name: String,
                     props: Map[String,String],
                     orgId: UUID = testOrg.id,
                     envId: UUID = testEnv.id) =
      createInstance(ResourceIds.Secret, name = name, properties = Some(props), org = orgId, parent = Some(envId))

    val Success(testLambda) = createLambda("test-lambda", Map(
      "public" -> "true",
      "cpus" -> "0.1",
      "memory" -> "128",
      "code_type" -> "inline",
      "timeout" -> "30",
      "handler" -> "blah;blah",
      "headers" -> Json.obj(
        "Existing-Header" -> "ExistingHeaderValue",
        "Remove-Header" -> "Nobody Cares What's Here"
      ).toString,
      "runtime" -> "custom",
      "periodic_info" -> "{}",
      "provider" -> Json.obj(
        "name" -> testLambdaProvider.name,
        "id" -> testLambdaProvider.id.toString
      ).toString
    ))

    val mockJsonClient = mock[JsonClient]
    mockProviderMethods.configureWebClient(argThat(
      (provider: GestaltResourceInstance) => provider.id == testLambdaProvider.id
    ), any) returns mockJsonClient
  }

  "toLaserLambda" should {

    "include all secret mounts" in new FakeLambdaScope {
      val Success(s1) = createSecret( "s1", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ))
      val Success(s2) = createSecret("s2", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ))
      val Success(s3) = createSecret("s3", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ))
      val testSecretMounts = Seq(
        SecretDirMount(s1.id, "/mnt/dir"),
        SecretFileMount(s2.id, "/mnt/dir/file", "secret_key"),
        SecretEnvMount(s3.id, "ENV_VAR", "secret_key")
      )
      val Success(testLambdaWithSecrets) = createLambda("test-lambda-with-secrets", Map(
        "public" -> "true",
        "cpus" -> "0.1",
        "memory" -> "128",
        "code_type" -> "inline",
        "timeout" -> "30",
        "handler" -> "blah;blah",
        "runtime" -> "custom",
        "periodic_info" -> "{}",
        "provider" -> Json.obj(
          "name" -> testLambdaProvider.name,
          "id" -> testLambdaProvider.id.toString
        ).toString,
        "secrets" -> Json.toJson(testSecretMounts).toString
      ))
      val Success(laserLambda) = toLaserLambda(testLambdaWithSecrets, testLambdaProvider.id)
      laserLambda.artifactDescription.secrets must beSome(testSecretMounts.map(Json.toJson(_)))
    }

    "fail if lambda provider references a non-existent secret" in new FakeLambdaScope {
      toLaserLambda(testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(Seq(SecretEnvMount(uuid, "ENV_VAR", "secret_key"))).toString
        ))
      ), testLambdaProvider.id) must beFailedTry.withThrowable[BadRequestException](".*Secret.*does not exist.*")
    }

    "fail if lambda provider is not conformant with secrets provider" in new FakeLambdaScope {
      val Success(secret) = createSecret( "nonconformant-secret", Map(
        "provider" -> Json.obj(
          "id" -> uuid().toString
        ).toString
      ))
      toLaserLambda(testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(Seq(SecretEnvMount(secret.id, "ENV_VAR", "secret_key"))).toString
        ))
      ), testLambdaProvider.id) must beFailedTry.withThrowable[BadRequestException](".*did not have same CaaS provider as mounted Secrets.*")
    }

    "fail if secrets aren't mutual siblings" in new FakeLambdaScope {
      val Success(testOrg2) = createOrg(name = uuid().toString)
      val Success((testWork2, testEnv2)) = createWorkEnv(wrkName = uuid().toString, envName = uuid().toString, org = testOrg2.id)
      val Success(testOrg3) = createOrg(name = uuid().toString)
      val Success((testWork3, testEnv3)) = createWorkEnv(wrkName = uuid().toString, envName = uuid().toString, org = testOrg3.id)

      val Success(s1) = createSecret("s1", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ))
      val Success(s2) = createSecret("s2", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ), orgId = testOrg2.id, envId = testEnv2.id)
      val Success(s3) = createSecret("s3", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ), orgId = testOrg3.id, envId = testEnv3.id)
      val testSecretMounts = Seq(
        SecretDirMount(s1.id, "/mnt/dir"),
        SecretFileMount(s2.id, "/mnt/dir/file", "secret_key"),
        SecretEnvMount(s3.id, "ENV_VAR", "secret_key")
      )

      toLaserLambda(testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(testSecretMounts).toString
        ))
      ), testLambdaProvider.id) must beFailedTry.withThrowable[BadRequestException](".*Secrets must belong to the same Environment.*")
    }

    "fail if secrets aren't siblings with lambda" in new FakeLambdaScope {
      val Success(testOrg2) = createOrg(name = uuid().toString)
      val Success((testWork2, testEnv2)) = createWorkEnv(wrkName = uuid().toString, envName = uuid().toString, org = testOrg2.id)

      val Success(s1) = createSecret("s1", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ), orgId = testOrg2.id, envId = testEnv2.id)
      val Success(s2) = createSecret("s2", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ), orgId = testOrg2.id, envId = testEnv2.id)
      val Success(s3) = createSecret("s3", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ), orgId = testOrg2.id, envId = testEnv2.id)
      val testSecretMounts = Seq(
        SecretDirMount(s1.id, "/mnt/dir"),
        SecretFileMount(s2.id, "/mnt/dir/file", "secret_key"),
        SecretEnvMount(s3.id, "ENV_VAR", "secret_key")
      )

      toLaserLambda(testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(testSecretMounts).toString
        ))
      ), testLambdaProvider.id) must beFailedTry.withThrowable[BadRequestException](".*Lambda.*must belong to the same Environment as all mounted Secrets.*")
    }

    "not specify computePathOverride in the absence of secrets" in new FakeLambdaScope {
      val Success(laserLambda) = toLaserLambda(testLambda, testLambdaProvider.id)
      laserLambda.artifactDescription.computePathOverride must beNone
    }

    "specify computePathOverride if preWarm > 0" in new FakeLambdaScope {
      val Success(laserLambda) = toLaserLambda(testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "pre_warm" -> "1"
        ))
      ), testLambdaProvider.id)
      laserLambda.artifactDescription.computePathOverride must beSome(
        s"/${testOrg.name}/environments/${testEnv.id}/containers"
      )
    }

    "return computePathOverride from Secrets" in new FakeLambdaScope {
      val Success(s1) = createSecret("s1", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ))
      val Success(s2) = createSecret("s2", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ))
      val Success(s3) = createSecret("s3", Map(
        "provider" -> Json.obj("id" -> caasProviderId).toString
      ))
      val testSecretMounts = Seq(
        SecretDirMount(s1.id, "/mnt/dir"),
        SecretFileMount(s2.id, "/mnt/dir/file", "secret_key"),
        SecretEnvMount(s3.id, "ENV_VAR", "secret_key")
      )

      val Success(laserLambda) = toLaserLambda(testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(testSecretMounts).toString
        ))
      ), testLambdaProvider.id)
      laserLambda.artifactDescription.computePathOverride must beSome(
        s"/${testOrg.name}/environments/${testEnv.id}/containers"
      )
    }

  }

  "LambdaMethods" should {

    "deep patch against headers should propagate to laser" in new FakeLambdaScope {
      mockJsonClient.get(meq(s"/lambdas/${testLambda.id}"), any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
        mockResp.json returns Json.toJson(LaserLambda(
          Some(testLambda.id.toString),
          None,
          false,
          None,
          LaserArtifactDescription(None,"runtime","handler",0,0.0)
        ))
        mockResp
      })
      mockJsonClient.put(meq(s"/lambdas/${testLambda.id}"), any, any, any)(any) returns Future.successful({
        // LambdaMethods doesn't actually care about the response as long as it is a 200
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
      })

      val updatedLambda = await(lambdaMethods.patchLambdaHandler(
        r = testLambda,
        patch = PatchDocument(
          PatchOp.Replace("/properties/headers/Accept","text/plain"),
          PatchOp.Add("/properties/headers/New-Header","NewHeaderValue"),
          PatchOp.Remove("/properties/headers/Remove-Header")
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/lambdas/${testLambda.id}")
      ))

      Json.parse(updatedLambda.properties.get("headers")) must_== Json.obj(
        "Accept" -> "text/plain",
        "Existing-Header" -> "ExistingHeaderValue",
        "New-Header" -> "NewHeaderValue"
      )

      there was one(mockJsonClient).put(
        uri = meq(s"/lambdas/${testLambda.id}"),
        payload = argThat((js: Option[JsValue]) => {
          js.get.as[LaserLambda].artifactDescription.headers must_== Map(
            "Accept" -> "text/plain",
            "Existing-Header" -> "ExistingHeaderValue",
            "New-Header" -> "NewHeaderValue"
          )
        }),
        hdrs = any,
        timeout = any
      )(any)
    }

    "patch against url lambda provider" in new FakeLambdaScope {
      mockJsonClient.get(meq(s"/lambdas/${testLambda.id}"), any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
        mockResp.json returns Json.toJson(LaserLambda(
          Some(testLambda.id.toString),
          None,
          false,
          None,
          LaserArtifactDescription(None,"runtime","handler",0,0.0)
        ))
        mockResp
      })
      mockJsonClient.put(meq(s"/lambdas/${testLambda.id}"), any, any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
      })

      val updatedLambda = await(lambdaMethods.patchLambdaHandler(
        r = testLambda,
        patch = PatchDocument(
          PatchOp.Replace("/properties/public",        true),
          PatchOp.Replace("/properties/compressed",    true),
          PatchOp.Replace("/properties/package_url",   "http://code.com/hello.js"),
          PatchOp.Replace("/properties/handler",       "hello.js;hello"),
          PatchOp.Replace("/properties/timeout",       300),
          PatchOp.Replace("/properties/runtime",       "newruntime"),
          PatchOp.Replace("/properties/cpus",          2.0),
          PatchOp.Replace("/properties/memory",        2048),
          PatchOp.Replace("/properties/code_type",     "package"),
          PatchOp.Replace("/properties/periodic_info", Json.obj("new" -> "periodicity")),
          PatchOp.Replace("/properties/env",           Json.obj("new" -> "env")),
          PatchOp.Replace("/properties/pre_warm",      3)
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/lambdas/${testLambda.id}")
      ))

      updatedLambda.properties.get("public") must_== "true"
      updatedLambda.properties.get("compressed") must_== "true"
      updatedLambda.properties.get("package_url") must_== "http://code.com/hello.js"
      updatedLambda.properties.get("handler") must_== "hello.js;hello"
      updatedLambda.properties.get("timeout") must_== "300"
      updatedLambda.properties.get("runtime") must_== "newruntime"
      updatedLambda.properties.get("cpus").toDouble must_== 2.0
      updatedLambda.properties.get("memory") must_== "2048"
      updatedLambda.properties.get("code_type") must_== "package"
      updatedLambda.properties.get("periodic_info") must_== Json.obj("new" -> "periodicity").toString
      updatedLambda.properties.get("env") must_== Json.obj("new" -> "env").toString
      updatedLambda.properties.get("pre_warm") must_== "3"

      there was one(mockJsonClient).put(
        uri = meq(s"/lambdas/${testLambda.id}"),
        payload = argThat((((_:Option[JsValue]).get.as[LaserLambda]) ^^ (
            (((_:LaserLambda).id) ^^ beSome(testLambda.id.toString))
              and
              (((_:LaserLambda).public) ^^ beTrue)
              and
              (((_:LaserLambda).artifactDescription) ^^ be_==(LaserArtifactDescription(
                cpus = 2.0,
                memorySize = 2048,
                timeoutSecs = 300,
                artifactUri = Some("http://code.com/hello.js"),
                code = None,
                runtime = "newruntime",
                handler = "hello.js;hello",
                description = None,
                compressed = true,
                publish = false,
                role = "none",
                periodicInfo = Some(Json.obj("new" -> "periodicity")),
                secrets = Some(Seq.empty),
                preWarm = Some(3),
                computePathOverride = Some(s"/${testOrg.name}/environments/${testEnv.id}/containers"),
                headers = Map(
                  "Existing-Header" -> "ExistingHeaderValue",
                  "Remove-Header" -> "Nobody Cares What's Here"
                )
              )))
            )
          )
        ),
        hdrs = any,
        timeout = any
      )(any)
    }

    "patch against code lambda provider removing periodic_info" in new FakeLambdaScope {
      mockJsonClient.get(meq(s"/lambdas/${testLambda.id}"), any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
        mockResp.json returns Json.toJson(LaserLambda(
          Some(testLambda.id.toString),
          None,
          false,
          None,
          LaserArtifactDescription(None,"runtime","handler",0,0.0,periodicInfo = Some(Json.obj()))
        ))
        mockResp
      })
      mockJsonClient.put(meq(s"/lambdas/${testLambda.id}"), any, any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
      })

      val updatedLambda = await(lambdaMethods.patchLambdaHandler(
        r = testLambda,
        patch = PatchDocument(
          PatchOp.Remove("/properties/periodic_info")
        ),
        user = user,
        request = FakeRequest(HttpVerbs.PATCH, s"/root/lambdas/${testLambda.id}")
      ))

      updatedLambda.properties.get.get("periodic_info") must beNone

      there was one(mockJsonClient).put(
        uri = meq(s"/lambdas/${testLambda.id}"),
        payload = (((_:Option[JsValue]).get.as[LaserLambda].artifactDescription.periodicInfo) ^^ beNone),
        hdrs = any,
        timeout = any
      )(any)
    }

    "delete against LambdaMethods deletes lambdas" in new FakeLambdaScope {
      mockJsonClient.delete(meq(s"/lambdas/${testLambda.id}"), any, any)(any) returns Future.successful({
        val mockResp = mock[WSResponse]
        mockResp.status returns 200
      })

      val Success(_) = lambdaMethods.deleteLambdaHandler(testLambda)

      there was one(mockJsonClient).delete(
        uri = meq(s"/lambdas/${testLambda.id}"),
        hdrs = any,
        timeout = any
      )(any)
    }

  }

}
