package com.galacticfog.gestalt.meta.providers.faas

import java.util.UUID
import scala.util.{Try,Success}
import scala.concurrent.Await
import scala.concurrent.duration._
import play.api.test.PlaySpecification
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.inject.bind
import play.api.libs.json._
import play.api.mvc._
import play.api.mvc.Results._
import org.specs2.specification.{BeforeAll, Scope}
import org.specs2.matcher.JsonMatchers
import mockws.MockWS
import com.galacticfog.gestalt.meta.api.errors.GenericApiException
import com.galacticfog.gestalt.meta.test.{DbShutdown, ResourceScope}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.ContainerSpec.{SecretDirMount, SecretEnvMount, SecretFileMount}
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import controllers.util.{ProviderMethods,GestaltSecurityMocking}
import controllers.SecurityResources

class LaserProviderSpec extends PlaySpecification with GestaltSecurityMocking with ResourceScope with BeforeAll with DbShutdown with JsonMatchers {

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

    // val mockProviderMethods = mock[ProviderMethods]
    val mockGestaltSecurityConfig = mock[GestaltSecurityConfig]
    val appBuilder = new GuiceApplicationBuilder()
      .disable[modules.ProdSecurityModule]
      .disable[modules.MetaDefaultSkuber]
      .disable[modules.MetaDefaultServices]
      .disable[modules.HealthModule]
      .bindings(
        bind(classOf[GestaltSecurityConfig]).toInstance(mockGestaltSecurityConfig)//,
        // bind(classOf[ProviderMethods]).toInstance(mockProviderMethods)
      )
    val injector = appBuilder.injector()
    setInjector(injector)

    // val lambdaMethods = injector.instanceOf[LambdaMethods]
    val providerMethods = injector.instanceOf[ProviderMethods]
    // val laserProvider = injector.instanceOf[LaserProvider]

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
        "id" -> testLambdaProvider.id.toString//,
        //"locations" -> Json.arr()
      ).toString
    ))

    // val mockJsonClient = mock[JsonClient]
    // mockProviderMethods.configureWebClient(argThat(
    //   (provider: GestaltResourceInstance) => provider.id == testLambdaProvider.id
    // ), any) returns mockJsonClient
  }

  "createLambda" should {
    "create lambda" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://laser.service:1111/lambdas") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)
      val Success(newTestLambda) = createLambda("test-lambda", Map(
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
        "isolate" -> "true"
      ))
      val _ = Await.result(laserProvider.createLambda(testLambdaProvider, newTestLambda), 10 .seconds)
      val removeEventFilter = (__ \ 'eventFilter).json.prune
      val JsSuccess(rr, _) = r.transform(removeEventFilter)
      rr must beEqualTo(Json.obj(
        "id" -> newTestLambda.id.toString,
        // "eventFilter" -> "38446d4f-9783-4ace-8c23-2410aa2b9fd2",
        "public" -> true,
        "provider" -> Json.obj(                             
          "id" -> testLambdaProvider.id.toString,
          "location" -> "",
          "href" -> "/foo/bar"
        ),
        "artifactDescription" -> Json.obj(
          "runtime" -> "custom",
          "handler" -> "blah;blah",
          "memorySize" -> 128,
          "cpus" -> 0.1,
          "compressed" -> false,
          "publish" -> false,
          "role" -> "none",
          "timeoutSecs" -> 30,
          "periodicInfo" -> Json.obj(),
          "headers" -> Json.obj(),
          "secrets" -> Json.arr(),
          "isolate" -> true
        )
      ))
    }

    "include all secret mounts" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://laser.service:1111/lambdas") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)
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
      val _ = Await.result(laserProvider.createLambda(testLambdaProvider, testLambdaWithSecrets), 10 .seconds)
      val removeEventFilter = (__ \ 'eventFilter).json.prune
      val removeComputePathOverride = (__ \ 'artifactDescription \ 'computePathOverride).json.prune
      val JsSuccess(rr, _) = r.transform(removeEventFilter).flatMap(_.transform(removeComputePathOverride))
      rr must beEqualTo(Json.obj(
        "id" -> testLambdaWithSecrets.id.toString,
        // "eventFilter" -> "38446d4f-9783-4ace-8c23-2410aa2b9fd2",
        "public" -> true,
        "provider" -> Json.obj(                             
          "id" -> testLambdaProvider.id.toString,
          "location" -> "",
          "href" -> "/foo/bar"
        ),
        "artifactDescription" -> Json.obj(
          "runtime" -> "custom",
          "handler" -> "blah;blah",
          "memorySize" -> 128, 
          "cpus" -> 0.1,
          "compressed" -> false,
          "publish" -> false,
          "role" -> "none",
          "timeoutSecs" -> 30,
          "periodicInfo" -> Json.obj(),
          "headers" -> Json.obj(),
          // "computePathOverride" -> "/1cdc1e6d-0366-4185-8eb2-45c44b36d3b9/environments/313627a5-4da5-4e66-a782-fdd447f4a8c8/containers",
          "secrets" -> Json.arr(Json.obj(
            "secret_id" -> s1.id,
            "path" -> "/mnt/dir",
            "mount_type" -> "directory"
          ), Json.obj(
            "secret_id" -> s2.id,
            "path" -> "/mnt/dir/file",
            "secret_key" -> "secret_key",
            "mount_type" -> "file"
          ), Json.obj(
            "secret_id" -> s3.id,
            "path" -> "ENV_VAR",
            "secret_key" -> "secret_key",
            "mount_type" -> "env"
          ))
        )
      ))
    }

    "fail if lambda provider references a non-existent secret" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://laser.service:1111/lambdas") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)
      val secretUuid = uuid()
      val newTestLambda = testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(Seq(SecretEnvMount(secretUuid, "ENV_VAR", "secret_key"))).toString
        ))
      )
      val t = Try(Await.result(laserProvider.createLambda(testLambdaProvider, newTestLambda), 10 .seconds))
      t must beFailedTry.withThrowable[GenericApiException](s"Secret '${secretUuid}' does not exist")
    }

    "fail if lambda provider is not conformant with secrets provider" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://laser.service:1111/lambdas") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)
      val Success(secret) = createSecret( "nonconformant-secret", Map(
        "provider" -> Json.obj(
          "id" -> uuid().toString
        ).toString
      ))
      val newTestLambda = testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(Seq(SecretEnvMount(secret.id, "ENV_VAR", "secret_key"))).toString
        ))
      )
      val t = Try(Await.result(laserProvider.createLambda(testLambdaProvider, newTestLambda), 10 .seconds))
      t must beFailedTry.withThrowable[GenericApiException](s"Lambda '${testLambda.id}' provider '${testLambdaProvider.id}' did not have same CaaS provider as mounted Secrets")
    }

    "fail if secrets aren't mutual siblings" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://laser.service:1111/lambdas") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)

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
      val newTestLambda = testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(testSecretMounts).toString
        ))
      )
      
      val t = Try(Await.result(laserProvider.createLambda(testLambdaProvider, newTestLambda), 10 .seconds))
      t must beFailedTry.withThrowable[GenericApiException]("All mounted Secrets must belong to the same Environment")
    }

    "fail if secrets aren't siblings with lambda" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://laser.service:1111/lambdas") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)

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

      val newTestLambda = testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(testSecretMounts).toString
        ))
      )
      
      val t = Try(Await.result(laserProvider.createLambda(testLambdaProvider, newTestLambda), 10 .seconds))
      t must beFailedTry.withThrowable[GenericApiException](s"Lambda '${testLambda.id}' must belong to the same Environment as all mounted Secrets")
    }

    "not specify computePathOverride in the absence of secrets" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://laser.service:1111/lambdas") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)

      val _ = Await.result(laserProvider.createLambda(testLambdaProvider, testLambda), 10 .seconds)
      (r \ "artifactDescription" \ "computePathOverride").asOpt[String] must beNone
    }

    "specify computePathOverride if preWarm > 0" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://laser.service:1111/lambdas") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)

      val newTestLambda = testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "pre_warm" -> "1"
        ))
      )
      
      val _ = Await.result(laserProvider.createLambda(testLambdaProvider, newTestLambda), 10 .seconds)
      (r \ "artifactDescription" \ "computePathOverride").asOpt[String] must beSome
    }

    "return computePathOverride from Secrets" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (POST, "http://laser.service:1111/lambdas") => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)

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

      val newTestLambda = testLambda.copy(
        properties = Some(testLambda.properties.get ++ Map(
          "secrets" -> Json.toJson(testSecretMounts).toString
        ))
      )

      val _ = Await.result(laserProvider.createLambda(testLambdaProvider, newTestLambda), 10 .seconds)
      (r \ "artifactDescription" \ "computePathOverride").asOpt[String] must beSome(s"/${testOrg.name}/environments/${testEnv.id}/containers")
    }
  }

  "updateLambda" should {
    "propagate deep patch against headers to laser" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (PUT, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)

      val patch = PatchDocument(
        PatchOp.Replace("/properties/headers/Accept","text/plain"),
        PatchOp.Add("/properties/headers/New-Header","NewHeaderValue"),
        PatchOp.Remove("/properties/headers/Remove-Header")
      )

      val patched = Await.result(laserProvider.updateLambda(testLambdaProvider, testLambda, patch), 10 .seconds)
      patched.properties.get.get("headers") must beEqualTo(Some("""{"Existing-Header":"ExistingHeaderValue","Accept":"text/plain","New-Header":"NewHeaderValue"}"""))
    }
    "patch against url lambda provider" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (PUT, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)

      val patch = PatchDocument(
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
      )

      val _ = Await.result(laserProvider.updateLambda(testLambdaProvider, testLambda, patch), 10 .seconds)
      val removeEventFilter = (__ \ 'eventFilter).json.prune
      val removeComputePathOverride = (__ \ 'artifactDescription \ 'computePathOverride).json.prune
      val JsSuccess(rr, _) = r.transform(removeEventFilter).flatMap(_.transform(removeComputePathOverride))
      rr must beEqualTo(Json.obj(
        "id" -> testLambda.id.toString,
        // "eventFilter" -> "38446d4f-9783-4ace-8c23-2410aa2b9fd2",
        "public" -> true,
        "provider" -> Json.obj(
          "id" -> testLambdaProvider.id.toString,
          "location" -> "",
          "href" -> "/foo/bar"
        ),
        "artifactDescription" -> Json.obj(
          "artifactUri" -> "http://code.com/hello.js",
          "runtime" -> "newruntime",
          "handler" -> "hello.js;hello",
          "memorySize" -> 2048,
          "cpus" -> 2.0,
          "compressed" -> true,
          "publish" -> false,
          "role" -> "none",
          "timeoutSecs" -> 300,
          "periodicInfo" -> Json.obj("new" -> "periodicity"),
          "headers" -> Json.obj(
            "Existing-Header" -> "ExistingHeaderValue",
            "Remove-Header" -> "Nobody Cares What's Here"
          ),
          "secrets" -> Json.arr(),
          "preWarm" -> 3
        )
      ))
    }
    "patch against code lambda provider removing periodic_info" in new FakeLambdaScope {
      var r: JsValue = null
      val ws = MockWS {
        case (PUT, _) => Action { request =>
          r = request.body.asJson.get
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)

      val patch = PatchDocument(
        PatchOp.Remove("/properties/periodic_info")
      )

      val _ = Await.result(laserProvider.updateLambda(testLambdaProvider, testLambda, patch), 10 .seconds)
      (r \ "artifactDescription" \ "periodicInfo").asOpt[Map[String,String]] must beNone
    }
  }

  "deleteLambda" should {
    "delete against LambdaMethods deletes lambdas" in new FakeLambdaScope {
      val ws = MockWS {
        case (DELETE, _) => Action {
          Ok("")
        }
      }
      val laserProvider = new LaserProvider(ws, providerMethods)

      val deleted = Await.result(laserProvider.deleteLambda(testLambdaProvider, testLambda), 10 .seconds)
      deleted must beEqualTo(())
    }
  }
}