package services.kubernetes

import java.util.Base64

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.testkit.{TestActor, TestActorRef, TestKit, TestProbe}
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.integrations.kubernetes.KubeTokenActor
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAll, Scope}
import play.api.test.PlaySpecification

import scala.util.Success

@RunWith(classOf[JUnitRunner])
class SkuberFactorySpec extends PlaySpecification with ResourceScope with BeforeAll {

  override def beforeAll(): Unit = pristineDatabase()

  abstract class WithProviderConfig( kubeConfig: String ) extends TestKit(ActorSystem("MySpec")) with Scope {
    val Success((testWork, testEnv)) = createWorkEnv(wrkName = "test-workspace", envName = "test-environment")
    val Success(testProvider) = createInstance(ResourceIds.KubeProvider, "test-provider",
      parent = Some(testEnv.id),
      properties = Some(Map(
        "config" -> "{}",
        "data" -> new String(Base64.getEncoder.encode(kubeConfig.getBytes))
      ))
    )
    implicit val mat = ActorMaterializer()
    implicit val ec = system.dispatcher
  }

  "SkuberFactory" should {

    "manage token-based external authenticators" in new WithProviderConfig("""
apiVersion: v1
clusters:
- cluster:
    api-version: v1
    server: http://eks.aws.com:8080
  name: aws-eks-cluster
contexts:
- context:
    cluster: aws-eks-cluster
    namespace: some-namespace
    user: aws-exec-user
  name: eks-context
current-context: eks-context
kind: Config
users:
- name: aws-exec-user
  user:
    token: kube-config-token-value
""") {
      val sf = new DefaultSkuberFactory(testActor, play.api.Configuration.empty)
      val client = await(sf.initializeKube(testProvider, "namespace-override"))
      client.namespaceName must_== "namespace-override"
      client.asInstanceOf[skuber.api.client.impl.KubernetesClientImpl].requestAuth must_== skuber.api.client.TokenAuth("kube-config-token-value")
      expectNoMsg
    }

    "manage exec-based external authenticators" in new WithProviderConfig("""
apiVersion: v1
clusters:
- cluster:
    api-version: v1
    server: http://eks.aws.com:8080
  name: aws-eks-cluster
contexts:
- context:
    cluster: aws-eks-cluster
    namespace: some-namespace
    user: aws-exec-user
  name: eks-context
current-context: eks-context
kind: Config
users:
- name: aws-exec-user
  user:
    exec:
      apiVersion: "client.authentication.k8s.io/v1alpha1"
      command: "/usr/local/bin/heptio-authenticator-aws"
      args: ["token", "-i", "CLUSTER_ID", "-r", "ROLE_ARN"]
      env:
      - name: "1"
        value: "2"
      - name: "3"
        value: "4"
""") {
      val probe = TestProbe()
      probe.setAutoPilot(new TestActor.AutoPilot {
        def run(sender: ActorRef, msg: Any): TestActor.AutoPilot = {
          msg match {
            case KubeTokenActor.KubeAuthTokenRequest(pid, _, auth) if pid == testProvider.id && auth.isInstanceOf[skuber.api.client.ExecAuth] =>
              sender ! KubeTokenActor.KubeAuthTokenResponse("externally-generated-token-value")
            case _ =>
              sender ! akka.actor.Status.Failure(new RuntimeException("not what I was expecting"))
          }
          TestActor.NoAutoPilot
        }
      })

      val sf = new DefaultSkuberFactory(probe.ref, play.api.Configuration(
        "skuberFactory.execWhiteList" -> Seq("/usr/local/bin/heptio-authenticator-aws")
      ))
      val client = await(sf.initializeKube(testProvider, "namespace-override"))
      client.namespaceName must_== "namespace-override"
      client.asInstanceOf[skuber.api.client.impl.KubernetesClientImpl].requestAuth must_== skuber.api.client.TokenAuth("externally-generated-token-value")
    }

    "fail on non-white-listed external authenticators" in new WithProviderConfig("""
apiVersion: v1
clusters:
- cluster:
    api-version: v1
    server: http://eks.aws.com:8080
  name: aws-eks-cluster
contexts:
- context:
    cluster: aws-eks-cluster
    namespace: some-namespace
    user: aws-exec-user
  name: eks-context
current-context: eks-context
kind: Config
users:
- name: aws-exec-user
  user:
    exec:
      apiVersion: "client.authentication.k8s.io/v1alpha1"
      command: "/usr/local/bin/not-in-the-white-list"
      args: ["token", "-i", "CLUSTER_ID", "-r", "ROLE_ARN"]
      env:
      - name: "1"
        value: "2"
      - name: "3"
        value: "4"
""") {
      val probe = TestProbe()
      probe.setAutoPilot(new TestActor.AutoPilot {
        def run(sender: ActorRef, msg: Any): TestActor.AutoPilot = {
          msg match {
            case KubeTokenActor.KubeAuthTokenRequest(pid, _, auth) if pid == testProvider.id && auth.isInstanceOf[skuber.api.client.ExecAuth] =>
              sender ! KubeTokenActor.KubeAuthTokenResponse("externally-generated-token-value")
            case _ =>
              sender ! akka.actor.Status.Failure(new RuntimeException("not what I was expecting"))
          }
          TestActor.NoAutoPilot
        }
      })

      val sf = new DefaultSkuberFactory(probe.ref, play.api.Configuration.empty)
      await(sf.initializeKube(testProvider, "namespace-override")) must throwAn[BadRequestException]("Kubernetes external authenticator was configured command not present on the configured white-list.")
    }

  }

  "KubeTokenActor" should {

    "expire tokens" in new WithProviderConfig("") {
      val providerId = uuid()
      val configHash = 1
      val tokenActor = TestActorRef(new KubeTokenActor(play.api.Configuration(
        "skuberFactory.tokenExpiryInSeconds" -> -1
      )))

      val firstToken = uuid().toString
      val firstResponse = await(tokenActor ? KubeTokenActor.KubeAuthTokenRequest(
        providerId = providerId,
        configHash = 1,
        tokenAuth = skuber.api.client.TokenAuth(firstToken) // HINT: this is acting a stand-in here for GcpAuth or ExecAuth, handy because AccessTokenAuth is a sealed trait
      ))
      firstResponse must_== KubeTokenActor.KubeAuthTokenResponse(firstToken)
      tokenActor.underlyingActor.providerTokens.get(providerId) must beSome(
        (((_:KubeTokenActor.CachedToken).accessToken) ^^ be_==(firstToken))
        and
        (((_:KubeTokenActor.CachedToken).configHash) ^^ be_==(configHash))
        and
        (((_:KubeTokenActor.CachedToken).expiry) ^^ beLessThan(java.time.Instant.now()))
      )

      val secondToken = uuid().toString
      val secondResponse = await(tokenActor ? KubeTokenActor.KubeAuthTokenRequest(
        providerId = providerId,
        configHash = 1,
        tokenAuth = skuber.api.client.TokenAuth(secondToken) // HINT: this is acting a stand-in here for GcpAuth or ExecAuth, handy because AccessTokenAuth is a sealed trait
      ))
      secondResponse must_== KubeTokenActor.KubeAuthTokenResponse(secondToken)
      tokenActor.underlyingActor.providerTokens.get(providerId) must beSome(
        (((_:KubeTokenActor.CachedToken).accessToken) ^^ be_==(secondToken))
          and
          (((_:KubeTokenActor.CachedToken).configHash) ^^ be_==(configHash))
          and
          (((_:KubeTokenActor.CachedToken).expiry) ^^ beLessThan(java.time.Instant.now()))
      )
    }

    "not re-use tokens if configHash has changed" in new WithProviderConfig("") {
      val providerId = uuid()
      val originalConfigHash = 1
      val tokenActor = TestActorRef(new KubeTokenActor(play.api.Configuration(
        "skuberFactory.tokenExpiryInSeconds" -> 600
      )))

      val firstToken = uuid().toString
      val firstResponse = await(tokenActor ? KubeTokenActor.KubeAuthTokenRequest(
        providerId = providerId,
        configHash = originalConfigHash,
        tokenAuth = skuber.api.client.TokenAuth(firstToken) // HINT: this is acting a stand-in here for GcpAuth or ExecAuth, handy because AccessTokenAuth is a sealed trait
      ))
      firstResponse must_== KubeTokenActor.KubeAuthTokenResponse(firstToken)
      tokenActor.underlyingActor.providerTokens.get(providerId) must beSome(
        (((_:KubeTokenActor.CachedToken).accessToken) ^^ be_==(firstToken))
          and
          (((_:KubeTokenActor.CachedToken).configHash) ^^ be_==(originalConfigHash))
          and
          (((_:KubeTokenActor.CachedToken).expiry) ^^ beGreaterThan(java.time.Instant.now()))
      )

      val secondToken = uuid().toString
      val newConfigHash = 2
      val secondResponse = await(tokenActor ? KubeTokenActor.KubeAuthTokenRequest(
        providerId = providerId,
        configHash = newConfigHash,
        tokenAuth = skuber.api.client.TokenAuth(secondToken) // HINT: this is acting a stand-in here for GcpAuth or ExecAuth, handy because AccessTokenAuth is a sealed trait
      ))
      secondResponse must_== KubeTokenActor.KubeAuthTokenResponse(secondToken)
      tokenActor.underlyingActor.providerTokens.get(providerId) must beSome(
        (((_:KubeTokenActor.CachedToken).accessToken) ^^ be_==(secondToken))
          and
          (((_:KubeTokenActor.CachedToken).configHash) ^^ be_==(newConfigHash))
          and
          (((_:KubeTokenActor.CachedToken).expiry) ^^ beGreaterThan(java.time.Instant.now()))
      )
    }

    "consider multiple providers" in new WithProviderConfig("") {
      val sameConfigHash = 1
      val tokenActor = TestActorRef(new KubeTokenActor(play.api.Configuration(
        "skuberFactory.tokenExpiryInSeconds" -> 600
      )))

      val firstProviderId = uuid()
      val firstToken = uuid().toString
      val firstResponse = await(tokenActor ? KubeTokenActor.KubeAuthTokenRequest(
        providerId = firstProviderId,
        configHash = sameConfigHash,
        tokenAuth = skuber.api.client.TokenAuth(firstToken) // HINT: this is acting a stand-in here for GcpAuth or ExecAuth, handy because AccessTokenAuth is a sealed trait
      ))
      firstResponse must_== KubeTokenActor.KubeAuthTokenResponse(firstToken)
      tokenActor.underlyingActor.providerTokens.get(firstProviderId) must beSome(
        (((_:KubeTokenActor.CachedToken).accessToken) ^^ be_==(firstToken))
          and
          (((_:KubeTokenActor.CachedToken).configHash) ^^ be_==(sameConfigHash))
          and
          (((_:KubeTokenActor.CachedToken).expiry) ^^ beGreaterThan(java.time.Instant.now()))
      )
      tokenActor.underlyingActor.providerTokens must haveSize(1)

      val secondProviderId = uuid()
      val secondToken = uuid().toString
      val secondResponse = await(tokenActor ? KubeTokenActor.KubeAuthTokenRequest(
        providerId = secondProviderId,
        configHash = sameConfigHash,
        tokenAuth = skuber.api.client.TokenAuth(secondToken) // HINT: this is acting a stand-in here for GcpAuth or ExecAuth, handy because AccessTokenAuth is a sealed trait
      ))
      secondResponse must_== KubeTokenActor.KubeAuthTokenResponse(secondToken)
      tokenActor.underlyingActor.providerTokens.get(secondProviderId) must beSome(
        (((_:KubeTokenActor.CachedToken).accessToken) ^^ be_==(secondToken))
          and
          (((_:KubeTokenActor.CachedToken).configHash) ^^ be_==(sameConfigHash))
          and
          (((_:KubeTokenActor.CachedToken).expiry) ^^ beGreaterThan(java.time.Instant.now()))
      )
      tokenActor.underlyingActor.providerTokens must haveSize(2)

    }

    "use cached tokens" in new WithProviderConfig("") {
      val providerId = uuid()
      val configHash = 1
      val tokenActor = TestActorRef(new KubeTokenActor(play.api.Configuration(
        "skuberFactory.tokenExpiryInSeconds" -> 600
      )))

      val firstToken = uuid().toString
      val firstResponse = await(tokenActor ? KubeTokenActor.KubeAuthTokenRequest(
        providerId = providerId,
        configHash = configHash,
        tokenAuth = skuber.api.client.TokenAuth(firstToken) // HINT: this is acting a stand-in here for GcpAuth or ExecAuth, handy because AccessTokenAuth is a sealed trait
      ))
      firstResponse must_== KubeTokenActor.KubeAuthTokenResponse(firstToken)
      tokenActor.underlyingActor.providerTokens.get(providerId) must beSome(
        (((_:KubeTokenActor.CachedToken).accessToken) ^^ be_==(firstToken))
          and
          (((_:KubeTokenActor.CachedToken).configHash) ^^ be_==(configHash))
          and
          (((_:KubeTokenActor.CachedToken).expiry) ^^ beGreaterThan(java.time.Instant.now()))
      )
      tokenActor.underlyingActor.providerTokens must haveSize(1)
      val cachedExpiry = tokenActor.underlyingActor.providerTokens.head._2.expiry

      val secondToken = uuid().toString
      val secondResponse = await(tokenActor ? KubeTokenActor.KubeAuthTokenRequest(
        providerId = providerId,
        configHash = configHash,
        tokenAuth = skuber.api.client.TokenAuth(secondToken) // HINT: this is acting a stand-in here for GcpAuth or ExecAuth, handy because AccessTokenAuth is a sealed trait
      ))
      secondResponse must_== KubeTokenActor.KubeAuthTokenResponse(firstToken)
      tokenActor.underlyingActor.providerTokens.get(providerId) must beSome(
        (((_:KubeTokenActor.CachedToken).accessToken) ^^ be_==(firstToken))
          and
          (((_:KubeTokenActor.CachedToken).configHash) ^^ be_==(configHash))
          and
          (((_:KubeTokenActor.CachedToken).expiry) ^^ be_==(cachedExpiry))
      )
      tokenActor.underlyingActor.providerTokens must haveSize(1)

    }
  }

}
