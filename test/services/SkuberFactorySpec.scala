package services

import java.util.Base64

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.testkit.TestKit
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.ResourceScope
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.{BeforeAll, Scope}
import play.api.test.PlaySpecification

import scala.language.implicitConversions
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
      val sf = new DefaultSkuberFactory()
      val client = await(sf.initializeKube(testProvider, "namespace-override"))
      client.namespaceName must_== "namespace-override"
      client.requestAuth must_== skuber.api.client.TokenAuth("kube-config-token-value")
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
      val sf = new DefaultSkuberFactory()
      val client = await(sf.initializeKube(testProvider, "namespace-override"))
      client.namespaceName must_== "namespace-override"
      client.requestAuth must_== skuber.api.client.TokenAuth("externally-generated-token-value")
    }

  }

}
