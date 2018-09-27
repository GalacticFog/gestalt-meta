package services

import akka.actor.{ActorRef, ActorSystem}
import akka.pattern.ask
import akka.stream.Materializer
import com.galacticfog.gestalt.caas.kube.{Ascii, KubeConfig}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ResourceNotFoundException}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.google.inject.name.Named
import com.google.inject.{Inject, Singleton}
import play.api.Logger
import services.KubeTokenActor.{KubeAuthTokenError, KubeAuthTokenResponse}
import skuber.api.client.RequestContext

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import scala.language.{implicitConversions, postfixOps}

trait SkuberFactory {
  def initializeKube( provider: GestaltResourceInstance, namespace: String )
                    ( implicit ec: ExecutionContext ): Future[RequestContext]
}

@Singleton
class DefaultSkuberFactory @Inject()(@Named(KubeTokenActor.name) kubeTokenActor: ActorRef,
                                     config: play.api.Configuration)
                                    (implicit actorSystem: ActorSystem, mat: Materializer) extends SkuberFactory {

  val timeoutInSeconds: Long = config.getLong("skuberFactory.execAuthTimeoutInSeconds").getOrElse(30)
  implicit val timeout = akka.util.Timeout(FiniteDuration(timeoutInSeconds, "seconds"))

  val log = Logger(this.getClass)

  final val whitelistedCmdPaths: Set[String] = config.getStringList("skuberFactory.execWhiteList").map(_.toSeq).getOrElse(Seq.empty[String]).toSet

  def getToken(req: KubeTokenActor.KubeAuthTokenRequest)
              (implicit timeout: akka.util.Timeout): Future[skuber.api.client.AuthInfo] = {
    for {
      resp <- kubeTokenActor ? req
      token <- resp match {
        case KubeAuthTokenResponse(token) =>
          Future.successful(skuber.api.client.TokenAuth(token))
        case KubeAuthTokenError(msg) =>
          Future.failed(new RuntimeException(msg))
      }
    } yield token
  }

  /**
    *
    */
  override def initializeKube( provider: GestaltResourceInstance, namespace: String )
                             ( implicit ec: ExecutionContext ): Future[RequestContext] = {
    for {
      configYaml  <- loadProviderConfiguration(provider)
      initialConfig = KubeConfig.parseYaml(configYaml, Map.empty)
      newAuth <- initialConfig.currentContext.authInfo match {
        case gcp: skuber.api.client.GcpAuth if whitelistedCmdPaths.contains(gcp.command)=>
          getToken(KubeTokenActor.KubeAuthTokenRequest(provider.id, configYaml.hashCode, gcp))
        case exec: skuber.api.client.ExecAuth if whitelistedCmdPaths.contains(exec.command) =>
          getToken(KubeTokenActor.KubeAuthTokenRequest(provider.id, configYaml.hashCode, exec))
        case exec: skuber.api.client.ExecAuth =>
          Future.failed(new BadRequestException("Kubernetes external authenticator was configured command not present on the configured white-list."))
        case gcp: skuber.api.client.GcpAuth =>
          Future.failed(new BadRequestException("Kubernetes GCP authenticator was configured command not present on the configured white-list."))
        case other => Future.successful(other)
      }
      finalconfig = {
        val ctx = initialConfig.currentContext.copy(
          namespace = skuber.Namespace(metadata = skuber.ObjectMeta(name = namespace)),
          authInfo = newAuth
        )
        initialConfig.copy(
          currentContext = ctx
        )
      }
    } yield skuber.api.client.init(finalconfig)
  }

  /**
    * Get kube configuration from Provider. Performs lookup and validation of provider type.
    */
  private[services] def loadProviderConfiguration(provider: GestaltResourceInstance)
                                                 (implicit ec: ExecutionContext): Future[String] = Future {
    if (provider.typeId != ResourceIds.KubeProvider)
      throw ResourceNotFoundException(s"Provider '$provider' is not a Kubernetes Provider")
    else extractKubeConfig(provider.properties) getOrElse {
      throw new RuntimeException(s"Provider configuration not found. This is a bug")
    }
  }

  /**
    * Get kube configuration from provider.properties. Decode if necessary.
    */
  private[services] def extractKubeConfig(props: Option[Map[String, String]]): Option[String] = {
    props flatMap { ps =>
      ps.get("data").map { config =>
        if (Ascii.isBase64(config)) Ascii.decode64(config) else config
      }
    }
  }
}