package com.galacticfog.gestalt.integrations.kubernetes

import java.util.UUID

import com.typesafe.config.Config
import akka.actor.{ActorSystem, ActorRef}
import akka.stream.Materializer
import akka.pattern.ask
import play.api.Logger
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.caas.kube.{Ascii, KubeConfig}
import skuber.api.client.RequestContext

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

trait KubeToken {

  val actor: ActorRef

  val whitelistedCmdPaths: Set[String]
  val timeout: FiniteDuration

  val log = Logger(this.getClass)

  def getToken(req: KubeTokenActor.KubeAuthTokenRequest)
   (implicit ec: ExecutionContext, system: ActorSystem, materializer: Materializer): Future[skuber.api.client.AuthInfo] = {
    log.info(s"acquiring token for provider ${req.providerId} from KubeTokenActor")
    implicit val askTimeout = akka.util.Timeout(timeout)
    for {
      resp <- actor ? req
      token <- resp match {
        case KubeTokenActor.KubeAuthTokenResponse(token) =>
          log.info("successfully acquired token")
          Future.successful(skuber.api.client.TokenAuth(token))
        case KubeTokenActor.KubeAuthTokenError(msg) =>
          log.info("failed to acquire token")
          Future.failed(new RuntimeException(msg))
      }
    } yield token
  }

  def mkKubeconfig(providerId: UUID, providerKubeconfig: Option[String], namespace: String, appConfig: Option[Config])
   (implicit ec: ExecutionContext, system: ActorSystem, materializer: Materializer): Future[RequestContext] = {
    def init(config: Option[skuber.api.Configuration] = None): RequestContext = {
      (config, appConfig) match {
        case (Some(a), Some(b)) => skuber.api.client.init(a, b)
        case (Some(a), None) => skuber.api.client.init(a)
        case (None, Some(b)) => skuber.api.client.init(b)
        case (None, None) => skuber.api.client.init()
      }
    }

    providerKubeconfig.fold {
      log.info(s"No configuration data found for Provider '${providerId}'. Proceeding with default config...")
      val requestContext = init()
      log.debug(s"default namespace: ${requestContext.namespaceName}")
      log.debug(s"switching to namespace: $namespace")
      Future.successful(requestContext.usingNamespace(namespace))
    } { yaml =>
      val configYaml = if(Ascii.isBase64(yaml)) Ascii.decode64(yaml) else yaml
      val initialConfig = KubeConfig.parseYaml(configYaml, Map.empty)
      log.info(s"parsed kubeconfig for provider ${providerId}, authInfo of type ${initialConfig.currentContext.authInfo.getClass.getSimpleName}")            
      
      for {
        newAuth <- initialConfig.currentContext.authInfo match {
          case gcp: skuber.api.client.GcpAuth if whitelistedCmdPaths.contains(gcp.command) =>
            getToken(KubeTokenActor.KubeAuthTokenRequest(providerId, configYaml.hashCode, gcp))
          case exec: skuber.api.client.ExecAuth if whitelistedCmdPaths.contains(exec.command) =>
            getToken(KubeTokenActor.KubeAuthTokenRequest(providerId, configYaml.hashCode, exec))
          case exec: skuber.api.client.ExecAuth =>
            Future.failed(new BadRequestException("Kubernetes external authenticator was configured command not present on the configured white-list."))
          case gcp: skuber.api.client.GcpAuth =>
            Future.failed(new BadRequestException("Kubernetes GCP authenticator was configured command not present on the configured white-list."))
          case other => Future.successful(other)
        }
        finalconfig = {
          val ctx = initialConfig.currentContext.copy (
            namespace = skuber.Namespace(metadata = skuber.ObjectMeta(name = namespace)),
            authInfo = newAuth
          )
          initialConfig.copy(currentContext = ctx)
        }
      } yield init(Some(finalconfig))
    }
  }

}