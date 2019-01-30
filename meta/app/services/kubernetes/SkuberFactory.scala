package services.kubernetes

import akka.actor.{ActorRef, ActorSystem}
import akka.stream.Materializer
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.google.inject.name.Named
import com.google.inject.{Inject, Singleton}
import play.api.Logger
import com.galacticfog.gestalt.integrations.kubernetes.{KubeToken, KubeTokenActor}
import skuber.api.client.RequestContext

import scala.collection.JavaConversions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._

trait SkuberFactory {
  def initializeKube( provider: GestaltResourceInstance, namespace: String )
                    ( implicit ec: ExecutionContext ): Future[RequestContext]
}

@Singleton
class DefaultSkuberFactory @Inject()(@Named(KubeTokenActor.name) kubeTokenActor: ActorRef,
                                     config: play.api.Configuration)
                                    (implicit actorSystem: ActorSystem, mat: Materializer) extends SkuberFactory {

  val log = Logger(this.getClass)

  val kubeToken = new KubeToken {
    val actor: ActorRef = kubeTokenActor

    val whitelistedCmdPaths: Set[String] = config.getStringList("skuberFactory.execWhiteList").map(_.toSeq).getOrElse(Seq.empty[String]).toSet
    val timeout: FiniteDuration = config.getLong("skuberFactory.execAuthTimeoutInSeconds").getOrElse(30l) .seconds
  }
  
  /**
   *
   */
  override def initializeKube( provider: GestaltResourceInstance, namespace: String )
                             ( implicit ec: ExecutionContext ): Future[RequestContext] = {
    if (provider.typeId != ResourceIds.KubeProvider) {
      Future.failed(ResourceNotFoundException(s"Provider '$provider' is not a Kubernetes Provider"))
    }

    val providerKubeconfig = for(
      props <- provider.properties;
      config <- props.get("data")
    ) yield config

    kubeToken.mkKubeconfig(provider.id, providerKubeconfig, namespace, None)
  }
}