package services

import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.Materializer

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.language.postfixOps
import com.galacticfog.gestalt.caas.kube.Ascii
import com.galacticfog.gestalt.caas.kube.KubeConfig
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.google.inject.{Singleton,Inject}
import play.api.Logger
import skuber.api.client.RequestContext


trait SkuberFactory {
  def initializeKube( provider: UUID, namespace: String )
                    ( implicit ec: ExecutionContext ): Future[RequestContext]
}

@Singleton
class DefaultSkuberFactory @Inject()()(implicit actorSystem: ActorSystem, mat: Materializer) extends SkuberFactory {

  val log = Logger(this.getClass)

  /**
    *
    */
  override def initializeKube( provider: UUID, namespace: String )
                             ( implicit ec: ExecutionContext ): Future[RequestContext] = for {
    config  <- loadProviderConfiguration(provider)
    context <- Future.fromTry(KubeConfig.initializeString(config, namespace = Some(namespace)))
  } yield context

  /**
    * Get kube configuration from Provider. Performs lookup and validation of provider type.
    */
  private[services] def loadProviderConfiguration(provider: UUID)(
    implicit ec: ExecutionContext): Future[String] = Future {

    val prv = ResourceFactory.findById(provider) getOrElse {
      throw new ResourceNotFoundException(s"Provider with ID '$provider' not found.")
    }

    if (prv.typeId != ResourceIds.KubeProvider)
      throw ResourceNotFoundException(s"Provider '$provider' is not a Kubernetes Provider")
    else extractKubeConfig(prv.properties) getOrElse {
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