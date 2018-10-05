package services

import java.util.UUID
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
import com.google.inject.Inject
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
// import play.api.Logger
import com.galacticfog.gestalt.integrations.ecs._

trait AwsSdkFactory {
  def getEcsClient(provider: UUID)(implicit ec: ExecutionContext): Future[EcsClient]
}

class DefaultAwsSdkFactory @Inject()() extends AwsSdkFactory {
  val clientFactory = new EcsClientFactory()

  override def getEcsClient(provider: UUID)(implicit ec: ExecutionContext): Future[EcsClient] = {
    val prv = ResourceFactory.findById(provider) getOrElse {
      throw new ResourceNotFoundException(s"Provider with ID '$provider' not found.")
    }
    if(prv.typeId != migrations.V14.ECS_PROVIDER_TYPE_ID) {
      throw ResourceNotFoundException(s"Provider '$provider' is not an ECS provider")
    }else {
      (for(
        props <- prv.properties;
        config <- props.get("config")
      ) yield {
        clientFactory.getEcsClient(config)
      }) match {
        case None => throw new RuntimeException("Empty ECS provider configuration. This is a bug")
        case Some(Left(errorMessage)) => throw new RuntimeException(s"Malformed ECS provider configuration: $errorMessage")
        case Some(Right(client)) => Future.successful(client)
      }
    }
  }
}