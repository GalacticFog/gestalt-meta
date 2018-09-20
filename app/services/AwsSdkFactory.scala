package services

import java.util.UUID
import scala.util.Try
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
import com.google.inject.Inject
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
// import play.api.Logger
import play.api.libs.json.Json
import com.amazonaws.auth.{AWSStaticCredentialsProvider, BasicAWSCredentials}
import com.amazonaws.services.ecs.{AmazonECSClientBuilder,AmazonECS}

case class AwsEcsClient(client: AmazonECS, cluster: String, taskRoleArn: String)

trait AwsSdkFactory {
  def getEcsClient(provider: UUID)(implicit ec: ExecutionContext): Future[AwsEcsClient]
}

class DefaultAwsSdkFactory @Inject()() extends AwsSdkFactory {

  // val log = Logger(this.getClass)

  type AwsSdkProperties = (String,String,String,String,String)

  private[services] def extractAwsConfig(props: Option[Map[String, String]]): Option[AwsSdkProperties] = {
    for(
      p <- props;
      config <- p.get("config");
      jsValue <- Try(Json.parse(config)).toOption;
      accessKey <- (jsValue \ "access_key").asOpt[String];
      secretKey <- (jsValue \ "secret_key").asOpt[String];
      region <- (jsValue \ "region").asOpt[String];
      cluster <- (jsValue \ "cluster").asOpt[String];
      taskRoleArn <- (jsValue \ "taskRoleArn").asOpt[String]
    ) yield {
      (accessKey, secretKey, region, cluster, taskRoleArn)
    }
  }

  private[services] def loadProviderConfiguration(provider: UUID)(
    implicit ec: ExecutionContext): Future[AwsSdkProperties] = Future {

    val prv = ResourceFactory.findById(provider) getOrElse {
      throw new ResourceNotFoundException(s"Provider with ID '$provider' not found.")
    }

    if (prv.typeId != ResourceIds.EcsProvider)
      throw ResourceNotFoundException(s"Provider '$provider' is not a AWS ECS Provider")
    else extractAwsConfig(prv.properties) getOrElse {
      throw new RuntimeException(s"Empty or malformed provider configuration. This is a bug")
    }
  }

  private[services] def buildEcsClient(credentials: AwsSdkProperties): Try[AwsEcsClient] = {
    val (accessKey, secretKey, region, cluster, taskRoleArn) = credentials
    Try(AwsEcsClient(AmazonECSClientBuilder.standard()
      .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(accessKey, secretKey)))
      .withRegion(region)
      .build(), cluster, taskRoleArn))
  }

  override def getEcsClient(provider: UUID)(implicit ec: ExecutionContext): Future[AwsEcsClient] = {
    for(
      credentials <- loadProviderConfiguration(provider);
      client <- Future.fromTry(buildEcsClient(credentials))
    ) yield client
  }
}