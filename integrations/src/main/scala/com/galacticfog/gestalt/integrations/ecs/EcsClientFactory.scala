package com.galacticfog.gestalt.integrations.ecs

// import play.api.Logger
import scala.collection.JavaConversions._
import play.api.libs.json._
import cats.syntax.either._
import com.amazonaws.{ClientConfiguration,Protocol,ProxyAuthenticationMethod}
import com.amazonaws.auth.{AWSStaticCredentialsProvider,BasicAWSCredentials}
import com.amazonaws.services.ecs.{AmazonECSClientBuilder,AmazonECS}
import com.amazonaws.services.elasticloadbalancingv2.{AmazonElasticLoadBalancing,AmazonElasticLoadBalancingClientBuilder}
import com.amazonaws.services.ec2.{AmazonEC2ClientBuilder,AmazonEC2}

object EcsProvider {
  case class Properties(
    access_key: String,
    secret_key: String,
    region: String,
    cluster: String,
    taskRoleArn: Option[String],
    request: Option[RequestConfiguration]
  )

  object HttpOrHttps extends Enumeration {
    val HTTP, HTTPS = Value
  }

  case class RequestConfiguration(
    proxy: Option[ProxyConfiguration],
    protocol: Option[HttpOrHttps.Value]
  )
  
  object ProxyAuthMethod extends Enumeration {
    val BASIC, DIGEST, KERBEROS, NTLM, SPNEGO = Value
  }

  case class ProxyConfiguration(
    protocol: Option[HttpOrHttps.Value],
    authMethods: Option[Seq[ProxyAuthMethod.Value]],
    host: Option[String],
    port: Option[Int],
    password: Option[String],
    username: Option[String],
    ntlmDomain: Option[String],
    ntlmWorkstation: Option[String]
  )

  implicit val httpOrHttpsReads = Reads.enumNameReads(HttpOrHttps)
  implicit val proxyAuthMethodReads = Reads.enumNameReads(ProxyAuthMethod)
  implicit val proxyConfigurationReads = Json.reads[ProxyConfiguration]
  implicit val requestConfigurationReads = Json.reads[RequestConfiguration]
  implicit val propertiesReads = Json.reads[Properties]
}

case class EcsClient(client: AmazonECS, elb: AmazonElasticLoadBalancing, ec2: AmazonEC2, cluster: String, launchType: String, taskRoleArn: Option[String])

trait FromJsResult {
  def fromJsResult[A](jsResult: JsResult[A]): Either[String,A] = {
    jsResult match {
      case JsError(errors) => {
        val errorMessage = errors map { case(path, errors) =>
          val allErrors = errors.map(_.message).mkString(", ")
          s"${path}: $allErrors"
        } mkString("; ")
        Left(s"Failed to parse payload: ${errorMessage}")
      }
      case JsSuccess(value, _) => Right(value)
    }
  }
}

trait EcsClientFactory {
  def getEcsClient(launchType: String, rawProperties: String): Either[String,EcsClient]
}

class DefaultEcsClientFactory extends EcsClientFactory with FromJsResult {
  import EcsProvider._

  def getEcsClient(launchType: String, rawProperties: String): Either[String,EcsClient] = {
    for(
      _ <- if(Seq("EC2", "FARGATE").contains(launchType)) { Right(()) }else {
        Left(s"Invalid launchType `${launchType}`") 
      };
      json <- Right(Json.parse(rawProperties));
      properties <- fromJsResult(json.validate[EcsProvider.Properties])
    ) yield {
      val clientConfiguration = new ClientConfiguration()

      properties.request.flatMap(_.protocol) collect {
        case HttpOrHttps.HTTP => clientConfiguration.setProtocol(Protocol.HTTP)
        case HttpOrHttps.HTTPS => clientConfiguration.setProtocol(Protocol.HTTPS)
      }

      val proxyConfiguration = properties.request.flatMap(_.proxy)
      proxyConfiguration.flatMap(_.protocol) collect {
        case HttpOrHttps.HTTP => clientConfiguration.setProxyProtocol(Protocol.HTTP)
        case HttpOrHttps.HTTPS => clientConfiguration.setProxyProtocol(Protocol.HTTPS)
      }
      proxyConfiguration.flatMap(_.authMethods) foreach { authMethods =>
        val proxyAuthenticationMethods = authMethods collect {
          case ProxyAuthMethod.BASIC => ProxyAuthenticationMethod.BASIC
          case ProxyAuthMethod.DIGEST => ProxyAuthenticationMethod.DIGEST
          case ProxyAuthMethod.KERBEROS => ProxyAuthenticationMethod.KERBEROS
          case ProxyAuthMethod.NTLM => ProxyAuthenticationMethod.NTLM
          case ProxyAuthMethod.SPNEGO => ProxyAuthenticationMethod.SPNEGO
        }
        clientConfiguration.setProxyAuthenticationMethods(proxyAuthenticationMethods)
      }
      proxyConfiguration.flatMap(_.host) foreach { host =>
        clientConfiguration.setProxyHost(host)
      }
      proxyConfiguration.flatMap(_.port) foreach { port =>
        clientConfiguration.setProxyPort(port)
      }
      proxyConfiguration.flatMap(_.password) foreach { password =>
        clientConfiguration.setProxyPassword(password)
      }
      proxyConfiguration.flatMap(_.username) foreach { username =>
        clientConfiguration.setProxyUsername(username)
      }
      proxyConfiguration.flatMap(_.ntlmDomain) foreach { ntlmDomain =>
        clientConfiguration.setProxyDomain(ntlmDomain)
      }
      proxyConfiguration.flatMap(_.ntlmWorkstation) foreach { ntlmWorkstation =>
        clientConfiguration.setProxyWorkstation(ntlmWorkstation)
      }

      val ecsBuilder = AmazonECSClientBuilder.standard()
        .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(properties.access_key, properties.secret_key)))
        .withRegion(properties.region)
        .withClientConfiguration(clientConfiguration)

      val elbBuilder = AmazonElasticLoadBalancingClientBuilder.standard()
        .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(properties.access_key, properties.secret_key)))
        .withRegion(properties.region)
        .withClientConfiguration(clientConfiguration)

      val ec2Builder = AmazonEC2ClientBuilder.standard()
        .withCredentials(new AWSStaticCredentialsProvider(new BasicAWSCredentials(properties.access_key, properties.secret_key)))
        .withRegion(properties.region)
        .withClientConfiguration(clientConfiguration)
      
      EcsClient(ecsBuilder.build(), elbBuilder.build(), ec2Builder.build(), properties.cluster, launchType, properties.taskRoleArn)
    }
  }
}