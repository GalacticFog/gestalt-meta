package com.galacticfog.gestalt.meta.providers.faas

import java.util.Base64
import java.nio.charset.StandardCharsets
import java.util.zip.{ZipOutputStream,ZipEntry}
import java.io.ByteArrayOutputStream
import scala.concurrent.Future
import javax.inject.Inject
import play.api.libs.json._
import play.api.libs.ws._
import play.api.Logger
import cats.syntax.either._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.util.FutureFromTryST._
import com.galacticfog.gestalt.util.Either._
import controllers.util.{ProviderMethods, unstringmap, stringmap}

class AWSLambdaProvider @Inject()(ws: WSClient, providerMethods: ProviderMethods) extends FaasProviderImplementation[Future] {

  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  import LambdaSpec.Implicits._

  val log = Logger(this.getClass)

  case class AWSLambdaConfiguration(
    description: Option[String],
    handler: String,
    runtime: String,
    timeout: Int,
    memorySize: Int,
    role: String
  )

  case class AWSLambdaResponse(
    arn: String,
    config: AWSLambdaConfiguration
  )

  implicit val formatAWSLambdaConfiguration = Json.format[AWSLambdaConfiguration]
  implicit val formatAWSLambdaResponse = Json.format[AWSLambdaResponse]

  private def mkRequest(provider: GestaltResourceInstance): Either[String,String => WSRequest] = {
    providerMethods.getHostConfig(provider) map { hc =>
      path => {
        val url = s"""${hc.protocol}://${hc.host}:${hc.port.getOrElse("80")}/${path.stripPrefix("/")}"""
        log.debug(s"request to $url with ${hc.creds}")
        val request = ws.url(url)
        hc.creds.foldLeft(request) { case(r, creds) => creds.addHeader(r) }
      }
    }
  }

  private def deserializeResource[A: Reads](resource: GestaltResourceInstance): Either[String,A] = {
    for(
      rawProperties <- Either.fromOption(resource.properties, s"Could not parse resource ${resource.id} with unset properties");
      rawJsonProperties = unstringmap(Some(rawProperties)).get;
      properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[A])
    ) yield properties
  }

  private def serializeResource[A: Writes](resource: GestaltResourceInstance, properties: A): Either[String,GestaltResourceInstance] = {
    for(
      serializedProperties <- Either.fromOption(stringmap(Json.toJson(properties).asOpt[Map[String,JsValue]]),
       s"Failed to serialize resource properties: $properties")
    ) yield {
      resource.copy(properties=Some(resource.properties.getOrElse(Map()) ++ serializedProperties))
    }
  }

  private def checkResponseStatus(response: WSResponse): Future[WSResponse] = {
    if(200 >= response.status && response.status < 300) {
      Future.successful(response)
    }else {
      Future.failed(ApiError(response.status, response.body).throwable)
    }
  }

  private def mkConfig(lambda: AWSLambdaProperties, lambdaProvider: LambdaProviderProperties): Either[String,JsValue] = {
    for(
      handler <- lambda.code_type match {
        case "Package" => Right(lambda.handler)
        case "Inline" if lambda.runtime.startsWith("nodejs") => Right(s"index.${lambda.handler}")
        case _ => Left(s"${lambda.code_type} code type not supported with ${lambda.runtime} runtime")
      }
    ) yield {
      val role = lambda.aws_role_id.getOrElse("auto")
      Json.obj(
        "description" -> "Managed by Gestalt Platform",
        "handler" -> handler,
        "runtime" -> lambda.runtime,
        "timeout" -> lambda.timeout,
        "memorySize" -> lambda.memory,
        "role" -> role
      )
    }
  }

  private def mkCode(lambda: AWSLambdaProperties, lambdaProvider: LambdaProviderProperties): Future[String] = {
    for(
      code <- (lambda.code_type, lambda.code, lambda.package_url) match {
        case ("Package", None, Some(url)) => {
          for(
            response <- ws.url(url).get();
            _ <- checkResponseStatus(response)
          ) yield response.bodyAsBytes.toArray
        }
        case ("Inline", Some(code), None) => Future.successful {
          val stream = new ByteArrayOutputStream()
          val out = new ZipOutputStream(stream)
          val entry = new ZipEntry("index.js")
          out.putNextEntry(entry)
          val codeByteArray = code.getBytes()
          out.write(codeByteArray, 0, codeByteArray.length)
          out.closeEntry()

          out.close()
          stream.toByteArray()
        }
        case _ => Future.failed(new RuntimeException("Invalid code settings"))
      }
    ) yield new String(Base64.getEncoder().encode(code), StandardCharsets.UTF_8)
  }

  def createLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    for(
      req <- mkRequest(provider).liftTo[Future];
      lambda <- deserializeResource[AWSLambdaProperties](resource).liftTo[Future];
      lambdaProvider <- deserializeResource[LambdaProviderProperties](provider).liftTo[Future];
      config <- mkConfig(lambda, lambdaProvider).liftTo[Future];
      code <- mkCode(lambda, lambdaProvider);
      response <- req("/function/create").post(Json.obj(
        "name" -> s"${resource.id}",
        "config" -> config,
        "publish" -> lambda.public,
        "code" -> code 
      ));
      _ <- checkResponseStatus(response);
      updatedResource <- (for(
        response <- eitherFromJsResult(response.json.validate[AWSLambdaResponse]);
        handler = lambda.code_type match {
          case "Package" => response.config.handler
          case "Inline" => response.config.handler.stripPrefix(s"index.")
          case _ => ???
        };
        updatedLambda = lambda.copy(
          handler=handler,
          runtime=response.config.runtime,
          timeout=response.config.timeout,
          memory=response.config.memorySize,
          aws_role_id=Some(response.config.role),
          aws_function_id=Some(response.arn)
        );
        r <- serializeResource[AWSLambdaProperties](resource, updatedLambda)
      ) yield r).liftTo[Future]
    ) yield updatedResource
  }

  def updateLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance, patch: PatchDocument): Future[GestaltResourceInstance] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))

    for(
      req <- mkRequest(provider).liftTo[Future];
      patched <- Future.fromTryST(PatchInstance.applyPatch(resource, patch));
      lambda <- deserializeResource[AWSLambdaProperties](patched).liftTo[Future];
      lambdaProvider <- deserializeResource[LambdaProviderProperties](provider).liftTo[Future];
      modifiesCode = patch exists { op => op.path == "/properties/code" };
      modifiesConfig = patch exists { op =>
        Seq("/properties/handler", "/properties/runtime", "/properties/timeout", "/properties/memory") contains op.path
      };
      _ <- if(modifiesCode) {
        for(
          code <- mkCode(lambda, lambdaProvider);
          response <- req(s"/function/${resource.id}/code").put(code);
          _ <- checkResponseStatus(response)
        ) yield ()
      }else {
        Future.successful(())
      };
      updatedResource <- if(modifiesConfig) {
        for(
          config <- mkConfig(lambda, lambdaProvider).liftTo[Future];
          response <- req(s"/function/${resource.id}/configuration").put(config);
          _ <- checkResponseStatus(response);
          updatedResource <- (for(
            response <- eitherFromJsResult(response.json.validate[AWSLambdaResponse]);
            handler = lambda.code_type match {
              case "Package" => response.config.handler
              case "Inline" => response.config.handler.stripPrefix(s"index.")
              case _ => ???
            };
            updatedLambda = lambda.copy(
              handler=handler,
              runtime=response.config.runtime,
              timeout=response.config.timeout,
              memory=response.config.memorySize,
              aws_role_id=Some(response.config.role)
            );
            r <- serializeResource[AWSLambdaProperties](resource, updatedLambda)
          ) yield r).liftTo[Future]
        ) yield updatedResource
      }else {
        Future.successful(patched)
      }
    ) yield updatedResource
  }

  def deleteLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[Unit] = {
    for(
      req <- mkRequest(provider).liftTo[Future];
      response <- req(s"/function/${resource.id}").delete();
      _ <- checkResponseStatus(response)
    ) yield ()
  }
}