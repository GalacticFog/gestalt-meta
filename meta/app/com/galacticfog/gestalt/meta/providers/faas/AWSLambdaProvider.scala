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
import com.galacticfog.gestalt.util.ResourceSerde
import controllers.util.ProviderMethods

class AWSLambdaProvider @Inject()(ws: WSClient, providerMethods: ProviderMethods) extends FaasProviderImplementation[Future] {

  import play.api.libs.concurrent.Execution.Implicits.defaultContext
  import LambdaSpec.Implicits._

  val log = Logger(this.getClass)

  case class AWSLambdaConfiguration(
    description: Option[String],
    handler: String,
    runtime: String,
    timeout: Int,
    environment: Map[String,String],
    memorySize: Int,
    role: String
  )

  case class AWSLambdaResponse(
    arn: String,
    config: AWSLambdaConfiguration
  )

  case class AWSLambdaWithCodeResponse(
    function: AWSLambdaResponse,
    codeLocation: String
  )

  implicit val formatAWSLambdaConfiguration = Json.format[AWSLambdaConfiguration]
  implicit val formatAWSLambdaResponse = Json.format[AWSLambdaResponse]
  implicit val formatAWSLambdaWithCodeResponse = Json.format[AWSLambdaWithCodeResponse]

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

  private def checkResponseStatus(response: WSResponse): Future[WSResponse] = {
    if(200 >= response.status && response.status < 300) {
      Future.successful(response)
    }else {
      Future.failed(throw ApiError(response.status, response.body).throwable)
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
        "environment" -> JsObject(lambda.env.getOrElse(Map()).toSeq map { ab => (ab._1, JsString(ab._2)) }),
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
      lambda <- ResourceSerde.deserialize[AWSLambdaProperties](resource).liftTo[Future];
      lambdaProvider <- ResourceSerde.deserialize[LambdaProviderProperties](provider).liftTo[Future];
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
        r <- ResourceSerde.serialize[AWSLambdaProperties](resource, updatedLambda)
      ) yield r).liftTo[Future]
    ) yield updatedResource
  }

  def importLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    for(
      req <- mkRequest(provider).liftTo[Future];
      lambdaProperties <- ResourceSerde.deserialize[AWSLambdaProperties](resource).liftTo[Future];
      // because ResourceSerde.serialize doesn't know how to remove fields:
      _ <- (if(lambdaProperties.code == None) { Right(()) }else { Left("code field must be set to None") }).liftTo[Future];
      functionArn <- Either.fromOption(lambdaProperties.aws_function_id, "aws_function_id must be set").liftTo[Future];
      response <- req(s"/function/${functionArn}").get();
      _ <- checkResponseStatus(response);
      importedResource <- (for(
        response <- eitherFromJsResult(response.json.validate[AWSLambdaWithCodeResponse]);
        importedLambdaProperties = lambdaProperties.copy(
          handler=response.function.config.handler,
          runtime=response.function.config.runtime,
          timeout=response.function.config.timeout,
          memory=response.function.config.memorySize,
          aws_role_id=Some(response.function.config.role),
          aws_function_id=Some(response.function.arn),
          code_type="Package",
          package_url=Some(response.codeLocation),
          code=None
        );
        r <- ResourceSerde.serialize[AWSLambdaProperties](resource, importedLambdaProperties)
      ) yield r).liftTo[Future]
    ) yield importedResource
  }

  def updateLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance, patch: PatchDocument): Future[GestaltResourceInstance] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))

    for(
      req <- mkRequest(provider).liftTo[Future];
      patched <- Future.fromTryST(PatchInstance.applyPatch(resource, patch));
      lambda <- ResourceSerde.deserialize[AWSLambdaProperties](patched).liftTo[Future];
      lambdaProvider <- ResourceSerde.deserialize[LambdaProviderProperties](provider).liftTo[Future];
      modifiesCode = patch exists { op => op.path == "/properties/code" };
      modifiesConfig = patch exists { op =>
        val a = Seq("/properties/handler", "/properties/runtime", "/properties/timeout", "/properties/memory") contains op.path
        val b = op.path.startsWith("/properties/env")
        a || b
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
            r <- ResourceSerde.serialize[AWSLambdaProperties](resource, updatedLambda)
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