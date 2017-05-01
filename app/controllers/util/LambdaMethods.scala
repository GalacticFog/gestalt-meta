package controllers.util


import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.json.Js
import play.api.libs.json._
import play.api.libs.ws.WSClient

import scala.concurrent.{Await, Future}
import scala.util.Try
import scala.language.postfixOps
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.laser.LaserLambda
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import play.api.Logger
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import javax.inject.Inject

import scala.concurrent.duration._

class LambdaMethods @Inject()( ws: WSClient,
                               providerMethods: ProviderMethods ) {

  val LAMBDA_PROVIDER_TIMEOUT_MS = 5000
  
  private val log = Logger(this.getClass)

  /**
   * Update a field on a LaserLambda. Currently this is limited to what is modifiable in the UI.
   */
  private[controllers] def updateLambdaData(lambda: LaserLambda, fieldName: String, value: JsValue) = {
    val artifact = lambda.artifactDescription
    
    fieldName match {
      case "public"   => lambda.copy(public = value.as[Boolean])
      case "cpus"     => lambda.copy(artifactDescription = artifact.copy(cpus = value.as[Double]))
      case "memory"   => lambda.copy(artifactDescription = artifact.copy(memorySize = value.as[Int]))
      case "timeout"  => lambda.copy(artifactDescription = artifact.copy(timeoutSecs = value.as[Int]))
      case "package_url"  => lambda.copy(artifactDescription = artifact.copy(artifactUri = Option(value.as[String])))
      case "code" => lambda.copy(artifactDescription = artifact.copy(code = Option(value.as[String])))
      case "runtime" => lambda.copy(artifactDescription = artifact.copy(runtime = value.as[String]))
      case "compressed" => lambda.copy(artifactDescription = artifact.copy(compressed = value.as[Boolean]))
      case "handler" => lambda.copy(artifactDescription = artifact.copy(handler = value.as[String]))
      case "periodic_info" => lambda.copy(artifactDescription = artifact.copy(periodicInfo = value.asOpt[JsObject]))
      case _ => lambda
    }
  }

  private[controllers] def getLambdaProvider(res: ResourceLike): GestaltResourceInstance = {
    (for {
      ps  <- res.properties
      pr  <- ps.get("provider")
      pid <- Js.find(Json.parse(pr).as[JsObject], "/id")
      prv <- ResourceFactory.findById(ResourceIds.LambdaProvider, UUID.fromString(pid.as[String]))
    } yield prv) getOrElse {
      throw new RuntimeException("Could not parse LambdaProvider ID from API.")
    }
  }

  def deleteLambdaHandler( r: ResourceLike, user: AuthAccountWithCreds ): Try[Unit] = {
    log.debug("Finding lambda in backend system...")
    val provider = getLambdaProvider(r)
    val client = providerMethods.configureWebClient(provider, Some(ws))

    // TODO: fdelete is never used, and this method returns a Try.success(()) even if fdelete (eventually) isFailure
    val fdelete = client.delete(s"/lambdas/${r.id.toString}") map { result =>
      log.info("Deleting API from Lambda backend...")
      log.debug("Response from Lambda backend: " + result.body)
    } recover {
      case e: Throwable => {
        log.error(s"Error deleting lambda from Lambda backend: " + e.getMessage)
        throw e
      }
    }
    Try {
      Await.result(fdelete, LAMBDA_PROVIDER_TIMEOUT_MS millis)
      ()
    }
  }

  def patchLambdaHandler(
      r: GestaltResourceInstance,
      patch: PatchDocument,
      user: AuthAccountWithCreds): Try[GestaltResourceInstance] = Try {

    def replace(data: Seq[(String,JsValue)], lm: LaserLambda): LaserLambda = {
      data.foldLeft[LaserLambda](lm)({
        case (l, (fieldName,value)) => updateLambdaData(l, fieldName, value)
      })
    }

    log.debug("Finding lambda in backend system...")
    val provider = getLambdaProvider(r)
    val client = providerMethods.configureWebClient(provider, Some(ws))

    // Strip path to last component to get field name.
    val ops: Seq[(String,JsValue)] = patch.ops collect {
      case PatchOp("add"|"replace",path,Some(value)) =>
        val fieldName = path.drop(path.lastIndexOf("/")+1)
        (fieldName -> value)
      case PatchOp("remove","/properties/periodic_info",None) =>
        ("periodic_info" -> JsNull)
    }

    val f = for {
      // Get lambda from gestalt-lambda
      getReq <- client.get(s"/lambdas/${r.id}") flatMap { response => response.status match {
        case 200 => Future.successful(response)
        case 404 => Future.failed(new ResourceNotFoundException(s"No Lambda with ID '${r.id}' was found in gestalt-lambda"))
        case _   => Future.failed(new RuntimeException(s"received $response response from Lambda provider on lambda GET"))
      } }
      gotLaserLambda <- getReq.json.validate[LaserLambda] match {
        case JsSuccess(l, _) => Future.successful(l)
        case e: JsError => Future.failed(new RuntimeException(
          "could not parse lambda GET response from lambda provider: " + e.toString
        ))
      }
      _ = log.debug("Lambda found in lambda provider.")
      patchedLaserLambda = replace(ops, gotLaserLambda)
      _ = log.debug("Patched lambda resource before PUT: " + Json.toJson(patchedLaserLambda))
      updatedLaserLambdaReq = client.put(s"/lambdas/${r.id}", Some(Json.toJson(patchedLaserLambda)))
      _ <- updatedLaserLambdaReq flatMap { response => response.status match {
        case 200 =>
          log.info(s"Successfully PUT Lambda in lambda provider.")
          Future.successful(response)
        case _   =>
          log.error(s"Error PUTting Lambda in lambda provider: ${response}")
          Future.failed(new RuntimeException(s"Error updating Lambda in lambda provider: ${response}"))
      }}
      updatedMetaLambda = PatchInstance.applyPatch(r, patch).get.asInstanceOf[GestaltResourceInstance]
    } yield updatedMetaLambda // we don't actually use the result from laser, though we probably should

    Await.result(f, LAMBDA_PROVIDER_TIMEOUT_MS millis)
  }

}