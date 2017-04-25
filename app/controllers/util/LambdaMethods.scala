package controllers.util


import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.json.Js
import play.api.libs.json.{JsError, JsObject, JsSuccess, Json}
import play.api.libs.ws.WSClient

import scala.concurrent.{Await, Future}
import scala.util.Try
import scala.language.postfixOps

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.uuid2string

import com.galacticfog.gestalt.laser.LaserLambda
import com.galacticfog.gestalt.patch.PatchDocument

import play.api.Logger
import play.api.libs.json.JsValue
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
      case _ => lambda
    }
  }

  private[controllers] def getLambdaProvider(res: GestaltResourceInstance): GestaltResourceInstance = {
    (for {
      ps  <- res.properties
      pr  <- ps.get("provider")
      pid <- Js.find(Json.parse(pr).as[JsObject], "/id")
      prv <- ResourceFactory.findById(ResourceIds.LambdaProvider, UUID.fromString(pid.as[String]))
    } yield prv) getOrElse {
      throw new RuntimeException("Could not parse LambdaProvider ID from API.")
    }
  }

  def patchLambdaHandler(
      r: GestaltResourceInstance,
      patch: PatchDocument,
      user: AuthAccountWithCreds): Try[GestaltResourceInstance] = Try {

    @scala.annotation.tailrec
    def replace(data: Seq[(String,Option[JsValue])], lm: LaserLambda): LaserLambda = {
      data match {
        case Nil => lm
        case h :: t => replace(t, updateLambdaData(lm, h._1, h._2.get))
      }
    }

    log.debug("Finding lambda in backend system...")
    val provider = getLambdaProvider(r)
    val client = providerMethods.configureWebClient(provider, Some(ws))

    // Strip path to last component to get field name.
    val ops = patch.ops map { o =>
      val fieldName = o.path.drop(o.path.lastIndexOf("/")+1)
      (fieldName -> o.value)
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
      _ = log.debug("Lambda found in lambda provider. Performing PUT to update...")
      patchedLaserLambda = replace(ops, gotLaserLambda)
      updatedLaserLambdaReq = client.put(s"/lambdas/${r.id}", Some(Json.toJson(patchedLaserLambda)))
      _ <- updatedLaserLambdaReq flatMap { response => response.status match {
        case 200 =>
          log.info(s"Successfully updated Lambda in lambda provider.")
          Future.successful(response)
        case _   =>
          log.error(s"Error updating Lambda in lambda provider: ${response}")
          Future.failed(new RuntimeException(s"Error updating Lambda in lambda provider: ${response}"))
      }}
      updatedMetaLambda = PatchInstance.applyPatch(r, patch).get.asInstanceOf[GestaltResourceInstance]
    } yield updatedMetaLambda // we don't actually use the result from laser, though we probably should

    Await.result(f, LAMBDA_PROVIDER_TIMEOUT_MS millis)
  }

}