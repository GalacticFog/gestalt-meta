package controllers.util


import java.net.URL
import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.laser
import play.api.libs.json.{JsObject, Json}

import scala.util.{Failure, Success, Try}

//import com.galacticfog.gestalt.meta.api.{PatchDocument => OldPatchDoc}

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.uuid2string

import com.galacticfog.gestalt.laser.LaserLambda
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.meta.api.sdk.HostConfig

import controllers.util.db.EnvConfig
import play.api.Logger
import play.api.libs.json.JsValue
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
//import com.galacticfog.gestalt.meta.api.sdk.JsonClient
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.meta.providers._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import javax.inject.Inject

class LambdaMethods @Inject()() {
  
  private val log = Logger(this.getClass)

//  lazy val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
//  lazy val lambdaConfig = HostConfig.make(new URL(EnvConfig.lambdaUrl))
//  lazy val laser = new Laser(
//      gatewayConfig, lambdaConfig, 
//      Option(EnvConfig.securityKey), 
//      Option(EnvConfig.securitySecret))
  
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
      case "synchronous"  => lambda.copy(artifactDescription = artifact.copy(synchronous = value.as[Boolean]))
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
    val client = ProviderMethods.configureWebClient(provider, hostVariable, Some(ws))
    // Get lambda from gestalt-lambda
    val lambda: LaserLambda = ??? // GET LaserLambda from lambda provider
//   laser.lambdas(r.id) getOrElse {
//      throw new ResourceNotFoundException(s"No Lambda with ID '${r.id}' was found in gestalt-lambda")
//    }

    // Strip path to last component to get field name.
    val ops = patch.ops map { o =>
      val fieldName = o.path.drop(o.path.lastIndexOf("/")+1)
      (fieldName -> o.value)
    }
    log.debug("Lambda found. Performing PATCH...")
    // Update the Lambda DAO (in-memory), then in gestalt-lambda
    val patchedLambda = replace(ops, lambda)
    val updatedLambda = ??? // ask provider to update lambda
//    laser.updateLambda(updatedLambda) match {
//      case Failure(e) => {
//        log.error(s"Error updating Lambda in gestalt-lambda: " + e.getMessage)
//        throw e
//      }
//      case Success(l) => {
//        log.info(s"Successfully updated Lambda in gestalt-lambda.")
//      }
//    }
    // TODO: why aren't we using updatedLambda from the line above?
    PatchInstance.applyPatch(r, patch).get.asInstanceOf[GestaltResourceInstance]
  }

  /**
   * Update a Lambda both in Meta and gestalt-lambda.
   */
//  def patchGestaltLambda(r: GestaltResourceInstance, patch: JsValue) = Try {
//
//    @scala.annotation.tailrec
//    def doUpdate(data: Seq[(String,JsValue)], lm: LaserLambda): LaserLambda = {
//      data match {
//        case Nil => lm
//        case h :: t => doUpdate(t, updateLambdaData(lm, h._1, h._2))
//      }  
//    }    
//    
//    // Get lambda from gestalt-lambda
//    val lambda = laser.lambdas(r.id) getOrElse {
//      throw new RuntimeException(s"No Lambda with ID '${r.id}' was found in gestalt-lambda")
//    }
//    
//    // Strip path to last component to get field name.
//    val pat = OldPatchDoc.fromJsValue(patch)
//    val ops = pat.op map { o => 
//      val fieldName = o.path.drop(o.path.lastIndexOf("/")+1)
//      (fieldName -> o.value)
//    }
//    
//    // Update the Lambda DAO (in-memory), then in gestalt-lambda   
//    val updatedLambda = doUpdate(ops, lambda)
//    laser.updateLambda(updatedLambda) match {
//      case Failure(e) => {
//        log.error(s"Error updating Lambda in gestalt-lambda: " + e.getMessage)
//        throw e
//      }
//      case Success(l) => {
//        log.info(s"Successfully updated Lambda in gestalt-lambda.")
//      }
//    }
//  }  
}