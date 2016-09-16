package controllers.util


import java.net.URL

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.laser.Laser
import com.galacticfog.gestalt.laser.LaserLambda
import com.galacticfog.gestalt.meta.api.PatchDocument
import com.galacticfog.gestalt.meta.api.sdk.HostConfig

import controllers.util.db.EnvConfig
import play.api.Logger
import play.api.libs.json.JsValue


object LambdaMethods {
  
  private val log = Logger(this.getClass)
  
  lazy val gatewayConfig = HostConfig.make(new URL(EnvConfig.gatewayUrl))
  lazy val lambdaConfig = HostConfig.make(new URL(EnvConfig.lambdaUrl))
  lazy val laser = new Laser(
      gatewayConfig, lambdaConfig, 
      Option(EnvConfig.securityKey), 
      Option(EnvConfig.securitySecret))
  
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
      case "synchronous"  => lambda.copy(artifactDescription = artifact.copy(synchronous = Option(value.as[Boolean])))
      case "code" => lambda.copy(artifactDescription = artifact.copy(code = Option(value.as[String])))
      case _ => lambda
    }
  }
  
  /**
   * Update a Lambda both in Meta and gestalt-lambda.
   */
  private[controllers] def patchGestaltLambda(r: GestaltResourceInstance, patch: JsValue) = Try {

    @scala.annotation.tailrec
    def doUpdate(data: Seq[(String,JsValue)], lm: LaserLambda): LaserLambda = {
      data match {
        case Nil => lm
        case h :: t => doUpdate(t, updateLambdaData(lm, h._1, h._2))
      }  
    }    
    
    // Get lambda from gestalt-lambda
    val lambda = laser.lambdas(r.id) getOrElse {
      throw new RuntimeException(s"No Lambda with ID '${r.id}' was found in gestalt-lambda")
    }
    
    // Strip path to last component to get field name.
    val pat = PatchDocument.fromJsValue(patch)
    val ops = pat.op map { o => 
      val fieldName = o.path.drop(o.path.lastIndexOf("/")+1)
      (fieldName -> o.value)
    }
    
    // Update the Lambda DAO (in-memory), then in gestalt-lambda   
    val updatedLambda = doUpdate(ops, lambda)
    laser.updateLambda(updatedLambda) match {
      case Failure(e) => {
        log.error(s"Error updating Lambda in gestalt-lambda: " + e.getMessage)
        throw e
      }
      case Success(l) => {
        log.info(s"Successfully updated Lambda in gestalt-lambda.")
      }
    }
  }  
}