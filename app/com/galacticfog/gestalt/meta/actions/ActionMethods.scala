package com.galacticfog.gestalt.meta.actions

import java.util.UUID

import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.Try

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, Resources}
import com.galacticfog.gestalt.meta.providers.ProviderActionSpec
import com.galacticfog.gestalt.meta.providers.ProviderEnv
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.google.inject.Inject

import controllers.util.LambdaMethods
import javax.inject.Singleton
import play.api.Logger
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json


@Singleton
class ActionMethods @Inject()(lambdaMethods: LambdaMethods) {
  
  private[this] val log = Logger(this.getClass)
  
  implicit lazy val messageImplementationFormat = Json.format[MessageImplementation]
  implicit lazy val messageEndpointConfigFormat = Json.format[MessageEndpointConfig]
  
  
  /**
   * Create a Lambda from a Provider Action Spec
   */
  def createActionLambda(
      org: UUID, 
      spec: ProviderActionSpec, 
      env: GestaltResourceInstance, 
      user: AuthAccountWithCreds): Future[GestaltResourceInstance] = {
    
    val impl = spec.implementation
    
    if (impl.id.nonEmpty && impl.spec.nonEmpty) {
      throw new BadRequestException("You must provide only one of 'implementation.id' or 'implementation.spec'")
    } else if (impl.id.isEmpty && impl.spec.isEmpty) {
      throw new BadRequestException("You must provide either 'implementation.id' or 'implementation.spec'")
    } else if (impl.id.nonEmpty) {
      throw new BadRequestException("Use of pre-existing implementation not currently supported.  Please supply 'implementation.spec'")
    } else {
      val lambdaSpec = impl.spec.get
      
      lambdaMethods.createLambdaCommon2(org, env, lambdaSpec, user) map { lam =>
        log.debug("Successfully created Lambda.")
        lam
      } recover {
        case scala.util.control.NonFatal(e) => {
          log.error("Call to create Lambda failed.")
          throw e
        }
      }
    }
  }
  
  def buildActionMessageEndpoint(lambda: GestaltResourceInstance) = {
    /*
     * Lookup Lambda and Message Provider resources
     */
    val (lambdaProvider, messageProvider) = (for {
      lp <- Try { lambdaMethods.findLambdaProvider(lambda) getOrElse {
        throw new RuntimeException(s"Could not find provider for Lambda '${lambda.id}'")
      }}
      mp <- Try { lambdaMethods.findMessageProvider(lp) getOrElse {
        throw new RuntimeException(s"Could not find message-provider for LambdaProvider '${lp.id}'")
      }}
    } yield (lp, mp)).get
    
    extractMessagingVars(lambdaProvider, messageProvider) map { vs =>

      val host  = vs.getOrElse("SERVICE_HOST", rte("RabbitProvider is missing SERVICE_HOST variable."))
      val port  = vs.get("SERVICE_PORT").map(_.toInt)
      val exch  = vs.getOrElse("RABBIT_MONITOR_EXCHANGE", rte("LambdaProvider is missing RABBIT_MONITOR_EXCHANGE variable."))
      val topic = vs.getOrElse("RABBIT_LISTEN_ROUTE", rte("LambdaProvider is missing RABBIT_LISTEN_ROUTE variable."))
      val impl  = MessageImplementation.apply(Resources.Lambda, lambda.id)
      
      MessageEndpointConfig(
          service_host = host,
          service_port = port, 
          message_exchange = exch, 
          message_topic = topic, 
          response_topic = None, 
          implementation = impl)
    }
  }

  /**
   * Collect the environment variables needed to send an Action invocation message to the
   * Lambda service. Looks up environment vars on the Rabbit and Lambda Providers.
   * 
   * @param lambda LambdaProvider resource
   * @param messaging MessageProvider resource (rabbit)
   */
  private def extractMessagingVars(
    lambda: GestaltResourceInstance,
    messaging: GestaltResourceInstance): Try[Map[String, String]] = {

    val targets = Seq("SERVICE_HOST", "SERVICE_PORT", "RABBIT_MONITOR_EXCHANGE", "RABBIT_LISTEN_ROUTE")

    Try {
      (for {
        pe1 <- ProviderEnv.fromResource(lambda) orElse {
          rte(s"Could not parse env vars from LambdaProvider '${lambda.id}'")
        }
        pe2 <- ProviderEnv.fromResource(messaging) orElse {
          rte(s"Could not parse env vars from MessageProvider '${messaging.id}'")
        }
        vars = {
          val a = (pe1.flatten.filter { case (k, v) => k.startsWith("RABBIT_") })
          val b = (pe2.flatten.filter { case (k, v) => k.startsWith("SERVICE_") })
          a ++ b
        }
        out = vars filter { case (k, v) => targets.contains(k) }
      } yield out).get
    }
  }

  /**
   * Find the Lambda associated with (implementing) the given Action.
   */
  def lambdaFromAction(actionId: UUID): Try[GestaltResourceInstance] = Try {
    
    val action = ResourceFactory.findById(ResourceIds.ProviderAction, actionId) getOrElse {
      throw new ResourceNotFoundException(s"ProviderAction with ID '${actionId}' not found.")
    }
    val spec = ProviderActionSpec.fromResource(action)

    // Only handling implementations of type Lambda
    if (spec.implementation.kind != Resources.Lambda) {
      throw new BadRequestException("`implementation.kind` must be of type Lambda.")
    }
    val lid = UUID.fromString {
      spec.implementation.id getOrElse {
        throw new BadRequestException("Missing `implementation.id`.")
      }
    }
    ResourceFactory.findById(ResourceIds.Lambda, lid) getOrElse {
      throw new BadRequestException(s"Lambda with ID '$lid' not found.")
    }    
  }
  
  def rte(message: String) = throw new RuntimeException(message)
}