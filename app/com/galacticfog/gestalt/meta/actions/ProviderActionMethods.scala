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
class ProviderActionMethods @Inject()(lambdaMethods: LambdaMethods) {
  
  private[this] val log = Logger(this.getClass)
  
  implicit lazy val messageImplementationFormat = Json.format[MessageImplementation]
  implicit lazy val messageEndpointConfigFormat = Json.format[MessageEndpointConfig]
  
  
  /**
   * Create or retrieve the implementation resource from a Provider Action Spec.
   * 
   * Currently only Lambda implementations are supported, but this will extend to other types
   * in the near future (i.e. Container-based implementation)
   */
  
  import com.galacticfog.gestalt.meta.providers.ActionImplSpec
  
  def resolveLambdaImplementation(
      org: UUID, 
      impl: ActionImplSpec, 
      env: GestaltResourceInstance, 
      user: AuthAccountWithCreds) = {
    
    log.info(s"Implementation Type: ${impl.kind} - looking up implementation-resource...")
    
    if (impl.id.nonEmpty && impl.spec.nonEmpty) {
      throw new BadRequestException("You must provide only one of 'implementation.id' or 'implementation.spec'")
    } else if (impl.id.isEmpty && impl.spec.isEmpty) {
      throw new BadRequestException("You must provide either 'implementation.id' or 'implementation.spec'")
    } else if (impl.id.nonEmpty) {
      /*
       * Payload contains an implementation ID, meaning there's an existing implementation-resource.
       * All we do here is look it up and return it if found.
       */
      log.info(s"Found existing implementation resource-ID (${impl.id.get}) - resolving implementation-type...")

      val implId = Try(UUID.fromString(impl.id.get.trim)).getOrElse {
        throw new BadRequestException(s"Invalid implementation.id. Not a valid UUID. Found: ${impl.id.get}")
      }
      
      val target = ResourceFactory.findById(ResourceIds.Lambda, implId) getOrElse {
        throw new BadRequestException(s"Invalid implementation.id. Lambda with ID '$implId' not found.")
      }
      
      log.info("Implementation resource found. Returning.")
      
      Future(target)
      
    } else {
      /*
       * Payload contains a 'spec' definition, meaning we need to create a new implementation-resource.
       */
      val implSpec = impl.spec.get
      
      /*
       * TODO: Use implementation type ID to map a function for creating a new implementation-resource.
       * This just assumes Lambda for now.
       */
      lambdaMethods.createLambdaCommon2(org, env, implSpec, user) map { lam =>
        log.debug(s"Successfully created new ${impl.kind}.")
        lam
      } recover {
        case scala.util.control.NonFatal(e) => {
          log.error(s"Call to create action-resource of type '${impl.kind}' failed.")
          throw e
        }
      } 
    }

  }
  
  def resolveActionImplementation2(
      org: UUID, 
      spec: ProviderActionSpec, 
      env: GestaltResourceInstance, 
      user: AuthAccountWithCreds): Future[GestaltResourceInstance] = {
    
    log.info("Attempting to resolve action-implementation...")
    
    val impl = spec.implementation

    impl.kind.trim.toLowerCase match {
      case "lambda" => resolveLambdaImplementation(org, impl, env, user)
      case "metacallback" => ???
    }
    
    val implTypeId = typeId(impl.kind.trim) getOrElse {
      throw new BadRequestException(s"Invalid implementation.kind. Type with name '${impl.kind}' not found.")
    }
    
    log.info(s"Implementation Type: ${impl.kind} - looking up implementation-resource...")
    
    if (impl.id.nonEmpty && impl.spec.nonEmpty) {
      throw new BadRequestException("You must provide only one of 'implementation.id' or 'implementation.spec'")
    } else if (impl.id.isEmpty && impl.spec.isEmpty) {
      throw new BadRequestException("You must provide either 'implementation.id' or 'implementation.spec'")
    } else if (impl.id.nonEmpty) {
      /*
       * Payload contains an implementation ID, meaning there's an existing implementation-resource.
       * All we do here is look it up and return it if found.
       */
      log.info(s"Found existing implementation resource-ID (${impl.id.get}) - resolving implementation-type...")

      val implId = Try(UUID.fromString(impl.id.get.trim)).getOrElse {
        throw new BadRequestException(s"Invalid implementation.id. Not a valid UUID. Found: ${impl.id.get}")
      }
      
      val target = ResourceFactory.findById(implTypeId, implId) getOrElse {
        throw new BadRequestException(s"Invalid implementation.id. ${impl.kind} with ID '$implId' not found.")
      }
      
      log.info("Implementation resource found. Returning.")
      
      Future(target)
      
    } else {
      /*
       * Payload contains a 'spec' definition, meaning we need to create a new implementation-resource.
       */
      val implSpec = impl.spec.get
      
      /*
       * TODO: Use implementation type ID to map a function for creating a new implementation-resource.
       * This just assumes Lambda for now.
       */
      lambdaMethods.createLambdaCommon2(org, env, implSpec, user) map { lam =>
        log.debug(s"Successfully created new ${impl.kind}.")
        lam
      } recover {
        case scala.util.control.NonFatal(e) => {
          log.error(s"Call to create action-resource of type '${impl.kind}' failed.")
          throw e
        }
      }
      
    }
  }  
  
  def resolveActionImplementation(
      org: UUID, 
      spec: ProviderActionSpec, 
      env: GestaltResourceInstance, 
      user: AuthAccountWithCreds): Future[GestaltResourceInstance] = {
    
    log.info("Attempting to resolve action-implementation...")
    
    val impl = spec.implementation
    val implTypeId = typeId(impl.kind.trim) getOrElse {
      throw new BadRequestException(s"Invalid implementation.kind. Type with name '${impl.kind}' not found.")
    }
    
    log.info(s"Implementation Type: ${impl.kind} - looking up implementation-resource...")
    
    if (impl.id.nonEmpty && impl.spec.nonEmpty) {
      throw new BadRequestException("You must provide only one of 'implementation.id' or 'implementation.spec'")
    } else if (impl.id.isEmpty && impl.spec.isEmpty) {
      throw new BadRequestException("You must provide either 'implementation.id' or 'implementation.spec'")
    } else if (impl.id.nonEmpty) {
      /*
       * Payload contains an implementation ID, meaning there's an existing implementation-resource.
       * All we do here is look it up and return it if found.
       */
      log.info(s"Found existing implementation resource-ID (${impl.id.get}) - resolving implementation-type...")

      val implId = Try(UUID.fromString(impl.id.get.trim)).getOrElse {
        throw new BadRequestException(s"Invalid implementation.id. Not a valid UUID. Found: ${impl.id.get}")
      }
      
      val target = ResourceFactory.findById(implTypeId, implId) getOrElse {
        throw new BadRequestException(s"Invalid implementation.id. ${impl.kind} with ID '$implId' not found.")
      }
      
      log.info("Implementation resource found. Returning.")
      
      Future(target)
      
    } else {
      /*
       * Payload contains a 'spec' definition, meaning we need to create a new implementation-resource.
       */
      val implSpec = impl.spec.get
      
      /*
       * TODO: Use implementation type ID to map a function for creating a new implementation-resource.
       * This just assumes Lambda for now.
       */
      lambdaMethods.createLambdaCommon2(org, env, implSpec, user) map { lam =>
        log.debug(s"Successfully created new ${impl.kind}.")
        lam
      } recover {
        case scala.util.control.NonFatal(e) => {
          log.error(s"Call to create action-resource of type '${impl.kind}' failed.")
          throw e
        }
      }
      
    }
  }
  

  import com.galacticfog.gestalt.data.TypeFactory
  def typeId(name: String): Option[UUID] = {
    TypeFactory.findByName(name) map { _.id }
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