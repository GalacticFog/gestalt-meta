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

import play.api.mvc.RequestHeader

import scala.concurrent.duration._
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods

class LambdaMethods @Inject()( ws: WSClient,
                               providerMethods: ProviderMethods ) extends AuthorizationMethods {

  val LAMBDA_PROVIDER_TIMEOUT_MS = 5000
  
  override val log = Logger(this.getClass)

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

  def getLambdaStreams(streamSpec: GestaltResourceInstance, user: AuthAccountWithCreds) = {
    /*
     * Get lambda from stream to get provider.
     */
    val provider = {
      val processor = Json.parse(streamSpec.properties.get("processor"))
      val lambdaId = Js.find(processor.as[JsObject], "/lambdaId") getOrElse {
        throw new RuntimeException("Could not find lambdaId in StreamSpec properties.")
      }
      log.debug("Found Lambda ID : " + lambdaId)
      ResourceFactory.findById(ResourceIds.Lambda, UUID.fromString(lambdaId.as[String])).fold {
        throw new ResourceNotFoundException(s"Lambda with ID '${lambdaId}' not found.")
      }{ lam =>
        getLambdaProvider(lam)
      }
    }

    val client = providerMethods.configureWebClient(provider, Some(ws))
    val streamUrl = s"/streamDefinitions/${streamSpec.id.toString}/streams"
    log.debug("Stream URL : " + streamUrl)
    
    val f = client.get(streamUrl) map { result =>
      result.body
    } recover {
      case e: Throwable => {
        log.error(s"Error looking up streams: " + e.getMessage)
        throw e
      }      
    }
    
    import scala.util.{Success,Failure}
      
    Try {
      Await.result(f, LAMBDA_PROVIDER_TIMEOUT_MS millis)
    }.map { Json.parse(_) }
  }
  
  
  
  def deleteLambdaHandler( r: ResourceLike, user: AuthAccountWithCreds ): Try[Unit] = {
    log.debug("Finding lambda in backend system...")
    val provider = getLambdaProvider(r)
    val client = providerMethods.configureWebClient(provider, Some(ws))

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

  def patchLambdaHandler( r: GestaltResourceInstance,
                          patch: PatchDocument,
                          user: AuthAccountWithCreds,
                          request: RequestHeader ): Future[GestaltResourceInstance] = {


    log.debug("Finding lambda in backend system...")
    val provider = getLambdaProvider(r)
    val client = providerMethods.configureWebClient(provider, Some(ws))

    for {
      // Get lambda from gestalt-lambda
      updatedLambda <- Future.fromTry{PatchInstance.applyPatch(r, patch).map(_.asInstanceOf[GestaltResourceInstance])}
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
      patchedLaserLambda = toLaserLambda(updatedLambda, provider.id.toString)
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
  }

  import scala.util.{Try, Success, Failure}
  import com.galacticfog.gestalt.data.ResourceState
  
  def createLambdaCommon2(
    org: UUID, 
    parent: GestaltResourceInstance,
    payload: JsValue,
    caller: AuthAccountWithCreds): Future[GestaltResourceInstance] = {
    
    val input    = toInput(payload, Some(ResourceIds.Lambda)).get
    val lambdaId = input.id.getOrElse(UUID.randomUUID)
    
    // Set ID for the Lambda.
    val newjson  = injectParentLink(payload.as[JsObject] ++ Json.obj("id" -> lambdaId.toString), parent)
    val pinfo    = getProviderInfo(newjson)
    val provider = ResourceFactory.findById(ResourceIds.LambdaProvider, UUID.fromString(pinfo.id)) getOrElse {
      unprocessable(s"Lambda Provider with ID '${pinfo.id}' not found.")
    }

    val metaCreate = for {
      metalambda <- CreateResource(org, caller, newjson, ResourceIds.Lambda, Some(parent.id))
      laserlambda = toLaserLambda(metalambda, provider.id.toString)
    } yield (metalambda, laserlambda)
    
    metaCreate match {
      case Failure(e) => {
        log.error("Failed to create Lambda in Meta: " + e.getMessage)
        Future(throw e)
      }
      case Success((meta,laser)) => {
    
        val client = providerMethods.configureWebClient(provider, Some(ws))
        
        log.debug("Creating lambda in Laser...")
        client.post("/lambdas", Option(Json.toJson(laser))) map { result =>

          if (Seq(200, 201).contains(result.status)) {
            log.info("Successfully created Lambda in backend system.")
            setNewResourceEntitlements(org, meta.id, caller, Some(parent.id))
            meta
          } else {
            log.error("Error creating Lambda in backend system.")
            updateFailedBackendCreateResource(caller, meta, ApiError(result.status, result.body).throwable).get
          }
          
        } recover {
          case e: Throwable => {
           log.error(s"Error creating Lambda in backend system.")
           updateFailedBackendCreateResource(caller, meta, e).get
          }
        }
      }
    }
  }
  
  import com.galacticfog.gestalt.meta.api.output._
  import controllers.LambdaProviderInfo
  import com.galacticfog.gestalt.data.parseUUID
  
  def updateFailedBackendCreateResource(caller: AuthAccountWithCreds, metaResource: GestaltResourceInstance, ex: Throwable)
      : Try[GestaltResourceInstance]= {
    log.error(s"Setting state of resource '${metaResource.id}' to FAILED")
    val failstate = ResourceState.id(ResourceStates.Failed)
    ResourceFactory.update(metaResource.copy(state = failstate), caller.account.id)
  }
  
  def injectParentLink(json: JsObject, parent: GestaltResourceInstance) = {
    val parentLink = toLink(parent, None)
    json ++ Json.obj("properties" -> 
      JsonUtil.replaceJsonPropValue(json, "parent", Json.toJson(parentLink)))
  }
  
  def getProviderInfo(lambdaJson: JsValue): LambdaProviderInfo = {
    Js.find(lambdaJson.as[JsObject], "/properties/provider/id").fold {
      unprocessable("Could not find value for [lambda.properties.provider.id]")
    }{ pid =>
      parseUUID(pid.as[String].trim).fold {
        unprocessable(s"[lambda.properties.provider.id] is not a valid UUID. found: '$pid'")
      }{ uid =>
        ResourceFactory.findById(UUID.fromString(uid)).fold {
          unprocessable(s"[lambda.properties.provider.id] '$uid' not found.")
        }{ p =>
          val pjson = Js.find(lambdaJson.as[JsObject], "/properties/provider").get
          Js.parse[LambdaProviderInfo](pjson.as[JsObject]) match {
            case Success(result) => result
            case Failure(e) =>
              unprocessable(s"Could not parse [lambda.properties.provider]. found: ${e.getMessage}'")
          }
        }
      }
    }
  }
  
  /**
   * Find the LambdaProvider backing the given Lambda
   */
  def findLambdaProvider(lambda: GestaltResourceInstance): Option[GestaltResourceInstance] = {
    val pid = (for {
      
      props <- lambda.properties
      json  <- props.get("provider")
      id1   <- Js.find(Json.parse(json).as[JsObject], "/id")
      id2 = UUID.fromString(id1.as[String])
      
    } yield id2) getOrElse {
      throw new RuntimeException("`lambda.properties.provider.id` not found. This is a bug.")
    }
    ResourceFactory.findById(ResourceIds.LambdaProvider, pid)
  }  
  
import com.galacticfog.gestalt.meta.providers._  
  
  /**
   * Find the Message (rabbit) provider linked to the given LambdaProvider
   */
  def findMessageProvider(lambdaProvider: GestaltResourceInstance): Option[GestaltResourceInstance] = {
    val messageProvider = {
      val json = Json.toJson(lambdaProvider).as[JsObject]
      val lps = Js.find(json, "/properties/linked_providers") map { lp => 
        LinkedProvider.fromJson(Json.parse(lp.as[String]).as[JsArray]) 
      }
      lps flatMap { _ find { _.name == "RABBIT" } }
    } getOrElse {
      throw new RuntimeException(s"No MessageProvider configured for LambdaProvider '{lambdaProvider.id}'. Cannot invoke action.")
    }
    ResourceFactory.findById(messageProvider.id)
  }  
  
  protected[controllers] def CreateResource(
    org: UUID,
    caller: AuthAccountWithCreds,
    resourceJson: JsValue,
    typeId: UUID,
    parentId: Option[UUID]): Try[GestaltResourceInstance] = {
    
    toInput(resourceJson) flatMap { input =>
      val tid = assertValidTypeId(input, Option(typeId))
      ResourceFactory.create(ResourceIds.User, caller.account.id)(
        resourceWithDefaults(org, input, caller, Option(tid)), parentId) map { res =>
          setNewResourceEntitlements(org, res.id, caller, parentId)
          res
        }
    }
  }
    
  private[this] def unprocessable(message: String) =
    throw new UnprocessableEntityException(message)    
}