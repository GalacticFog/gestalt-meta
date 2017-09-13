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

    for {
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
  }

  import scala.util.{Try, Success, Failure}
  import com.galacticfog.gestalt.data.ResourceState
  
  def createLambdaCommon2(
    org: UUID, 
    parent: GestaltResourceInstance,
    payload: JsValue,
    caller: AuthAccountWithCreds): Future[GestaltResourceInstance] = {
    
    val input    = safeGetInputJson(payload, Some(ResourceIds.Lambda)).get
    val lambdaId = input.id.getOrElse(UUID.randomUUID)
    
    // Set ID for the Lambda.
    val newjson  = injectParentLink(payload.as[JsObject] ++ Json.obj("id" -> lambdaId.toString), parent)
    val pinfo    = getProviderInfo(newjson)
    val provider = ResourceFactory.findById(ResourceIds.LambdaProvider, UUID.fromString(pinfo.id)) getOrElse {
      unprocessable(s"Lambda Provider with ID '${pinfo.id}' not found.")
    }

    val metaCreate = for {
      metalambda <- CreateResource(org, caller, newjson, ResourceIds.Lambda, Some(parent.id))
      laserlambda = toLaserLambda(input.copy(id = Some(lambdaId)), provider.id.toString)
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
            setNewEntitlements(org, meta.id, caller, Some(parent.id))
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
    
    safeGetInputJson(resourceJson) flatMap { input =>
      val tid = assertValidTypeId(input, Option(typeId))
      ResourceFactory.create(ResourceIds.User, caller.account.id)(
        withInputDefaults(org, input, caller, Option(tid)), parentId) map { res =>
          setNewEntitlements(org, res.id, caller, parentId)
          res
        }
    }
  }
    
  private[this] def unprocessable(message: String) =
    throw new UnprocessableEntityException(message)    
}