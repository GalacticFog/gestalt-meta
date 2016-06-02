package controllers

import java.util.UUID
import java.net.URL
import play.api.http.HttpVerbs
import play.api.libs.ws.WS
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{ PatchOp, PatchDocument, PatchHandler }
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db._
import controllers.util.MetaController
import controllers.util.Security
import play.api.{ Logger => log }
import play.api.libs.json._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.stringmap
import controllers.util.trace
import com.galacticfog.gestalt.meta.api._
import play.api.mvc.Result
import com.galacticfog.gestalt.laser._
import  com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat


object AuthorizationController extends MetaController with NonLoggingTaskEvents {
  
  
  def postEntitlement(fqon: UUID, typeId: UUID, resourceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    Future {
      val user = request.identity
      ResourceFactory.findById(typeId, resourceId) match {
        case None => NotFoundResult(request.uri)
        case Some(_) => {
          CreateResource(
            ResourceIds.User, user.account.id, 
            fqid(fqon), request.body, user, 
            typeId = Some(ResourceIds.Entitlement), 
            parentId = Some(resourceId)) match {
              case Success(res) => Ok(Output.renderInstance(res, META_URL))
              case Failure(err) => HandleExceptions(err)
          }
        }
      }
    }
  }
  
  def deleteEntitlement(fqon: UUID, typeId: UUID, resourceId: UUID) = Authenticate(fqon) { implicit request =>
    ???
  }
  
  def patchEntitlement(fqon: UUID, typeId: UUID, resourceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  def getEntitlementsFqon(fqon: UUID, typeId: UUID, resourceId: UUID) = Authenticate(fqon) { implicit request =>
    ???  
  }
  
  def getEntitlementFqon(fqon: UUID, typeId: UUID, resourceId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    ???  
  }  
  
  
  
}

