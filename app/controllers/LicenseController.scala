package controllers


import play.api.{ Logger => log }
import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.RequestHeader
import play.api.mvc.AnyContent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{ Try, Success, Failure }
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk.{ ResourceLink => MetaLink }

import com.galacticfog.gestalt.meta.services.ResourceQueryService
import com.galacticfog.gestalt.tasks.io.TaskStatus
import com.galacticfog.gestalt.tasks.play.actors.TaskEventMessage
import com.galacticfog.gestalt.tasks.play.io._
import controllers.util._
import controllers.util.db._
import play.mvc.Result
import java.util.UUID
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltBaseAuthProvider
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecuredController
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.{ DummyAuthenticatorService, DummyAuthenticator }
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}

import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.api.json.JsonImports
import play.api.libs.json._
import com.galacticfog.gestalt.security.api.json.JsonImports.{ orgFormat, linkFormat, acctFormat }
import com.mohiva.play.silhouette.api.util.Credentials

import com.galacticfog.gestalt.meta.api.output._ //JsonImports._

import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecuredResource }
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.JsonUtil._
import com.galacticfog.gestalt.laser._

import com.galacticfog.gestalt.meta.api.BuildInfo

  import com.galacticfog.gestalt.data.ResourceFactory.findById
  import com.galacticfog.gestalt.data.ResourceFactory.hardDeleteResource

object LicenseController extends Authorization {
  

  /**
   * POST /{fqon}/licenses
 
     {
        "name":"Example-License-1",
        "description": "Optional description of this License Resource",
        "properties":{
          "data":"ABwwGgQUV92Zy9f2IBZLwfQK1bzpAM0cjdICAgQAuEF47kDd95PMxrw+7Gfhjabg9uUXHNSu+j1wlaqS9ev5V2e2Q8uoQ6YcoDkSE/RqwuOuUdv3bYXKXLwjlw0aLpGlXtFlxuVEjHV4/gUvWIJXOytTXkQG6L1UpisnKfCK5rz/FYLkM2dsGZjOh+50skQIrTSd1a9OWqT7GbQ/meXpDcHv/9C0ah3sdnxr2oEHtZDkJPwjvpoO9Xx3ckvbG7dGXsnnrnFGMyV1AkI6Ny2IPzyhGOJUmOjzmA9IBUoq7fPM/03vcg6yzU5kXcqbieWq8vB/voV3W7ksKqPItNA="
        }
      }
   */
  def postLicense(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    Future {
      
      // Get the Org UUID from its FQON
      val org = fqid(fqon)
      
      /*
       * 
       * If you want to do anything to the input JSON before create, here's the place..
       * 
       * val inputJson = request.body
       * val licenseText = (inputJson \ "properties" \ "data").as[String]
       * 
       */

      createLicense(org) match {
        case Failure(e) => HandleExceptions(e)
        case Success(license) => {
          
          // Set CRUD entitlements for creating User    
          setCreatorEntitlements(license.id, org, request.identity)
          
          // Render resource to JSON and return 201
          Created(Output.renderInstance(license, META_URL))
          
        }
      }
    }
  }
  

  /**
   * GET /{fqon}/licenses/{id}
   */
  def getLicense(fqon: String, licenseId: UUID) = Authenticate(fqon) { implicit request =>
    /*
     * val license = findById(ResourceIds.License, licenseId).get
     * val licenseText = license.properties.get("data")
     */
    
    findById(ResourceIds.License, licenseId).fold {
      LicenseNotFound(licenseId) 
    } { 
      license => Ok(Output.renderInstance(license, META_URL)) 
    }
  }
  
  /**
   * DELETE /{fqon}/licenses/{id}
   */
  def deleteLicense(fqon: String, licenseId: UUID) = Authenticate(fqon) { implicit request =>

    findById(ResourceIds.License, licenseId).fold {
      LicenseNotFound(licenseId)
    } { _ =>
      
      hardDeleteResource(licenseId) match {
        case Failure(e) => HandleExceptions(e)
        case Success(_) => NoContent
      }
      
    }
  }
  
  /**
   * Create a Gestalt::Resource::License
   * 
   * @param org UUID of the Org that owns the License
   * @param request the
   */
  private[controllers] def createLicense(org: UUID)(implicit request: SecuredRequest[JsValue]) = {
    createResourceInstance(org, request.body, Some(ResourceIds.License), Some(org))
  }
  
  /**
   * Set CRUD Entitlements on the License Resource for the creating User.
   * 
   * @param licenseId UUID of the License the Entitlements apply to
   * @param org UUID of the Org that owns the Entitlements
   * @param user User account to create the Entitlements for
   */
  private[controllers] def setCreatorEntitlements(licenseId: UUID, org: UUID, user: AuthAccountWithCreds) = {
    generateEntitlements(
      user.account.id, org, licenseId,
      Seq(ResourceIds.License), ACTIONS_CRUD )    
  }
 
  private[controllers] def LicenseNotFound(licenseId: UUID) = {
    NotFoundResult(s"License with ID '$licenseId' not found.")
  }
}