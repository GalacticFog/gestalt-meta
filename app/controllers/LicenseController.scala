package controllers

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import com.galacticfog.gestalt.data.ResourceFactory.findById
import com.galacticfog.gestalt.data.ResourceFactory.hardDeleteResource
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.keymgr._
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import controllers.util.HandleExceptions
import controllers.util.NotFoundResult
import play.api.libs.json.{JsValue, Json}


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

          // Install the license
          val licenseText = (request.body \ "properties" \ "data").as[String]
          if (licenseText == "") {
            // TODO - if resource exists, retrieve and install
            GestaltLicense.instance().install()
          } else {
            GestaltLicense.instance().install(licenseText)
          }

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
     val transform = (request.queryString.getOrElse("transform", "true") == "false")
     if (transform) {
        try {
          Ok(Json.parse(GestaltLicense.instance().view()))
        } catch {
          case e: Throwable => HandleExceptions(ResourceNotFoundException(e.getMessage))
        }
     } else {
       findById(ResourceIds.License, licenseId).fold {
       LicenseNotFound(licenseId)
       } {
         license => Ok(Output.renderInstance(license, META_URL))
       }
     }
  }
  
  /**
   * DELETE /{fqon}/licenses/{id}
   */
  def deleteLicense(fqon: String, licenseId: UUID) = Authenticate(fqon) { implicit request =>

    GestaltLicense.instance().uninstall()
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
