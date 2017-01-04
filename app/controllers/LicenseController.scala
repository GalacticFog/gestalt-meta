package controllers

import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import com.galacticfog.gestalt.data.ResourceFactory.findById
import com.galacticfog.gestalt.data.ResourceFactory.hardDeleteResource
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.galacticfog.gestalt.keymgr._
import com.galacticfog.gestalt.meta.api.errors.{ConflictException, ResourceNotFoundException}
import controllers.util.{HandleExceptions, NotFoundResult, SecureController, getExpandParam}
import play.api.{Logger => log}
import play.api.libs.json.{JsArray, JsValue, Json}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import javax.inject.Singleton

@Singleton
class LicenseController @Inject()(messagesApi: MessagesApi,
                                  env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

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
    
    log.info(s"INFO - posting a new license in meta.")
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
      // Remove old license - currently there will be only 1 license
      ResourceFactory.findAll(ResourceIds.License, org).headOption match {
        case Some(oldlicense) =>
          hardDeleteResource(oldlicense.asInstanceOf[GestaltResourceInstance].id) match {
            case Failure(e) =>  log.warn(s"Error deleting old license '${oldlicense.id.toString}: ${e.getMessage}'")

            case Success(_) => // Do nothing
          }
        case None =>  // Do nothing
      }
      // Install the license
      try {
        log.info(s"INFO - getting the license string from properties.data in meta.")
        val licenseText = (request.body \ "properties" \ "data").as[String]
        log.info(s"INFO - installing the license with GestaltLicense.")
        if (licenseText == "") {
          GestaltLicense.instance().install()
        } else {
          GestaltLicense.instance().install(licenseText)
        }
        log.info(s"INFO - license installed.  Creating the license resource.")
        createLicense(org) match {
          case Failure(e) => HandleExceptions(e)
          case Success(license) => {

            // Set CRUD entitlements for creating User
            setCreatorEntitlements(license.id, org, request.identity)

            // Render resource to JSON and return 201
            Created(Output.renderInstance(license, META_URL))

          }
        }
      } catch {
        case e: Throwable => HandleExceptions(ConflictException(e.getMessage))
      }
    }
  }
  

  /**
   * GET /{fqon}/licenses
   */
  def getLicenses(fqon: String) = Authenticate(fqon) { implicit request =>
    log.info(s"INFO - verifying license from meta.")
    try {
      GestaltLicense.instance().verify()
      GestaltLicense.instance().view()
    } catch {
      case e: Throwable =>
        try {
          log.info("Trying to install license from meta.")
          postLicense("")
          log.info("Default license installed from meta.")
        } catch {
          case e: Throwable =>
            log.error(s"ERROR - Installing default license from meta: ${e.getMessage}")
        }
    }

    val transform = request.queryString.getOrElse("transform", "true") != "false"
    val expand = getExpandParam(request.queryString)
    try {
      if (expand == true) {
        Ok( JsArray().append(Json.parse(GestaltLicense.instance().view())) )
      } else {
        val org = fqid(fqon)
        val licenses = ResourceFactory.findAll(ResourceIds.License, org)
        Ok(Output.renderLinks(licenses, META_URL))
      }
    } catch {
      case e: Throwable => HandleExceptions(ResourceNotFoundException(e.getMessage))
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
     val transform = request.queryString.getOrElse("transform", "true") != "false"
     if (transform == true) {
        try {
          println(GestaltLicense.instance().view())
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
