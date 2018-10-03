package controllers


import java.util.UUID

import scala.util.{Failure, Success, Try}
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import play.api.libs.json.JsObject
import play.api.{Logger => log}
import play.api.libs.json.{JsArray, JsObject, JsValue, Json}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceFactory.{findAll, findById, hardDeleteResource}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment, GestaltSecurityEnvironment}
import com.galacticfog.gestalt.keymgr._
import com.galacticfog.gestalt.meta.api.errors.{BadRequestException, ConflictException, ResourceNotFoundException}
import play.api.libs.json.{JsArray, JsValue, Json}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi
import javax.inject.Singleton
import controllers.util._
import com.galacticfog.gestalt.json.Js
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import play.api.{Logger => log}

@Singleton
class LicenseController @Inject()(
    messagesApi: MessagesApi,
    sec: GestaltFrameworkSecurity)
  extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
  
  /**
   * POST /{fqon}/licenses
   */
  def postLicense(fqon: String) = AsyncAudited(fqon) { implicit request =>
    
    val action = "license.create"
    val org = fqid(fqon)
    
    AuthorizeAsync(org, action) {
      newLicense(org, request.body) match {
        case Failure(error)   => HandleExceptions(error)
        case Success(license) => Created(RenderSingle(license))
      }
    }
  }
  
  /**
   * GET /{fqon}/licenses
   */
  def getLicenses(fqon: String) = Audited(fqon) { implicit request =>
    log.info(s"verifying license from meta.")

    // Verify license.  If not valid, then check for license resources in META.  If exists, install head and re-verify.
    // If META has scaled or migrated to a new container, this will propagate the license

    Try {
      GestaltLicense.instance.verify() // check if license is good, exception if not.
    } match {
      case Failure(e) =>
        ResourceFactory.findAll(ResourceIds.License, fqid(fqon)).headOption match {
          case None          => throw new BadRequestException("No license found.")
          case Some(license) => {
              log.info("Installing license from resource.")
              Try {
                val licstr = license.properties.get("data")
                GestaltLicense.instance.install(licstr)
              } match {
                case Failure(e2) => throw new BadRequestException("Invalid license.")
                case Success(_) => Ok {
                  findAndRenderLicenses(fqid(fqon), request.queryString, request.identity, META_URL)
                }
              }
          }
        }

      case Success(_) => Ok {
        findAndRenderLicenses(fqid(fqon), request.queryString, request.identity, META_URL)
      }
    }
  }
  
  /**
   * GET /{fqon}/licenses/{id}
   */
  def getLicense(fqon: String, license: UUID) = Audited(fqon) { implicit request =>
    
    val action = "license.view"

    findById(ResourceIds.License, license).fold {
      LicenseNotFound(license) 
    }{ lic => Authorize(lic.id, action) {

        if (QueryString.singleBoolean(request.queryString, "transform"))
          Ok(Json.parse(GestaltLicense.instance.view))
        else Ok(RenderSingle(lic))
        
      }
    }
  }
  
  /**
   * DELETE /{fqon}/licenses/{id}
   */
  def deleteLicense(fqon: String, license: UUID) = Audited(fqon) { implicit request =>

    val action = "license.delete"

    Authorize(license, action) {
      Try { GestaltLicense.instance.uninstall() } match { case _ => }
      findById(ResourceIds.License, license).fold {
        LicenseNotFound(license)
      } { _ =>
        hardDeleteResource(license) match {
          case Failure(e) => HandleExceptions(e)
          case Success(_) => NoContent
        }
      }
    }
  }  

  /**
   * Find existing licenses in the given Org and render them to JSON.
   * 
   * Rendering considers the `transform` and `expand` query parameters.
   * 
   * @param org UUID of the Org in which to search for Licenses
   * @param qs The request.queryString from the original Request
   * @param caller Identity of the original requestor
   * @param metaUrl URL of this Meta instance
   */
  private[controllers] def findAndRenderLicenses(
      org: UUID, 
      qs: Map[String, Seq[String]], 
      caller: AuthAccountWithCreds, 
      metaUrl: String): JsValue = {
      
    val action = "license.view"
    
    ResourceFactory.findAll(ResourceIds.License, org).headOption match {
      case None          => Json.arr() // No license in Meta - empty array
      case Some(license) => {

        if(!isAuthorized(license.id, action, caller).get) 
          Json.arr() // not authorized - empty array
        else {
          val ex = QueryString.singleBoolean(qs, "expand")
          val tr = QueryString.singleBoolean(qs, "transform")
          
          if (ex && tr) 
            throw new ConflictException(s"Querystring parameters 'expand' and 'transform' cannot both be TRUE")
          else {
            if (tr) Json.arr(Json.parse(GestaltLicense.instance.view()))           // return raw license data
            else if (ex) Json.arr(Output.renderInstance(license, Option(metaUrl))) // return expanded Meta resource
            else Output.renderLinks(Seq(license), Option(metaUrl))                 // return Meta resource link
          }
        }
      }
    }    
  }

  
  private[controllers] def installNewLicense(js: JsObject): Try[Unit] = Try {

    val licenseText = {
      val t = Js.find(js, "/properties/data") getOrElse {
        log.error(s"Failed parsing license data. found:\n${Json.prettyPrint(js)}")
        throw new BadRequestException(s"could not parse properties.data from license JSON.")
      }
      t.as[String]
    }
    log.info(s"--------\n${licenseText}")
    if (licenseText.trim.nonEmpty) 
      GestaltLicense.instance.install(licenseText)
    else throw new BadRequestException("Invalid license.")
  }
  
  /**
   * Create and install a new License in Meta.
   * 
   * @param org UUID of Org that owns the License
   * @param json JSON body of the new License
   */
  private[controllers] def newLicense(org: UUID, json: JsValue)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]) = {
    log.info(s"Posting new license in Meta.")

    (for {
      _ <- removeExistingLicense(org)
      _ <- installNewLicense(json.as[JsObject])
      r <- CreateWithEntitlements(org, request.identity, json, ResourceIds.License, Some(org))
    } yield r) map { license =>
      license 
    }
  }    
  
  
  /**
   * Remove an existing License from the given Org.
   * 
   * @param org UUID of the Org where the License is to be removed.
   */
  private[controllers] def removeExistingLicense(org: UUID): Try[Unit] = Try {
    val license = ResourceFactory.findAll(ResourceIds.License, org).headOption
    if (license.isDefined) destroyLicense(license.get.id).get
  }
  
  /**
   * Uninstall and delete a license from Meta.
   * 
   * @param license UUID of the License to destroy.
   */
  private[controllers] def destroyLicense(license: UUID): Try[Unit] = Try {
    findById(ResourceIds.License, license).fold { 
      ErrorLicenseNotFound(license) 
    }{ _ =>
      
      Try(GestaltLicense.instance.uninstall()) match {
        case Failure(e) =>
          log.warn(s"Failed uninstalling License '$license': ${e.getMessage}")
        case Success(_) => 
          hardDeleteResource(license) match {
            case Failure(e) => ErrorLicenseDelete(license, e)
            case Success(_) => Unit
          }
      }
    }
  }
  
  
  /**
   * Create a Gestalt::Resource::License
   * 
   * @param org UUID of the Org that owns the License
   * @param request the
   */
  private[controllers] def createLicense(org: UUID)(implicit request: SecuredRequest[GestaltFrameworkSecurityEnvironment,JsValue]) = {
    CreateWithEntitlements(org, request.identity, request.body, ResourceIds.License, Some(org))
  }

  private[this] def LicenseNotFound(licenseId: UUID) = {
    NotFoundResult(s"License with ID '$licenseId' not found.")
  }
  
  private[this] def ErrorLicenseNotFound(license: UUID): Unit = {
    throw new ResourceNotFoundException(s"License with ID '$license' not found.")
  }
  
  private[this] def ErrorLicenseUninstall(license: UUID, e: Throwable): Unit = {
    log.error(s"Failed uninstalling License '$license': ${e.getMessage}")
    throw e
  }
  
  private[this] def ErrorLicenseDelete(license: UUID, e: Throwable): Unit = {
    log.error(s"Failed deleting License '$license': ${e.getMessage}")
    throw e
  }
  
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
}
