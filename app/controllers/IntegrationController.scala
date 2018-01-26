package controllers

import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceFactory.findById
import com.galacticfog.gestalt.data.ResourceFactory.hardDeleteResource
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecurityEnvironment
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator

import controllers.util.HandleExceptions
import controllers.util.NotFoundResult
import controllers.util.RequestOptions
import controllers.util.SafeRequest
import controllers.util.SecureController
import controllers.util.standardMethods
import javax.inject.Singleton
import play.api.i18n.MessagesApi
import play.api.libs.json.JsValue

@Singleton
class IntegrationController @Inject()(messagesApi: MessagesApi,
                                      env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  /**
   * POST /{fqon}/environments/{envid}/integrations
 
     {
        "name":"MyLink",
        "description": "Optional description of this Integration Resource",
        "properties":{
           "iconUrl": "",
           "url": "",
           "method": "",
           "headers": [""],
           "payload": {}
        }
      }
   */

  
  def postIntegration(fqon: String, envid: java.util.UUID) = AsyncAudited(fqon) { implicit request =>
    Future {
      
      // Ensure Environment exists and create Integration.
      findById(envid).fold( NotFoundResult(request.uri) ) { env =>
        createIntegrationResult(fqid(fqon), envid, request.body, request.identity, Some(META_URL))
      }
    }
  }
  
  private[controllers] def createIntegrationResult(
      org: UUID,
      parent: UUID,
      inputJson: JsValue, 
      user: AuthAccountWithCreds, 
      baseUri: Option[String])(implicit request: SecuredRequest[JsValue]) = {
    
    val typeId     = ResourceIds.Integration
    val operations = standardMethods(typeId, "integration.create")
    val options    = requestCreateOptions(org, parent, user, inputJson)
    
    /*
     * This performs the 'standard operations' before and after creating the resource.
     * 1.) Authorize against entitlements (integration.create)
     * 2.) Check and evaluate policy
     * 3.) Check for and fire pre-create event
     * 4.) Check for and fire post-create event
     */
    SafeRequest (operations, options) Protect { maybeState =>

      CreateWithEntitlements(org, user, inputJson, typeId, Some(parent)) match {
        case Failure(e) => HandleExceptions(e)
        case Success(integration) => {
          setNewEntitlements(org, integration.id, user, Option(parent))
          Created(Output.renderInstance(integration, Some(META_URL)))
        }
      }
    }
    
  }
  
  
  /**
   * GET /{fqon}/environments/{envid}/integrations
   */
  def getIntegrations(fqon: String, envid: UUID) = Audited(fqon) { implicit request =>
    val transform = false // request.queryString.getOrElse("transform", "true") != "false"
    try {
      val orgid = fqid(fqon)
      val integrations = ResourceFactory.findChildrenOfType(orgid, envid, ResourceIds.Integration)
      Ok(Output.renderLinks(integrations, Some(META_URL)))
    } catch {
      case e: Throwable => HandleExceptions(ResourceNotFoundException(e.getMessage))
    }
  }

  /**
   * GET /{fqon}/environments/{envid}/integration/{id}
   */
  def getIntegration(fqon: String, envid: UUID, integrationId: UUID) = Audited(fqon) { implicit request =>
     val env = findById(ResourceIds.Environment, envid).get
     val integration = findById(ResourceIds.Integration, integrationId).get
     val transform = false // TODO - request.queryString.getOrElse("transform", "true") != "false"
     findById(ResourceIds.Integration, integrationId).fold {
       IntegrationNotFound(integrationId)
     } { integration =>
/*
          if (transform == true) {
  				  // get service account secret/token
  				  val secret = GestaltAccount.generateAPICredentials()
  				  val url = substitute(integration.properties \ "url", env)
  				  val headers = substitute(integration.properties \ "headers", env)
  				  val payload = substitute(integration.properties \ "payload", env)
  				  // substitute env fields into url, headers, payload
          } 
*/
           Ok(Output.renderInstance(integration, Some(META_URL)))
     }
  }

  /**
   * DELETE /{fqon}/environments/{envid}/integrations/{id}
   */
  def deleteIntegration(fqon: String, envid: UUID, id: UUID) = Audited(fqon) { implicit request =>

    findById(ResourceIds.Integration, id).fold {
      IntegrationNotFound(id)
    } { integration =>

      // delete service account
//      GestaltAccount.deleteAccount(integration.accountUuid) match {
//        case Failure(e) => HandleExceptions(e)
//        case Success(_) =>
//      }

      hardDeleteResource(id) match {
        case Failure(e) => HandleExceptions(e)
        case Success(_) => NoContent
      }
    }
  }
  
  /**
   * Create a Gestalt::Resource::Integration
   * 
   * @param org UUID of the Org that owns the Integration
   * @param request the
   */
  private[controllers] def createIntegration(orgid: UUID, envid: UUID)(implicit request: SecuredRequest[JsValue]) = {
    log.info(s"INFO - creating integration resource in meta." + request.body.toString)
    CreateWithEntitlements(orgid, request.identity, request.body, ResourceIds.Integration, Some(envid))
  }
  
  private[controllers] def IntegrationNotFound(integrationId: UUID) = {
    NotFoundResult(s"Integration with ID '$integrationId' not found.")
  }

  def substitute(strfld: String, env: GestaltResourceInstance) = {
    val pattern = "\\Q{{\\E[a-zA-Z]\\Q}}\\E".r
      strfld.replace("{{fqon}}", "testorg.testws").replace("{{name}}", env.name).replace("{{id}}", env.id.toString)
      //TODO - add remaining env properties
      strfld
  }

  /*
   * This is all the data needed to perform policy and auth checks on the new resource.  
   */
  private[this] def requestCreateOptions(
      org: UUID,
      policyOwner: UUID,
      user: AuthAccountWithCreds,
      inputJson: JsValue): RequestOptions = {
    
      RequestOptions(user, 
          authTarget = Option(policyOwner), 
          policyOwner = Option(policyOwner), 
          policyTarget = Option(j2r(org, user, inputJson, Option(ResourceIds.Integration)))/*,
          data = Option(Map("host" -> baseUri.get))*/)
  }
  
}
