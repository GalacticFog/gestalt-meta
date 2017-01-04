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
import controllers.util._
import play.api.{Logger => log}
import play.api.libs.json.{JsArray, JsValue, Json}
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.meta.auth.{Actions, Authorization}
import com.galacticfog.gestalt.security.api._
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import scala.reflect.io.Directory

import javax.inject.Singleton

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
  def postIntegration(fqon: String, envid: java.util.UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    log.info(s"INFO - posting a new integration in meta.")
    Future {
      
      // Get the Env UUID from its FQON
     
      // Create service account before creating the integration
      try {
        val orgid = fqid(fqon)
        val user =  request.identity  //GestaltAccount.
//        val org = GestaltOrg.findOr(ResourceIds.Org, orgid).get
        /*
        log.info(s"INFO - (TODO) Creating the service user account.")
        val username = "integration" //TODO - unique username generator
        val firstname = "gestalt"
        val lastname = "integration"
        val email = username + "@gestaltintegration"
        val phone = ""
        val credential = "password" //TODO - generated password
        val groups = Seq.empty[GestaltGroup]
        val description = "A gestalt integration account"
        GestaltDirectory.createAccount(dirid, GestaltAccountCreate(username, firstname, lastname, None, None, credential, None, Some(description))) match {
           case Failure(e) => HandleExceptions(e)
           case Success(account) => {
             log.info(s"INFO - Creating the integration resource.")        
	          createIntegration(env) match {
	            case Failure(e) => HandleExceptions(e)
	            case Success(integration) => {
	              // Set CRUD entitlements for creating User
//	              setCreatorEntitlements(integration.id, org, request.identity)
	              // Render resource to JSON and return 201
	              Created(Output.renderInstance(integration, META_URL))
	            }
	          }
           }
        }
        */
//        val integrationCreds = request.identity.account.generateAPICredentials()
        createIntegration(orgid, envid) match {
          case Failure(e) => HandleExceptions(e)
          case Success(integration) => {
            // Set CRUD entitlements for creating User
//            setCreatorEntitlements(integration.id, envid, request.identity)
            // Render resource to JSON and return 201
            Created(Output.renderInstance(integration, META_URL))
          }
        }
      } catch {
        case e: Throwable => HandleExceptions(ConflictException(e.getMessage))
      }
    }
  }
  
  def postIntegration2(fqon: String, envid: java.util.UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    Future {
      
      // Ensure Environment exists and create Integration.
      findById(envid).fold( NotFoundResult(request.uri) ) { env =>
        createIntegrationResult(fqid(fqon), envid, request.body, request.identity, META_URL)
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
      
      createResourceInstance(org, inputJson, Some(typeId), Some(parent)) match {
        case Failure(e) => HandleExceptions(e)
        case Success(integration) => {
          setNewIntegrationEntitlements(org, integration.id, user, parent)
          Created(Output.renderInstance(integration, META_URL))
        }
      }
    }
    
  }
  
  
  /**
   * GET /{fqon}/environments/{envid}/integrations
   */
  def getIntegrations(fqon: String, envid: UUID) = Authenticate(fqon) { implicit request =>
    val transform = false // request.queryString.getOrElse("transform", "true") != "false"
    try {
      val orgid = fqid(fqon)
      val integrations = ResourceFactory.findChildrenOfType(orgid, envid, ResourceIds.Integration)
      Ok(Output.renderLinks(integrations, META_URL))
    } catch {
      case e: Throwable => HandleExceptions(ResourceNotFoundException(e.getMessage))
    }
  }

  /**
   * GET /{fqon}/environments/{envid}/integration/{id}
   */
  def getIntegration(fqon: String, envid: UUID, integrationId: UUID) = Authenticate(fqon) { implicit request =>
     val env = findById(ResourceIds.Environment, envid).get
     val integration = findById(ResourceIds.Integration, integrationId).get
     val transform = false // TODO - request.queryString.getOrElse("transform", "true") != "false"
     findById(ResourceIds.Integration, integrationId).fold {
       IntegrationNotFound(integrationId)
     } {
       integration =>
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
           Ok(Output.renderInstance(integration, META_URL))
     }
  }

  /**
   * DELETE /{fqon}/environments/{envid}/integrations/{id}
   */
  def deleteIntegration(fqon: String, envid: UUID, id: UUID) = Authenticate(fqon) { implicit request =>

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
    createResourceInstance(orgid, request.body, Some(ResourceIds.Integration), Some(envid))
  }
  
  /**
   * Set CRUD Entitlements on the Integration Resource for the creating User.
   * 
   * @param integrationId UUID of the Integration the Entitlements apply to
   * @param org UUID of the Org that owns the Entitlements
   * @param user User account to create the Entitlements for
   */
  private[controllers] def setCreatorEntitlements(integrationId: UUID, env: UUID, user: AuthAccountWithCreds) = {
    generateEntitlements(user.account.id, env, integrationId, Seq(ResourceIds.Integration), ACTIONS_CRUD )    
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

  /*
   * TODO: This method will be factored out into controllers.util package.
   * Convert input JSON into a GestaltResourceInstance with default values. This is needed
   * for policy checks.
   */
  private[this] def j2r(org: UUID, creator: AuthAccountWithCreds, json: JsValue, typeId: Option[UUID] = None) = {
    inputWithDefaults(
          org = org, 
          typeId = typeId,
          input = safeGetInputJson(json).get, 
          creator = creator)
  }    
  
}
