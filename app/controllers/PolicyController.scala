package controllers

import java.util.UUID

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.ResourceLike
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import controllers.util._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._

import scala.concurrent.Future
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.keymgr.GestaltLicense
import com.galacticfog.gestalt.keymgr.GestaltFeature
import play.api.mvc.Result
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import javax.inject.Singleton
import com.galacticfog.gestalt.json.Js

@Singleton
class PolicyController @Inject()(messagesApi: MessagesApi,
                                 env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  

  // --------------------------------------------------------------------------
  // POST POLICY
  // --------------------------------------------------------------------------
  
  def postResourceToOrg(fqon: String, typ: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newDefaultResourceResult(org, typeid, parent = org, payload = request.body)
  }
  
  def postResource(fqon: String, typ: String, parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newDefaultResourceResult(org, typeid, parent, request.body)
  }
  
  def postResourceOpt(fqon: String, typ: Option[String], parent: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    val typeid = {
      (typ.map(UUID.fromString(_)) orElse resolveTypeFromPayload(request.body)) getOrElse {
        throw new UnprocessableEntityException(s"Missing [resource_type].")
      }
    }
    newDefaultResourceResult(org, typeid, parent, request.body)
  }  
  
  // ==========================================================================
  // RULES
  // ==========================================================================

  /**
   * Implements http://{host}/rules?type={rule-type}
   */
  def getRulesGlobal() = Authenticate() { implicit request =>
    filterRules(ResourceFactory.findSubTypesGlobal(ResourceIds.Rule), request.queryString)
  }
  
  private lazy val RULE_TYPE_NAME = Resources.Rule
  

  /**
   * Filter a list of Rule resources based on the type names given in a querystring.
   * TODO: This only handles a single 'type' filter.
   */
  protected [controllers] def filterRules(rules: Seq[ResourceLike], qs: Map[String,Seq[String]])(implicit request: SecuredRequest[_]) = {
    val outputRules = {
      if (qs.contains("type")) {
        val typeName = "%s::%s".format(RULE_TYPE_NAME, qs("type")(0))
        val typeId = ruleTypeId(typeName) getOrElse {
          throw new BadRequestException(s"Unknown rule-type: " + typeName)
        }
        log.debug("[TypeFilter]: %s - %s".format(typeName, typeId))
        rules filter { _.typeId == typeId }
      } else rules
    }
    handleExpansion(outputRules, qs, META_URL)
  }  
  
  protected [controllers] def typeFilter(rules: Seq[ResourceLike], qs: Map[String,Seq[String]])(implicit request: SecuredRequest[_]) = {
    val outputRules = {
      if (qs.contains("type")) {
        val typeName = "%s::%s".format(RULE_TYPE_NAME, qs("type")(0))
        val typeId = ruleTypeId(typeName) getOrElse {
          throw new BadRequestException(s"Unknown rule-type: " + typeName)
        }
        rules filter { _.typeId == typeId }
      } else rules
    }
    outputRules
  }    
  
  protected [controllers] def setupFilter(typeId: UUID)(implicit request: SecuredRequest[_]) = {
    if (typeId == ResourceIds.Rule) (true, Some(typeFilter _)) else (false, None)
  }
  
}