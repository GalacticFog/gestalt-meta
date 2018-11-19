package controllers

import java.util.UUID


import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.ResourceLike
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import controllers.util._
import com.galacticfog.gestalt.meta.api.sdk._

import play.api.mvc.RequestHeader
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecurity
import com.google.inject.Inject
import play.api.i18n.MessagesApi
import javax.inject.Singleton

@Singleton
class PolicyController @Inject()(messagesApi: MessagesApi,
                                 sec: GestaltFrameworkSecurity)
  extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
  

  // --------------------------------------------------------------------------
  // POST POLICY
  // --------------------------------------------------------------------------
  
  def postResourceToOrg(fqon: String, typ: String) = AsyncAudited(fqon) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newDefaultResourceResult(org, typeid, parent = org, payload = request.body)
  }
  
  def postResource(fqon: String, typ: String, parent: UUID) = AsyncAudited(fqon) { implicit request =>
    val org = fqid(fqon)
    val typeid = UUID.fromString(typ)
    newDefaultResourceResult(org, typeid, parent, request.body)
  }
  
  def postResourceOpt(fqon: String, typ: Option[String], parent: UUID) = AsyncAudited(fqon) { implicit request =>
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
  def getRulesGlobal() = Audited() { implicit request =>
    filterRules(ResourceFactory.findSubTypesGlobal(ResourceIds.Rule), request.queryString)
  }
  
  private lazy val RULE_TYPE_NAME = Resources.Rule
  
  
  /**
   * Filter a list of Rule resources based on the type names given in a querystring.
   * TODO: This only handles a single 'type' filter.
   */
  protected [controllers] def filterRules(rules: Seq[ResourceLike], qs: Map[String,Seq[String]])(implicit request: RequestHeader) = {
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
    handleExpandResourceResult(outputRules.asInstanceOf[Seq[GestaltResourceInstance]], qs, Some(META_URL))
  }  
  
  protected [controllers] def typeFilter(rules: Seq[ResourceLike], qs: Map[String,Seq[String]])(implicit request: RequestHeader) = {
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
  
}