package controllers

import java.util.UUID

import scala.concurrent.ExecutionContext.Implicits.global
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

@Singleton
class PolicyController @Inject()(messagesApi: MessagesApi,
                                 env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  type FilterFunction = ((Seq[ResourceLike], QueryString) => Seq[ResourceLike])
  type LookupFunction = ((UUID,UUID) => Try[ResourceLike])
  
  //case class LookFunction[T <: ResourceLike](t1: UUID, t2: UUID) extends ((UUID,UUID) => T)
  
  def getResourceListOrgFqon(fqon: String, resourceType: String) = Authenticate(fqon) { implicit request =>
    val rtid = UUID.fromString(resourceType)
    val (subtypes,filter) = setupFilter(rtid)
    getResourceListFqonCommon(fqon, ResourceIds.Org, fqid(fqon), rtid, subtypes)(filter, Some(request.queryString))
  }
  
  def getResourceListFqon(fqon: String, parentType: String, parentId: UUID, resourceType: String) = Authenticate(fqon) { implicit request =>
    val ptid = UUID.fromString(parentType)
    val rtid = UUID.fromString(resourceType)
    val (subtypes,filter) = setupFilter(rtid)
    getResourceListFqonCommon(fqon, ptid, parentId, rtid, subtypes)(filter, Some(request.queryString))
  }
  
  def getResourceListFqonCommon(
      fqon: String, 
      parentType: UUID, 
      parentId: UUID, 
      resourceType: UUID,
      includeSubTypes: Boolean = false)(
          filter: Option[FilterFunction] = None, 
          qs: Option[QueryString] = None)(
              implicit request: SecuredRequest[_]) = {
    
    ResourceFactory.findById(parentType, parentId) match {
      case None => ResourceNotFound(parentType, parentId)
      case Some(_) => {
        val rs = if (includeSubTypes) 
          ResourceFactory.findChildrenOfSubType(resourceType, parentId)
        else ResourceFactory.findChildrenOfType(resourceType, parentId)
        
        handleExpansion(if (filter.isDefined) filter.get(rs, qs.get) else rs, request.queryString, META_URL)        
      }
    }

  }

  def getResourceByIdOrgFqon(fqon: String, resourceType: String, resourceId: UUID, subTypes: Boolean = false) = Authenticate(fqon) { implicit request =>
    val rtid = UUID.fromString(resourceType)
    getResourceByIdFqonCommon(fqon, ResourceIds.Org, fqid(fqon), rtid, resourceId){
      if (subTypes) Some(findCovariantResource _) else None
    }
  }
  
  def getResourceByIdFqon(
      fqon: String, 
      parentType: String, 
      parentId: UUID, 
      resourceType: String, 
      resourceId: UUID, 
      subTypes: Boolean = false) = Authenticate(fqon) { implicit request =>
        
    val ptid = UUID.fromString(parentType)
    val rtid = UUID.fromString(resourceType)

    getResourceByIdFqonCommon(fqon, ptid, parentId, rtid, resourceId) {
      if (subTypes) Some(findCovariantResource _) else None
    }
  }
  
  def getResourceByIdFqonCommon[T](
      fqon: String, 
      parentType: UUID, 
      parentId: UUID, 
      resourceType: UUID, 
      resourceId: UUID)(fn: Option[LookupFunction] = None)(implicit request: SecuredRequest[_]) = {
    
    val lookup: LookupFunction = if (fn.isDefined) fn.get else findResource _
    
    val target = for {
      p <- findResource(parentType, parentId)
      c <- lookup(resourceType, resourceId)
    } yield c

    target match {
      case Failure(e) => HandleExceptions(e)
      case Success(r) => {
        val out = postProcess(r.asInstanceOf[GestaltResourceInstance])
        Ok(Output.renderInstance(out))
      }
    }
  }
  
  def postProcess(r: GestaltResourceInstance)(implicit request: SecuredRequest[_]): GestaltResourceInstance = {
    def upsertProperties(resource: GestaltResourceInstance, values: (String,String)*) = {
      resource.copy(properties = Some((resource.properties getOrElse Map()) ++ values.toMap))
    }
    
    if (r.typeId != ResourceIds.Policy) r else {
      val rs = ResourceFactory.findChildrenOfSubType(ResourceIds.Rule, r.id) map { r => 
        toLink(r, META_URL)  
      }
      upsertProperties(r, "rules" -> Json.stringify(Json.toJson(rs)))
    }
    
  }
  
  protected [controllers] def findCovariantResource(baseType: UUID, id: UUID) = Try {
    val out = ResourceFactory.findTyped(CoVariant(baseType), id) getOrElse {
      throw new ResourceNotFoundException(s"${ResourceLabel(baseType)} with ID '$id' not found.")
    }
    out.asInstanceOf[GestaltResourceInstance]
  }
  
  protected [controllers] def findResource(typeId: UUID, id: UUID) = Try {
    ResourceFactory.findById(typeId, id) getOrElse {
      throw new ResourceNotFoundException(s"${ResourceLabel(typeId)} with ID '$id' not found.")
    }
  }
  // --------------------------------------------------------------------------
  // POST POLICY
  // --------------------------------------------------------------------------

  def postPolicyOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    createPolicyCommon(org, ResourceIds.Org, org)
  }
  
  def postPolicyFqon(fqon: String, parentType: String, parentId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    createPolicyCommon(fqid(fqon), UUID.fromString(parentType), parentId) recoverWith { case e =>
      Future(HandleExceptions(e))
    }
  }
  
  def WithFeature(feature: GestaltFeature, failMessage: String)(block: => Future[Result]) = {
    if (!GestaltLicense.instance.isFeatureActive(feature)) {
      log.warn(s"Attempt to use feature '${feature.toString}' denied due to license.")
      Future(throw new NotAcceptableException(failMessage))
    } else block
  }
  
  //
  // TODO: Add entitlements and authorization.
  //
  
  def createPolicyCommon(org: UUID, parentType: UUID, parentId: UUID)(implicit request: SecuredRequest[JsValue]) = {
    
    WithFeature(GestaltFeature.Policy, "Policy creation is disabled under your current license.") {
      
      ResourceFactory.findById(parentType, parentId) match {
        case None => Future(ResourceNotFound(parentType, parentId))
        case Some(parent) => {
          val json = updatePolicyJson(request.body, parentId)
          createResourceD(org, json, Some(ResourceIds.Policy), Some(parentId))
        }
      }
      
    }
  }
  
  
  def postResourceCommon(
      org: UUID, 
      parentType: UUID, 
      parentId: UUID,  
      resourceJson: JsValue,
      resourceType: Option[UUID] = None)(
          pre: Option[(JsValue => JsValue)])(
          post: Option[(ResourceLike => ResourceLike)])(
            implicit request: SecuredRequest[JsValue]) = Future {
    
    val json = if (pre.isDefined) pre.get(resourceJson) else resourceJson
    
    (for {
      p <- findResource(parentType, parentId)
      c <- createResourceInstance(org, json, resourceType, Some(p.id))
    } yield c) match {
      case Failure(e) => HandleExceptions(e)
      case Success(r) => {
        val out = if (post.isDefined) post.get(r) else r
        Created(Output.renderInstance(out.asInstanceOf[GestaltResourceInstance]))
      }
    }
  }
  

  def hardDeleteSimpleOrgFqon(fqon: String, targetId: UUID) = Authenticate(fqon) {
    val org = fqid(fqon)
    hardDeleteSimple(org, ResourceIds.Org, org, targetId)
  }
  def hardDeleteSimpleFqon(fqon: String, parentType: String, parentId: UUID, targetId: UUID) = Authenticate(fqon) {
    hardDeleteSimple(fqid(fqon), UUID.fromString(parentType), parentId, targetId)
  }
  
  def hardDeleteSimple(org: UUID, parentType: UUID, parentId: UUID, targetId: UUID) = {
    (for {
      p <- findResource(parentType, parentId)
      x <- ResourceFactory.hardDeleteResource(targetId)
    } yield x) match {
      case Failure(e) => HandleExceptions(e)
      case Success(_) => NoContent
    }
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
  
  def postRuleFqon(fqon: String, policy: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    log.debug(s"Received request to create new Rule for Policy($policy)")
    
    val resourceType = Try {
      (request.body \ "resource_type") match {
        case u: JsUndefined => 
          throw new BadRequestException("You must provide a 'resource_type'")
        case v => {
          val expanded = expandRuleTypeName(v.as[String])
          ruleTypeId(expanded)
        }
      }
    }

    resourceType match {
      case Failure(e) => Future(HandleExceptions(e))
      case Success(tpe) => {
        ResourceFactory.findById(ResourceIds.Policy, policy).fold {
          Future(NotFoundResult(s"Policy with ID '$policy' not found."))
        }{ pol =>  
          val json = updateRuleJson(request.body, tpe.get, pol)
          log.debug("Creating Rule:\n" + Json.prettyPrint(json))
          createResourceD(fqid(fqon), json, tpe, Some(policy))
        }
      }
    }
  }
  
  protected [controllers] def parentLink(pid: UUID, baseUri: Option[String]): Option[ResourceLink] = {
    ResourceFactory.findById(pid) map { toLink( _, baseUri ) }
  }
  
  protected [controllers] def updatePolicyJson(policyJson: JsValue, parent: UUID)(implicit request: SecuredRequest[_]) = {
    val link = Json.toJson(parentLink(parent, META_URL).get)
    JsonUtil.withJsonPropValue(policyJson.as[JsObject], "parent", link)
  }
  
  protected [controllers] def updateRuleJson(ruleJson: JsValue, resourceType: UUID, parent: GestaltResourceInstance) = {
    val json = ruleJson.as[JsObject] ++ Json.obj("resource_type" -> JsString(resourceType.toString))
    val json2 = JsonUtil.withJsonPropValue(json, "parent", JsString(parent.id.toString))
    val definedAt = Json.parse(parent.properties.get("parent"))
    JsonUtil.withJsonPropValue(json2, "defined_at", definedAt)
  }
  
  protected [controllers] def getPolicyParentLink(policy: GestaltResourceInstance) = {
    val id = (Json.parse(policy.properties.get("parent")) \ "id") match {
      case u: JsUndefined => throw new RuntimeException(s"Failed to parse policy.properties.parent")
      case v => UUID.fromString(v.as[String]) 
    }
  }

  /**
   * Get Rule type ID from its fully-qualified name.
   */
  protected [controllers] def ruleTypeId(typeName: String) = {
    typeName match {
      case a if a == Resources.RuleEvent  => Some(ResourceIds.RuleEvent)
      case b if b == Resources.RuleConfig => Some(ResourceIds.RuleConfig)
      case c if c == Resources.RuleLimit  => Some(ResourceIds.RuleLimit)
      case _ => None
    }
  }
  
  private lazy val RULE_TYPE_NAME = Resources.Rule
  
  /**
   * Create a fully-qualified Rule type name from it's simple name, i.e.,
   * 'event' becomes 'Gestalt::Resource::Rule::Event'
   */
  protected [controllers] def expandRuleTypeName(shortName: String) = {
    if (Seq("config", "event", "limit").contains(shortName.trim.toLowerCase)) {
      "%s::%s".format(RULE_TYPE_NAME, shortName.trim.capitalize)
    } else throw new BadRequestException(s"Invalid Rule type - found: '$shortName'")
  }

  
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