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

import com.galacticfog.gestalt.laser._
import  com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat

import play.api.mvc.Result


case class Entitlement(
    id: UUID,
    org: UUID,
    name: String,
    description: Option[String] = None,  
    properties: EntitlementProps,
    variables: Option[Hstore] = None,
    tags: Option[List[String]] = None)

object Entitlement {
  
  /**
   * Convert a GestaltResourceInstance to an Entitlement object.
   */
  def make(r: GestaltResourceInstance) = {
    Entitlement(
      id = r.id,
      org = r.orgId,
      name = r.name,
      description = r.description,
      properties = EntitlementProps.make(r),
      variables = r.variables,
      tags = r.tags
    )
  }
  
  /**
   * Convert an Entitlement object to a GestaltResourceInstance.
   */
  def toGestalt(creator: UUID, ent: Entitlement) = {
    
    GestaltResourceInstance(
      id = ent.id,
      typeId = ResourceIds.Entitlement,
      orgId = ent.org, // this needs to be org from URI
      owner = ResourceOwnerLink(ResourceIds.User, creator),
      name = ent.name,
      description = ent.description,
      properties = Option(EntitlementProps.toMap(ent.properties)) 
    ) 
  }  

  implicit lazy val entitlementPropsFormat = Json.format[EntitlementProps]
  implicit lazy val entitlementFormat = Json.format[Entitlement]
}


object EntitlementProps {
 /**
  * Convert EntitlementProps object to Map[String,String]
  */
  def toMap(props: EntitlementProps) = {
    val v = props.value map { v => Map("value" -> v) } getOrElse Map()
    val i = props.identities map { ids => 
      Map(
        "identities" -> Json.toJson(ids).as[JsArray].toString.replaceAll("\\\\", "")
      )
    } getOrElse Map()
    
    Map("action" -> props.action) ++ v ++ i
  }
  
  /**
   * Convert GestaltResourceInstance.properties to EntitlementProps object.
   */
  def make(r: GestaltResourceInstance): EntitlementProps = {
    val props = r.properties.get
    val action = props("action")
    val value = if (props.contains("value")) Some(props("value")) else None
    val identities = if (props.contains("identities")) Some(props("identities")) else None
    
    // TODO: Combine ops to regex for replaceAll
    def uuidsFromString(str: String) = str.
      stripPrefix("[").
      stripSuffix("]").
      replaceAll("\"", "").
      split(",").
      toSeq.
      map { UUID.fromString(_)
    }
    EntitlementProps(action, value, identities map { uuidsFromString(_) })
  }    
  
}


case class EntitlementProps(
    action: String, 
    value: Option[String], 
    identities: Option[Seq[UUID]]) {

  /**
   * Validates that action name is valid for the resource type and that all identities given are valid.
   */
  def validate(): Either[String, Unit] = {
    if (identities.isEmpty) Right(())
    else {
      val given = identities.get
      val found = ResourceFactory.findAllIn(given) map { _.id }
      val notFound = given.diff(found)
      
      if (notFound.isEmpty) Right(())
      else Left(s"Invalid identities : [ ${notFound.mkString(",")} ]")
    }
    
  }
}



object AuthorizationController extends MetaController with NonLoggingTaskEvents {
  
  def postEntitlementOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    postEntitlementCommon(org, ResourceIds.Org, org)
  }
  
  def postEntitlementFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    postEntitlementCommon(fqid(fqon), typeId, resourceId)
  }
  
  def deleteEntitlementOrgFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    deleteEntitlementCommon(org, org, id)
  }
  
  def deleteEntitlementFqon(fqon: String, typeId: String, resourceId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>
    deleteEntitlementCommon(fqid(fqon), resourceId, id)
  }
  
  private [controllers] def deleteEntitlementCommon(org: UUID, parent: UUID, id: UUID) = {
    ResourceFactory.findChildOfType(ResourceIds.Entitlement, parent, id) match {
      case None => NotFoundResult(s"Entitlement with ID '$id' not found.")
      case Some(res) => {
        ResourceFactory.hardDeleteResource(ResourceIds.Entitlement, id) match {
          case Success(_) => NoContent
          case Failure(e) => HandleExceptions(e)
        }
      }
    }
  }
  
  def patchEntitlementFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    ???
  }
  
  /*
   * TODO: All the GET methods need to dynamically build the identities list
   */
  
  def transformEntitlement(ent: GestaltResourceInstance, org: UUID, baseUrl: Option[String]): JsValue = {

    val props = EntitlementProps.make(ent)
    
    val output = props.identities match {
      case None => None
      case Some(ids) => {
        val identities = ResourceFactory.findAllIn(ids)
        
        // Transform identity Seq[UUID] to Seq[ResourceLink] JSON.
        val idJson = Output.renderLinks(identities, baseUrl)
        
        JsonUtil.upsertProperty(Json.toJson(ent).as[JsObject], "identities", idJson) match {
          case Failure(e) => throw e
          case Success(newres) => {
            
            val orgLink = {
              Json.toJson {
                com.galacticfog.gestalt.meta.api.output.toLink(
                  ResourceFactory.findById(org).get, baseUrl)
              }
            }
            
            val out =
              JsonUtil.replaceKey(
                JsonUtil.replaceKey(
                  JsonUtil.replaceKey(newres, "typeId", "resource_type"),
                    "state", "resource_state"),
                      "orgId", "org", Some(orgLink))

            Option(out.as[JsValue])
          }
        }
      }
    }  
    if (output.isDefined) output.get else Json.toJson(ent) 
  }
  
  
  def transformEntitlements(ents: Seq[GestaltResourceInstance], org: UUID, baseUrl: Option[String]) = {
    ents map { e => transformEntitlement(e, org, baseUrl) }
  }
  
  
  def getEntitlementsOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    val ents = ResourceFactory.findEffectiveEntitlements(org)
    val entitlements = entitlementsAll(ents)
    
    handleEntitlementExpansion(org, entitlements, request.queryString, META_URL)
  }
  
  
  def handleEntitlementOptions(org: UUID, qs: Map[String,Seq[String]], baseUrl: Option[String] = None) = {
    val merge = booleanParam("effective", qs)
    val filter = qs.get("action")
    
    
  }
  
  def handleEntitlementExpansion(org: UUID, es: Seq[GestaltResourceInstance], qs: Map[String,Seq[String]], baseUrl: Option[String] = None) = {
    if (getExpandParam(qs)) {
      Ok(Json.toJson(transformEntitlements(es, org, baseUrl)))
    } else Ok(Output.renderLinks(es, baseUrl))
  }
  
  def getEntitlementsFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon) { implicit request =>
    getEntitlementsCommon(fqid(fqon), typeId, resourceId)
  }
  
  def getEntitlementByIdOrgFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    ResourceFactory.findById(ResourceIds.Entitlement, id) match {
      case Some(res) => {
        val output = transformEntitlement(res, fqid(fqon), META_URL)
        Ok(output)
      }
      case None => NotFoundResult(request.uri)
    }
  }
  
  def getEntitlementByIdFqon(fqon: String, typeId: String, resourceId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>  
    ???
  }

  private[controllers] def postEntitlementCommon(org: UUID, typeId: UUID, resourceId: UUID)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    // This is the resource we're creating the Entitlement for.
    val parentResource = ResourceFactory.findById(typeId, resourceId)
    
    parentResource match {
      case None => NotFoundResult(s"${ResourceLabel(typeId)} with ID '$resourceId' not found.")
      case Some(_) => {

        validateEntitlementPayload(org, request.identity, request.body) match {
          case Failure(e) => HandleExceptions(e)
          case Success(r) => {
            createResourceInstance(
              org, request.body, 
              Some(ResourceIds.Entitlement), 
              Some(resourceId)) match {
                case Failure(err) => HandleExceptions(err)
                case Success(res) => Created(transformEntitlement(res, org, META_URL)) 
              }
              
            //CreateResourceResult(org, ResourceIds.Entitlement, resourceId)
          }
        }
        
      }
    }
    
  }

//  Implement type-hierarchy:
//  Resource->Runnable->ResourceContainer->{Org, Workspace}
//  Resource->Runnable->{Container,Lambda}

  private[controllers] def validateEntitlementPayload(org: UUID, creator: AuthAccountWithCreds, payload: JsValue): Try[GestaltResourceInstance] = Try {
    log.debug("Entered: validateEntitlementPayload(...)")
    
    /*
     * Validations:
     * 1.) Caller has SetEntitlements permission
     * 2.) Action is valid
     * 3.) Entitlement for given action DOES NOT already exist
     * 4.) Given identities ALL exist
     */
    
    //
    // TODO: Validate that 'action' is unique for this resource (specified at most once).
    //
    
    //
    // TODO: Remove unnecessary pattern matching
    //
    toResource(org, creator, payload) match {
      case Failure(err) => throw err
      case Success(res) => {
        EntitlementProps.make(res).validate match {
          case Left(err) => throw new BadRequestException(err)
          case Right(_) => res
        }
      }
    }
  }
  
  
  def entitlementAction(e: GestaltResourceInstance) = {
    e.properties.get("action")
  }  
 


  /**
   * TODO: Get rid of unnecessary pattern-matching
   */
  def toResource(org: UUID, creator: AuthAccountWithCreds, json: JsValue) = Try {
    log.debug("Entered: toResource(...)")
    safeGetInputJson(ResourceIds.Entitlement, json) match {
      case Failure(error) => throw error
      case Success(input) => {
      
        inputWithDefaults(org, input.copy(resource_type = Option(ResourceIds.Entitlement)), creator)
      }
    }
  }
  
  private[controllers] def getEntitlementByIdCommon(org: UUID, typeId: UUID, resourceId: UUID, id: UUID)(implicit request: SecuredRequest[_]) = {
    ResourceFactory.findChildOfType(ResourceIds.Entitlement, resourceId, id) match {
      case Some(res) => Ok(Output.renderInstance(res, META_URL))
      case None => NotFoundResult(request.uri)
    }
  }
  
  private def entitlementsAll(es: Map[Int, Seq[GestaltResourceInstance]]): Seq[GestaltResourceInstance] = {
    es flatMap { case (_,v) => v } toSeq
  }  
  
  def entitlementsFiltered(es: Map[Int, Seq[GestaltResourceInstance]], actions: Seq[String]): Seq[GestaltResourceInstance] = {
    es flatMap { case (_,v) => v } filter { e => actions.contains(entitlementAction(e)) } toSeq
  }  
  
  
  def entitlementsMerged(es: Map[Int, Seq[GestaltResourceInstance]]): Seq[GestaltResourceInstance] = {

    def go(
        exclude: Map[String, GestaltResourceInstance], 
        ez: List[(Int, Seq[GestaltResourceInstance])], 
        acc: Map[String, GestaltResourceInstance])  : Map[String, GestaltResourceInstance] = {
      ez match {
        case Nil => acc
        case h :: t => go(exclude, t, acc ++ accumulateEnts(acc, h._2))
      }
    }

    if (es.isEmpty) {
      
      Seq()
    }
    else {
      // Highest key corresponds with highest level in the hierarchy.
      val max = es.keys.max
      val topLevel = es(max)
      
      // Top-Down evaluation
      //val ents = (es - max).toSeq.sortWith((a,b) => a._1 > b._1)
      
      // Bottom-Up evaluation
      val ents = (es - max).toSeq.sortWith((a,b) => a._1 < b._1)
  
      // Add all top-level entitlements to map
      val output = topLevel map { e => (entitlementAction(e), e) } toMap    
      
      (output ++ go(output, ents.toList, Map())).values.toSeq
    }    
  }  
  
  def accumulateEnts(exclude: Map[String,GestaltResourceInstance], ents: Seq[GestaltResourceInstance]) = {
    def go(es: Seq[GestaltResourceInstance], acc: Map[String, GestaltResourceInstance]): Map[String,GestaltResourceInstance] = {
      es match {
        case Nil => acc
        case h :: t => {
          val action = entitlementAction(h)
          val newacc = if (acc.contains(action)) acc else acc ++ Map(action -> h)
          
          go(t, newacc)
        }
      }
    }
    go(ents, exclude)
  }  
  
  /**
   * This currently only gets entitlements that are set DIRECTLY on the target Resource (resourceId)
   */
  def getEntitlementsCommon(org: UUID, typeId: UUID, resourceId: UUID)(implicit request: SecuredRequest[_]) = {
    handleExpansion(ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, resourceId),
        request.queryString, META_URL)    
  }

  
  def getEntitlementsMerged(resourceId: UUID): Seq[GestaltResourceInstance] = {
    println("---- Looking up entitlements for Resource : " + resourceId)
    entitlementsMerged(ResourceFactory.findEffectiveEntitlements(resourceId))
  }

  
  def isAuthorized(resource: UUID, identity: UUID, action: String, account: AuthAccountWithCreds) = Try {
    findMatchingEntitlement(resource, action) match {
      case None => false
      case Some(entitlement) => {
        val allowed = getAllowedIdentities(entitlement)
        val membership = getUserMembership(identity, account)
        (allowed intersect membership).isDefinedAt(0)
      }
    }
  }
  
  def getUserMembership(user: UUID, account: AuthAccountWithCreds): Seq[UUID] = {
    user +: (Security.getAccountGroups(user, account).get map { _.id })
  }
  
  def findMatchingEntitlement(resource: UUID, action: String): Option[GestaltResourceInstance] = {
    val ents = AuthorizationController.getEntitlementsMerged(resource) filter { ent =>
      ent.properties.get("action") == action
    }
    println("-----Matching Entitlements:\n" + ents)
    if (ents.isEmpty) None
    else if (ents.size > 1) {
      throw new RuntimeException(s"Multiple entitlements found for action '$action'. Data is corrupt.")
    } 
    else Option(ents(0))    
  }
  
  def getAllowedIdentities(entitlement: GestaltResourceInstance): Seq[UUID] = {
    entitlement.properties.get.get("identities") match {
      case None => Seq()
      case Some(identities) => {
        
        Json.parse(identities).validate[Seq[String]].map {
          case sq: Seq[String] => sq map { UUID.fromString(_) }
        }.recoverTotal { e =>
          throw new RuntimeException(
              s"Failed parsing Entitlement identities: " + JsError.toFlatJson(e).toString)
        }
      }
    }
  }
  
  /**
   * Determine if an identity is found in a list of users and groups
   * 
   * @param identity the user ID to test
   * @param allowed list of User and Group IDs
   */
//  def isInRole(identity: UUID, identities: Seq[UUID], account: AuthAccountWithCreds) = {
//    val (users,groups) = ResourceFactory.findAllIn(identities) partition { _.typeId == ResourceIds.User }
//    
//    println("USERS  : " + (users map { _.id }))
//    println("GROUPS : " + (groups map { _.id }))
//    
//    val userids = users map { _.id }
//    if (userids.contains(identity)) true
//    else {
//      // Check if the user exists in one of the groups.
//      groups exists { g => isInGroup(g.id, identity, account) }
//    }
//  }
  
//  def isInGroup(groupId: UUID, target: UUID, auth: AuthAccountWithCreds) = {
//    val users = Security.getGroupAccounts(groupId, auth) match {
//      case Failure(err) => throw err
//      case Success(users) => users map { _.id }
//    } 
//    users contains target
//  }
  
  
}

