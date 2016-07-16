package controllers

import java.util.UUID

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.session
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.output.gestaltResourceInstanceFormat
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.meta.api.sdk.resourceLinkFormat
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import controllers.util.HandleExceptions
import controllers.util.JsonUtil
import controllers.util.NotFoundResult
import play.api.{Logger => log}
import play.api.libs.json.JsError
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json


//
// TODO: Rename to 'EntitlementController'
//

object AuthorizationController extends Authorization {
  
  // --------------------------------------------------------------------------
  // POST
  // --------------------------------------------------------------------------
  
  /**
   * POST /{fqon}/entitlements
   */
  def postEntitlementOrgFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    postEntitlementCommon(org, ResourceIds.Org, org)
  }
  
  /**
   * POST /{fqon}/{resource-type}/entitlements
   */
  def postEntitlementFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    postEntitlementCommon(fqid(fqon), typeId, resourceId)
  }
  
  
  // --------------------------------------------------------------------------
  // GET
  // --------------------------------------------------------------------------
  
  def getEntitlementsOrgFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    getEntitlementsCommon(org, ResourceIds.Org, org)
  }
  
  def getEntitlementsFqon(fqon: String, typeId: String, resourceId: UUID) = Authenticate(fqon) { implicit request =>
    getEntitlementsCommon(fqid(fqon), typeId, resourceId)
  }
  
  def getEntitlementByIdOrgFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    getEntitlementByIdCommon(org, ResourceIds.Org, org, id)
  }
  
  def getEntitlementByIdFqon(fqon: String, typeId: String, resourceId: UUID, id: UUID) = Authenticate(fqon) { implicit request =>  
    getEntitlementByIdCommon(fqid(fqon), UUID.fromString(typeId), resourceId, id)
  }
  
  
 
  
  /**
   * DELETE /{fqon}/entitlements/{id}
   */
  def deleteEntitlementOrgFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    val org = fqid(fqon)
    deleteEntitlementCommon(org, org, id)
  }
  
  /**
   * DELETE /{fqon}/{resource-type}/entitlements/{id}
   */
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
  
  
  def handleEntitlementOptions(org: UUID, qs: Map[String,Seq[String]], baseUrl: Option[String] = None) = {
    val merge = booleanParam("effective", qs)
    val filter = qs.get("action")
    
  }
  
  
  def handleEntitlementExpansion(org: UUID)(entitlements: Seq[GestaltResourceInstance])(implicit request: SecuredRequest[_]) = {
    if (getExpandParam(request.queryString)) {
      Ok(Json.toJson(transformEntitlements(org)(entitlements)))
    } else Ok(Output.renderLinks(entitlements, META_URL))
  }
  
  
  def validateEntitlementUpdate(old: GestaltResourceInstance, newent: GestaltResourceInstance) = {
    val oldaction = old.properties.get("action")
    val newaction = newent.properties.get("action")
    
    for {
      r1 <- Try { if (old.id == newent.id) newent else throw new ConflictException("You may not modify the resource ID.") }
      r2 <- Try { if (oldaction == newaction) newent else throw new ConflictException(s"You may not modify the entitlement action. Found: '$newaction', Expected: '$oldaction'") }
    } yield r2
    
  }
  
  def copyEntitlementForUpdate(old: GestaltResourceInstance, newent: GestaltResourceInstance) = Try {
     newent.copy(created = old.created)
  }  
  
  def putEntitlementOrgFqon(fqon: String, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    val org = fqid(fqon)
    putEntitlementCommon(org, org, id)
  }
  
  def putEntitlementFqon(fqon: String, parentTypeId: String, parentId: UUID, id: UUID) = Authenticate(fqon).async(parse.json) { implicit request =>
    val parentType = UUID.fromString(parentTypeId)
    ResourceFactory.findById(parentTypeId, parentId) match {
      case None => Future(NotFoundResult(s"${ResourceLabel(parentId)} with ID '$id' not found."))
      case Some(parent) => {
        putEntitlementCommon(fqid(fqon), parent.id, id)
      }
    }
  }
  
  private[controllers] def putEntitlementCommon(org: UUID, parent: UUID, id: UUID)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    val user = request.identity
    val json = request.body
    
    ResourceFactory.findById(ResourceIds.Entitlement, id) map { ent =>
      for {
        r1 <- validateEntitlementPayload(org, parent, user, json, "update")
        r2 <- validateEntitlementUpdate(ent, r1)
        r3 <- copyEntitlementForUpdate(ent, r2)
        r4 <- ResourceFactory.update(r3, user.account.id)
      } yield r4
      
    } getOrElse {
      
     throw new ResourceNotFoundException(s"Entitlement with ID '$id' not found.")
     
    } match {
      case Failure(e) => HandleExceptions(e)
      case Success(r) => Ok(transformEntitlement(r, org, META_URL))
    }
    
  }
  

  
  private[controllers] def postEntitlementCommon(org: UUID, typeId: UUID, resourceId: UUID)(
      implicit request: SecuredRequest[JsValue]) = Future {
    
    // This is the resource we're creating the Entitlement for.
    val parentResource = ResourceFactory.findById(typeId, resourceId)
    
    parentResource match {
      case None => NotFoundResult(s"${ResourceLabel(typeId)} with ID '$resourceId' not found.")
      case Some(parent) => {

        validateEntitlementPayload(org, parent.id, request.identity, request.body, "create") match {
          case Failure(e) => HandleExceptions(e)
          case Success(r) => {
            createResourceInstance(
              org, request.body, 
              Some(ResourceIds.Entitlement), 
              Some(resourceId)) match {
                case Failure(err) => HandleExceptions(err)
                case Success(res) => Created(transformEntitlement(res, org, META_URL)) 
              }

          }
        }   
      }
    }
  }

  
  
  
  /**
   * This currently only gets entitlements that are set DIRECTLY on the target Resource (resourceId)
   */
  private[controllers] def getEntitlementsCommon(org: UUID, typeId: UUID, resourceId: UUID)(implicit request: SecuredRequest[_]) = {
    handleEntitlementExpansion(org) {
      ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, resourceId)
    }
  }
  
  private[controllers] def getEntitlementByIdCommon(org: UUID, typeId: UUID, resourceId: UUID, id: UUID)(
      implicit request: SecuredRequest[_]) = {

    ResourceFactory.findChildOfType(ResourceIds.Entitlement, resourceId, id) match {
      case Some(res) => Ok(transformEntitlement(res, org, META_URL))
      case None      => NotFoundResult(s"Entitlement with ID '$id' not found.")
    }
  }

  private[controllers] def validateEntitlementPayload(
      org: UUID, 
      parent: UUID, 
      creator: AuthAccountWithCreds, 
      payload: JsValue,
      accessType: String): Try[GestaltResourceInstance] = Try {
    
    log.debug("Entered: validateEntitlementPayload(...)")
    
    /*
     * Validations:
     * 1.) Caller has SetEntitlements permission
     * 2.) Action is valid
     * 3.) Entitlement for given action DOES NOT already exist
     * 4.) Given identities ALL exist
     */
    
    //
    // TODO: Validate user has 'setEntitlements' permission
    //???
    
    // TODO: Validate that 'action' is unique for this resource (specified at most once).
    // ???
    
    
    //
    // TODO: Remove unnecessary pattern matching
    //
    toResource(org, creator, payload) match {
      case Failure(err) => throw err
      case Success(resource) => {
        EntitlementProps.make(resource).validate match {
          case Left(err) => throw new BadRequestException(err)
          case Right(props) => {

            
            /*
             * If we're trying to create, existing must be empty.
             */
            
            
            if (accessType == "update") resource
            else if (accessType == "create") { 
              
              val existing = entitlementsByAction(parent, props.action)
              
              println("EXISTING-ENTITLEMENTS:")
              existing foreach {e => println(e.properties.get)}
              
              if (existing.isEmpty) resource
              else {
                throw new ConflictException(
                    s"Found existing entitlement for action '${props.action}'. There can be only one.")
              }
              
            } else {
              throw new RuntimeException(s"Unhandled accessType: '$accessType'")
            }
            
          }
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
    
    safeGetInputJson(ResourceIds.Entitlement, json) match {
      case Failure(error) => throw error
      case Success(input) => {
      
        inputWithDefaults(org, input.copy(resource_type = Option(ResourceIds.Entitlement)), creator)
      }
    }
  }
  

  
  private def entitlementsAll(es: Map[Int, Seq[GestaltResourceInstance]]): Seq[GestaltResourceInstance] = {
    es flatMap { case (_,v) => v } toSeq
  }  
  

  private[controllers] def entitlementsFiltered(es: Map[Int, Seq[GestaltResourceInstance]], actions: Seq[String]): Seq[GestaltResourceInstance] = {
    es flatMap { case (_,v) => v } filter { e => actions.contains(entitlementAction(e)) } toSeq
  }  
  
  
  def entitlementsMerged(es: Map[Int, Seq[GestaltResourceInstance]]): Seq[GestaltResourceInstance] = {

    @tailrec def go(
        exclude: Map[String, GestaltResourceInstance], 
        ez: List[(Int, Seq[GestaltResourceInstance])], 
        acc: Map[String, GestaltResourceInstance])  : Map[String, GestaltResourceInstance] = {
      ez match {
        case Nil => acc
        case h :: t => go(exclude, t, acc ++ accumulateEnts(acc, h._2))
      }
    }

    if (es.isEmpty) Seq() else {
      
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
  
  
  private[controllers] def isAuthorized(resource: UUID, identity: UUID, action: String, account: AuthAccountWithCreds) = Try {
    findMatchingEntitlement(resource, action) match {
      case None => false
      case Some(entitlement) => {
        println("*******************GOT ENTITLEMENTS**********************")
        val allowed = getAllowedIdentities(entitlement)
        val membership = getUserMembership(identity, account)
        
        println("***ALLOWED IDENTITIES:\n" + allowed)
        println("***MEMBERSHIP:\n" + membership)
        println("INTERSECT: " + (allowed intersect membership))
        
        (allowed intersect membership).isDefinedAt(0)
        
      }
    }
  }  
  

  def getEntitlementsMerged(resourceId: UUID): Seq[GestaltResourceInstance] = {
    entitlementsMerged(ResourceFactory.findEffectiveEntitlements(resourceId))
  }

  
  /**
   * Get a Seq containing the User ID and the IDs of all Groups the User is a
   * member of.
   */
  private[controllers] def getUserMembership(user: UUID, account: AuthAccountWithCreds): Seq[UUID] = {
    //user +: (Security.getAccountGroups(user, account).get map { _.id })
    user +: account.groups.map( _.id )
  }
  
  
  object PermissionSet {
    val SELF = "self"
    val MERGED = "merged"
  }
  
  def entitlementsByAction(resource: UUID, action: String, setType: String = PermissionSet.SELF) = {
    val entitlements = setType match {
      case PermissionSet.SELF => ResourceFactory.findChildrenOfType(ResourceIds.Entitlement, resource)
      case PermissionSet.MERGED => ResourceFactory.findEffectiveEntitlements(resource) flatMap { _._2 } toList
      case e => throw new IllegalArgumentException(s"Invalid PermissionSet type '$e'")
    } 
    entitlements filter { _.properties.get("action") == action }
  }
  
  
  /**
   * Search the effective entitlements on the given resource for the given action name.
   * 
   * @param resource ID of Resource to search for the matching entitlement
   * @param action   exact name of the action to search for
   */
  private[controllers] def findMatchingEntitlement(resource: UUID, action: String): Option[GestaltResourceInstance] = {
    val ents = AuthorizationController.getEntitlementsMerged(resource) filter { ent =>
      ent.properties.get("action") == action
    }
    println("**********Entitlements Found:\n" + ents)
    if (ents.isEmpty) None
    else if (ents.size > 1) {
      throw new RuntimeException(
          s"Multiple entitlements found for action '$action'. Data is corrupt.")
    } 
    else Option(ents(0))    
  }
  
  
  /**
   * Parse the array of identities defined in an entitlement Resource. 
   * (resource.properties.identities)
   * 
   */
  private[controllers] def getAllowedIdentities(entitlement: GestaltResourceInstance): Seq[UUID] = {
    
    entitlement.properties.get.get("identities") match {
      case None => Seq()
      case Some(identities) => {
        
        Json.parse(identities).validate[Seq[String]].map {
          case sq: Seq[String] => sq map { UUID.fromString(_) }
        }.recoverTotal { e =>
          log.error("Failed parsing 'identities' to Seq[UUID].")
          throw new RuntimeException(
              s"Failed parsing Entitlement identities: " + JsError.toFlatJson(e).toString)
        }
        
      }
    }
  }
  
  
  /*
   * HACK: This is temporary. the 'transformEntitlement' methods are used to transform 'entitlement.properties.identities' 
   * from a Seq[UUID] to a Seq[ResourceLink]. This is necessary because 'identity' is a polymorphic reference
   * (either a User or a Group), and the type system has no way to indicate this fact for reference properties.
   */
//  private[this] def transformEntitlements(ents: Seq[GestaltResourceInstance], org: UUID, baseUrl: Option[String]) = {
//    ents map { e => transformEntitlement(e, org, baseUrl) }
//  }
  
  private[this] def transformEntitlements(org: UUID)(entitlements: => Seq[GestaltResourceInstance])(implicit request: SecuredRequest[_]) = {
    entitlements map { transformEntitlement(_, org, META_URL) }    
  }
  
  private[this] def transformEntitlement(ent: GestaltResourceInstance, org: UUID, baseUrl: Option[String]): JsValue = {

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

  
}

