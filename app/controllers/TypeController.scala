package controllers


import play.api.{ Logger => log }

import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import play.api.Play.current
import play.api.libs.ws._
import play.api.libs.ws.ning.NingAsyncHttpClientConfigBuilder
import scala.concurrent.Future
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.RequestHeader
import play.api.mvc.AnyContent
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{ Try, Success, Failure }
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk.{ ResourceLink => GestaltLink }

import controllers.util._
import controllers.util.db._
import play.mvc.Result
import java.util.UUID
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltBaseAuthProvider
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecuredController
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.{ DummyAuthenticatorService, DummyAuthenticator }
import com.galacticfog.gestalt.security.api.{GestaltResource => SecurityResource}
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}

import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.api.json.JsonImports
import play.api.libs.json._
import com.galacticfog.gestalt.security.api.json.JsonImports.{ orgFormat, linkFormat, acctFormat }
import com.mohiva.play.silhouette.api.util.Credentials

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.output._

import com.galacticfog.gestalt.security.api.{ GestaltResource => SecuredResource }
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.auth.Authorization


object TypeController extends Authorization {
  

  def getAllResourceTypes(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    Ok(Output.renderLinks(TypeFactory.findAll(ResourceIds.ResourceType, org)))
  }
  
  def getTypesByName(org: UUID, names: List[String]): Seq[ResourceLike] = {
    def go(nms: List[String], out: Seq[ResourceLike]): Seq[ResourceLike] = {
      nms match {
        case Nil => out
        case h :: t => go(t, out ++ TypeFactory.findByName(org, h))
      }
    }
    go(names, Seq())
  }
  

  
  def getAllResourceTypesFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    orgFqon(fqon) match {
      case None => NotFoundResult(Errors.ORG_NOT_FOUND(fqon))
      case Some(org) => {
        if (request.queryString.contains("name")) {
          val names = request.queryString("name").toSeq
          val tpes = names flatMap { TypeFactory.findByName(org.id, _)}
          Ok(Output.renderLinks(tpes))
        } else {
          OkTypeLinksResult(org.id)
        }
      }
    }
    
  }
  
  def getResourceTypeById(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    OkTypeByIdResult(org, id)
  }
  
  def getResourceTypeByIdFqon(fqon: String, id: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => OkTypeByIdResult(org.id, id)
      case None => NotFoundResult(Errors.TYPE_NOT_FOUND(id))
    }
  }
  
  def createResourceType(org: UUID) = GestaltFrameworkAuthAction(Some(org)).async(parse.json) { implicit request =>
    CreateTypeWithPropertiesResult(org, request.body)
  }
  
  def createResourceTypeFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)).async(parse.json) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => CreateTypeWithPropertiesResult(org.id, request.body)
      case None      => Future { OrgNotFound(fqon) }
    }
  }

  private def CreateTypeWithPropertiesResult[T](org: UUID, typeJson: JsValue)(implicit request: SecuredRequest[T]) = {
    Future {
      createTypeWithProperties(org, typeJson) match {
        case Failure(e) => GenericErrorResult(500, e.getMessage)
        case Success(newtype) => Ok(Output.renderResourceTypeOutput( newtype ))
      }
    }
  }
  
  /**
   * Decompose a GestaltResourceTypeInput into a tuple2 where:
   * _1 : A GestaltResourceType
   * _2 : An Option[Seq[GestaltTypeProperty]]
   */
  private def deconstructType(org: UUID, owner: UUID, typeJson: JsValue)(): 
      (GestaltResourceType, Option[Seq[GestaltTypeProperty]]) = {    
    
    val input: GestaltResourceTypeInput = safeGetTypeJson(typeJson).get
    
    val domain: GestaltResourceType = typeFromInput(org, owner, input).get
    
    val definitions: Option[Seq[GestaltTypeProperty]] = 
      input.property_defs map { ps => ps map { p => 
        PropertyController.propertyFromInput(org, owner, p.copy(applies_to = Some(domain.id))).get } 
    }
    
    (domain, definitions)
  }

  
  private def createTypeWithProperties[T](org: UUID, typeJson: JsValue)(implicit request: SecuredRequest[T]) = {
    Try {
      val owner = request.identity.account.id
      
      log.debug("Converting input types to domain...")
      val (domain, propdefs) = deconstructType(org, owner, typeJson)

      log.debug("Creating new ResourceType...")
      val newtype = TypeFactory.create(owner)(domain).get
      
      log.debug("Checking for TypeProperties...")
      if (propdefs.isDefined) {
        for (prop <- propdefs.get) {
          log.debug(s"Creating TypeProperty : ${prop.name}")
          PropertyFactory.create(owner)(prop).get
        }
      }
      trace("createTypeWithProperties => COMPLETE.")
      newtype
    }
  }
  
  
  /* DELETE /orgs/:uuid/resourcetypes/:uuid */
  def deleteResourceTypeById(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    ???    
  }  

  /** Get a list of ResourceTypes */
  private def OkTypeLinksResult(org: UUID) = {
    Ok(Output.renderLinks(TypeFactory.findAll(ResourceIds.ResourceType, org)))
  }  
  
  /** Get a single ResourceType by ID */
  private def OkTypeByIdResult(org: UUID, id: UUID) = {
    TypeFactory.findById(id) match {
      case Some(t) => Ok(Output.renderResourceTypeOutput(t))
      case None    => NotFoundResult(Errors.TYPE_NOT_FOUND(id.toString))
    }
  }
  
  /** Convert GestaltResourceTypeInut to GestaltResourceType */
  private def typeFromInput(org: UUID, owner: UUID, r: GestaltResourceTypeInput) = Try {

    val ownerLink = ResourceOwnerLink(ResourceIds.User, owner)
    GestaltResourceType(
        id = r.id.getOrElse(UUID.randomUUID),
        typeId = ResourceIds.ResourceType,
        extend = r.extend,
        state = r.resource_state.getOrElse(ResourceState.id(ResourceStates.Active)),
        orgId = org,
        owner = ownerLink,
        name = r.name,
        description = r.description,
        created = None, modified = None,
        properties = r.properties, 
        variables = r.variables, 
        tags = r.tags, 
        auth = r.auth)
  }

  private def safeGetTypeJson(json: JsValue): Try[GestaltResourceTypeInput] = Try {
    json.validate[GestaltResourceTypeInput].map{
      case resource: GestaltResourceTypeInput => resource
    }.recoverTotal{
      e => throw new BadRequestException(JsError.toFlatJson(e).toString)
    }
  }
  

  def getPropertySchema(org: UUID, typeId: UUID) = Authenticate(org) { implicit request =>
    getSchemaResult(typeId)
  }
  
  def getPropertySchemaFqon(fqon: String, typeId: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon) match {
      case Some(org) => getSchemaResult(typeId)
      case None => OrgNotFound(fqon)
    }
  }
  
  def getSchemaResult(typeId: UUID) = TypeFactory.findById(typeId) match {
    case Some(tpe) => {
      val ps = Properties.getTypeProperties(typeId)   
      if (ps.isEmpty) Ok(s"There are no properties defined for type: ${tpe.name}")
      else Ok(renderTypePropertySchema(ps))
    }
    case None => NotFoundResult(s"ResourceType ID '$typeId' not found.")
  }    
  
  
  
  private def maxwidth(ps: Seq[GestaltTypeProperty]) = {
    ps map { _.name.size } reduceLeft( _ max _)
  }
  
  private def typename(tpe: UUID) = {
    DataType.name(tpe) match {
      case arr if arr.endsWith("::list") => {
        val dt = arr.take(arr.indexOf(":"))
        s"array_${dt}"
      }
      case ref if ref.contains("::uuid") => "string_uuid"
      case "uuid" => "string_uuid"
      case "json" => "json_object"        
      case other => other
    }
  }
  
  private def renderTypePropertySchema(ps: Seq[GestaltTypeProperty]) = {
    val buf = new StringBuilder
    val namewidth = maxwidth(ps)
    val typewidth = 13
    
    buf append "[properties]:\n"
    ps foreach { p =>   
      val required = if (p.isRequired) ": [required]" else ""
      buf append s"\t%-${namewidth}s : %-${typewidth}s %s".format(p.name, typename(p.datatype), required) + "\n"
    }
    buf toString
  }  
  
}
