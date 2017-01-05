package controllers


import play.api.Logger
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import controllers.util._
import java.util.UUID

import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import scala.language.postfixOps
import com.galacticfog.gestalt.json.Js
import javax.inject.Singleton

@Singleton
class TypeController @Inject()(messagesApi: MessagesApi,
                               env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  private[this] val log = Logger(this.getClass)

  def getAllResourceTypesFqon(fqon: String) = Authenticate(fqon) { implicit request =>
    
    orgFqon(fqon).fold(NotFoundResult(Errors.ORG_NOT_FOUND(fqon))) { org =>
      if (!request.queryString.contains("name")) 
        OkTypeLinksResult(org.id)
      else {
        val names = request.queryString("name").toSeq
        val tpes = names flatMap (TypeFactory.findByName(org.id, _))
        Ok(Output.renderLinks(tpes))
      }
    }
  }
  
  def getResourceTypeByIdFqon(fqon: String, id: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon).fold(NotFoundResult(Errors.TYPE_NOT_FOUND(id))) { org =>
      OkTypeByIdResult(org.id, id)
    }
  }
  
  def createResourceTypeFqon(fqon: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    orgFqon(fqon).fold(Future(OrgNotFound(fqon))) { org =>
      CreateTypeWithPropertiesResult(org.id, request.body)  
    }
  }

  private def CreateTypeWithPropertiesResult[T](org: UUID, typeJson: JsValue)(implicit request: SecuredRequest[T]) = {
    Future {
      createTypeWithProperties(org, typeJson) match {
        case Failure(e) => HandleExceptions(e)
        case Success(newtype) => Created(Output.renderResourceTypeOutput( newtype ))
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
      log.debug("createTypeWithProperties => COMPLETE.")
      newtype
    }
  }
  
  
  /* DELETE /orgs/:uuid/resourcetypes/:uuid */
  def deleteResourceTypeById(org: UUID, id: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    ???    
  }  

  /** 
   * Get a list of ResourceTypes 
   */
  private def OkTypeLinksResult(org: UUID) = {
    Ok(Output.renderLinks(TypeFactory.findAll(ResourceIds.ResourceType, org)))
  }  
  
  /** 
   * Get a single ResourceType by ID 
   */
  private def OkTypeByIdResult(org: UUID, id: UUID) = {
    TypeFactory.findById(id).fold(NotFoundResult(Errors.TYPE_NOT_FOUND(id.toString))) {
      typ => Ok(Output.renderResourceTypeOutput(typ))
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
        properties = stringmap(r.properties), 
        variables = r.variables, 
        tags = r.tags, 
        auth = r.auth)
  }
  
  def getPropertySchemaFqon(fqon: String, typeId: UUID) = Authenticate(fqon) { implicit request =>
    orgFqon(fqon).fold(OrgNotFound(fqon))( _ => getSchemaResult(typeId) )
  }
  
  
  private def safeGetTypeJson(json: JsValue): Try[GestaltResourceTypeInput] = Try {
    json.validate[GestaltResourceTypeInput].map{
      case resource: GestaltResourceTypeInput => resource
    }.recoverTotal{
      e => throw new BadRequestException(Js.errorString(e))
    }
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
  
  def getSchemaResult(typeId: UUID) = {
    TypeFactory.findById(typeId).fold{
      NotFoundResult(s"ResourceType ID '$typeId' not found.")
    }{ typ =>
      val ps = Properties.getTypeProperties(typeId)
      Ok {
        if (ps.isEmpty) s"There are no properties defined for type: ${typ.name}"
        else renderTypePropertySchema(ps)      
      }
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
  
  private def typename(tpe: UUID) = {
    DataType.name(tpe) match {
      case arr if arr.endsWith("::list") => {
        val dt = arr.take(arr.indexOf(":"))
        s"array_${dt}"
      }
      case ref if ref.contains("::uuid") => "string_uuid"
      case "uuid" => "string_uuid"
      case "json" => "json_object"        
      case other  => other
    }
  }

  private def maxwidth(ps: Seq[GestaltTypeProperty]) = {
    ps map ( _.name.size ) reduceLeft ( _ max _)
  }  
}
