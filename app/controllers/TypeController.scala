package controllers


import play.api.Logger
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

import play.api.libs.concurrent.Execution.Implicits.defaultContext
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
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.data.ResourceSelector




@Singleton
class TypeController @Inject()(messagesApi: MessagesApi,
                               env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  private[this] val log = Logger(this.getClass)

  case class SchemaEntry(name: String, datatype: String, required: Option[Boolean])  
  object SchemaEntry {
    implicit lazy val schemaEntryFormat = Json.format[SchemaEntry]
  }


  def getAllResourceTypesFqon(fqon: String) = Audited(fqon) { implicit request =>
    
    orgFqon(fqon).fold(NotFoundResult(Errors.ORG_NOT_FOUND(fqon))) { org =>
      val qs = request.queryString
      
      val tpes = {
        if (qs.contains("name") && qs.contains("type")) {
          throw new BadRequestException(
              "'name' and 'type' query params are not supported simultaneously. Supply one or the other")
        }
        else if (qs.contains("name")) findNamedTypes(qs)
        else if (qs.contains("type")) findCovariantTypes(qs)
        else TypeFactory.findAll(ResourceIds.ResourceType, org.id)
      }
      handleExpansion(tpes, qs, META_URL)
    }
  }  
  
  import controllers.util.booleanParam
  import com.galacticfog.gestalt.meta.providers.ui.Assembler
  import com.galacticfog.gestalt.meta.providers._
  import scala.util.{Try,Success,Failure}
  

  /**
   * Get a Provider Action specification from the Provider. This is a convenience method that can be used to get
   * an Action spec before an Action instance is created. The intended use is to test rendering of the Action UI.
   */
  def getProviderActionByIndex(fqon: String, typeId: UUID, actionIndex: Int) = Audited(fqon){ implicit request =>
    TypeFactory.findById(typeId).fold {
      HandleExceptions {
        throw new ResourceNotFoundException(
          s"Resource-Type with ID '$typeId' not found.")
      }
    }{ r =>
      val jtype = Json.toJson(Output.renderResourceTypeOutput(r)).as[JsObject]
      Js.find(jtype, s"/properties/provider_actions/$actionIndex").fold {
        HandleExceptions {
          throw new ResourceNotFoundException(
            s"/properties/provider_actions/$actionIndex not found in Resource Type $typeId")
        }
      }{ action =>
        if (booleanParam("render", request.queryString)) {
          log.debug("Found 'render' query param.")
          
          if (booleanParam("envelope", request.queryString)) {
            val out = Assembler.envelope(fqon, META_URL.get, None, request.identity)
            Ok(out).as("text/html")
          } else {
            Js.parse[ProviderActionSpec](action) match {
              case Failure(e) => HandleExceptions {
                throw new RuntimeException(s"Could not parse action JSON from provider. Error: $e.getMessage")
              }
              case Success(spec) => {
                log.debug("Found 'envelope' query param.")
                val output = Assembler.assemble(fqon, META_URL.get, spec, None, None, request.identity)
                Ok(output).as("text/html")              
              }
            }
          }

        } else {
          Ok(action)
        }
      }
    }
  }
  
  def getResourceTypeByIdFqon(fqon: String, id: UUID) = Audited(fqon) { implicit request =>
    OkTypeByIdResult(fqid(fqon), id)
  }
  
  def createResourceTypeFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    CreateTypeWithPropertiesResult(fqid(fqon), request.body)  
  }
  
  override def handleExpansion(rs: Seq[ResourceLike], qs: QueryString, baseUri: Option[String] = None) = {
    if (getExpandParam(qs)) {
      expandOutput(rs.asInstanceOf[Seq[GestaltResourceType]], qs, baseUri)(Output.renderResourceTypeOutput)
    }
    else Ok(Output.renderLinks(rs, baseUri))
  }  
  
  private[controllers] def findCovariantTypes(qs: QueryString): Seq[GestaltResourceType] = {
    if (qs.get("type").isEmpty) List.empty
    else if (qs("type").size > 1) {
      throw new BadRequestException("Query parameter 'type' may only be given once. Found multiple.")
    } else {
      typeId(qs("type").head).fold {
        throw new ResourceNotFoundException(s"Type with ID '${qs("type")}' not found.")
      } { tid =>
        ResourceSelector.findTypesWithVariance(CoVariant(tid))
      }    
    }
  }
  
  private[controllers] def findNamedTypes(qs: QueryString): Seq[GestaltResourceType] = {
    qs("name").toSeq.flatMap(TypeFactory.findByName(_))     
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
  
  def getPropertySchemaFqon(fqon: String, typeId: UUID) = Audited(fqon) { implicit request =>
    getSchemaResult(typeId, request.queryString)
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
  
  def getSchemaResult(typeId: UUID, qs: QueryString) = {
    TypeFactory.findById(typeId).fold{
      NotFoundResult(s"ResourceType ID '$typeId' not found.")
    }{ typ =>

      val ps = Properties.getTypeProperties(typeId)
      
      val result = {
        if (ps.isEmpty) s"[]"
        else if (qs.contains("filter") && qs("filter").contains("config"))
          renderProviderConfiguration(typ)
        else 
          renderTypePropertySchema(ps, qs)      
      }
      Ok(Json.parse(result))
    }
  }
  
  
  def renderProviderConfiguration(typ: GestaltResourceType): String = {
    val emptyArray = "[]"
    
    if (typ.properties.isDefined) {
      typ.properties.get.get("config_schema") match {
        case None => emptyArray
        case Some(v) => {
          Js.find(Json.parse(v).as[JsObject], "/entries") match {
            case None => emptyArray
            case Some(js) => Json.prettyPrint(js)
          }
        }
      }
    } else emptyArray
  }
  
  
  private def renderTypePropertySchema(ps: Seq[GestaltTypeProperty], qs: QueryString) = {
    
    def loop(ps: Seq[GestaltTypeProperty], acc: Seq[SchemaEntry]): Seq[SchemaEntry] = {
      ps match {
        case Nil => acc
        case h :: t => {
          val required = if (h.isRequired) Some(true) else Some(false)
          loop(t, SchemaEntry(h.name, typename(h.datatype), required) +: acc)
        }
      }
    }
    
    val entries = loop(ps, Seq.empty)
    Json.prettyPrint(Json.toJson(entries))
    
//    val buf = new StringBuilder
//    val namewidth = maxwidth(ps)
//    val typewidth = 13
//    
//    buf append "[properties]:\n"
//    ps foreach { p =>   
//      val required = if (p.isRequired) ": [required]" else ""
//      buf append s"\t%-${namewidth}s : %-${typewidth}s %s".format(p.name, typename(p.datatype), required) + "\n"
//    }
//    buf toString
    
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
  
  def typeId(name: String): Option[UUID] = {
    TypeFactory.findByName(name) map { _.id }
  }
  
  private def maxwidth(ps: Seq[GestaltTypeProperty]) = {
    ps map ( _.name.size ) reduceLeft ( _ max _)
  }
  
}
