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
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity, GestaltFrameworkSecurityEnvironment, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import scala.language.postfixOps
import com.galacticfog.gestalt.json.Js
import javax.inject.Singleton
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.data.ResourceSelector
import com.galacticfog.gestalt.meta.providers.ui.Assembler
import com.galacticfog.gestalt.meta.providers._

import scala.util.{Failure, Success, Try}
import controllers.util.Security
import com.galacticfog.gestalt.security.api.{GestaltAPICredentials, GestaltAccount, GestaltDirectory}
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.GestaltOrgSync
import com.galacticfog.gestalt.security.api.GestaltResource
import com.galacticfog.gestalt.security.api.json.JsonImports._
import com.mohiva.play.silhouette.api.actions.SecuredRequest
import play.api.cache.CacheApi
import play.api.mvc.RequestHeader
import scala.concurrent.duration._


@Singleton
class TypeController @Inject()(
                                messagesApi: MessagesApi,
                                sec: GestaltFrameworkSecurity,
                                security: Security,
                                cache: CacheApi)
  extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {

  case class SchemaEntry(name: String, datatype: String, required: Option[Boolean])
  object SchemaEntry {
    implicit lazy val schemaEntryFormat = Json.format[SchemaEntry]
  }


  def getAllResourceTypesFqon(fqon: String) = Audited(fqon) { implicit request =>
    log.info("Got a request to allResourceTypes")
    orgFqon(fqon).fold(NotFoundResult(Errors.ORG_NOT_FOUND(fqon))) { org =>
      val qs = request.queryString

      val tpes = {
        if (qs.contains("name") && qs.contains("type")) {
          throw BadRequestException(
            "'name' and 'type' query params are not supported simultaneously. Supply one or the other")
        }
        else if (qs.contains("name")) {
          log.info("Trying to get a value from cache")
          cache.getOrElse[Seq[GestaltResourceType]]("name=" + qs) {
            log.info("No value found in cache for name=" + qs)
            val result = findNamedTypes(qs)
            cache.set("name=" + qs, result, 10.minutes)
            result
          }
        }
        else if (qs.contains("type")) {
          log.info("Trying to get a value from cache")
          cache.getOrElse[Seq[GestaltResourceType]]("type=" + qs) {
            log.info("No value found in cache for type=" + qs)
            val result = findCovariantTypes(qs)
            cache.set("type=" + qs, result, 10.minutes)
            result
          }
        }
        else {
          log.info("Trying to get a value from cache")
          cache.getOrElse[Seq[GestaltResourceType]]("id=" + org.id) {
            log.info("No value found in cache for id=" + org.id)
            val result = TypeFactory.findAll(ResourceIds.ResourceType, org.id)
            cache.set("id=" + org.id, result, 10.minutes)
            result
          }
        }
      }
      Ok(handleExpandType(tpes, qs, META_URL))
    }
  }

  /**
    * Get a Provider Action specification from the Provider. This is a convenience method that can be used to get
    * an Action spec before an Action instance is created. The intended use is to test rendering of the Action UI.
    */
  def getProviderActionByIndex(fqon: String, typeId: UUID, actionIndex: Int) = Audited(fqon){ implicit request =>
    log.info(s"getProviderActionByIndex($fqon, $typeId)")
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
        val qs = request.queryString
        if (QueryString.singleBoolean(qs, "render")) {
          log.debug("Found 'render' query param.")

          if (QueryString.singleBoolean(qs, "envelope")) {
            val out = Assembler.envelope(fqon, META_URL, None, request.identity)
            Ok(out).as("text/html")
          } else {
            Js.parse[ProviderActionSpec](action) match {
              case Failure(e) => HandleExceptions {
                throw new RuntimeException(s"Could not parse action JSON from provider. Error: ${e.getMessage}")
              }
              case Success(spec) => {
                log.debug("Found 'envelope' query param.")
                val output = Assembler.assemble(fqon, META_URL, spec, None, None, request.identity)
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
    log.info(s"getResourceTypeByIdFqon($fqon, $id)")
    OkTypeByIdResult(fqid(fqon), id, request.queryString)
  }

  def createResourceTypeFqon(fqon: String) = AsyncAudited(fqon) { implicit request =>
    TypeMethods.validateCreatePayload(request.body) match {
      case Failure(e) => HandleExceptionsAsync(e)
      case Success(payload) => {
        if (QueryString.singleBoolean(request.queryString, "test")) {
          Future.successful(Ok(payload))
        } else {
          CreateTypeWithPropertiesResult(fqid(fqon), payload, request.identity)
        }
      }
    }
  }

  private[controllers] def findCovariantTypes(qs: Map[String, Seq[String]]): Seq[GestaltResourceType] = {
    if (qs.get("type").isEmpty) List.empty
    else if (qs("type").size > 1) {
      throw new BadRequestException("Query parameter 'type' may only be given once. Found multiple.")
    } else {
      TypeMethods.typeId(qs("type").head).fold {
        throw new ResourceNotFoundException(s"Type with ID '${qs("type")}' not found.")
      } { tid =>
        ResourceSelector.findTypesWithVariance(CoVariant(tid))
      }
    }
  }

  private[controllers] def findNamedTypes(qs: Map[String, Seq[String]]): Seq[GestaltResourceType] = {
    qs("name").toSeq.flatMap(TypeFactory.findByName(_))
  }

  private[controllers] def CreateTypeWithPropertiesResult(org: UUID, typeJson: JsValue, caller: AuthAccountWithCreds) = {
    Future {
      createTypeWithProperties(org, typeJson, caller) match {
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
  private[controllers] def deconstructType(org: UUID, owner: UUID, typeJson: JsValue)():
  (GestaltResourceType, Option[Seq[GestaltTypeProperty]]) = {

    val input: GestaltResourceTypeInput = safeGetTypeJson(typeJson).get
    val domain: GestaltResourceType = typeFromInput(org, owner, input).get

    val definitions: Option[Seq[GestaltTypeProperty]] =
      input.property_defs map { ps => ps map { p =>
        PropertyController.propertyFromInput(org, owner, p.copy(applies_to = Some(domain.id))).get }
      }
    (domain, definitions)
  }

  private[controllers] def getRootAccountWithCreds(): AuthAccountWithCreds = {

    val securityJs = Json.toJson {
      security.getOrgSyncTree2() match {
        case Failure(e) =>
          throw new RuntimeException("Failed retrieving data from gestalt-security: " + e.getMessage)
        case Success(t) => t
      }
    }.as[JsObject]

    val adminId: UUID = {
      Js.find(securityJs, "/admin/id").fold {
        throw new RuntimeException("Could not parse admin id from security info.")
      }{ id =>
        UUID.fromString(id.as[String])
      }
    }
    val rootAccount  = findSecurityObject[GestaltAccount](securityJs, "/accounts", adminId)
    val directoryOrg = findSecurityObject[GestaltOrg](securityJs, "/orgs", rootAccount.directory.orgId)

    TypeMethods.makeAccount(directoryOrg, rootAccount)
  }


  private def findSecurityObject[A <: GestaltResource](json: JsObject, path: String, target: UUID)(implicit ra: Reads[A]): A = {
    val found = for {
      obj <- Js.find(json,path)
      as = Js.parse[Seq[A]](obj) match {
        case Failure(e) => throw new RuntimeException("Failed parsing accounts: " + e.getMessage,e)
        case Success(as) => as
      }
      a <- as.find(_.id == target)
    } yield a
    found getOrElse {
      throw new RuntimeException("Could not find admin account in security info.")
    }
  }

  private[this] lazy val root: AuthAccountWithCreds = try {
    getRootAccountWithCreds()
  } catch {
    case e: Throwable => {
      log.error("Failed looking up root user : " + e.getMessage)
      throw e
    }
  }


  /**
    * Add a new child type to a parent at the type level. This enables child-type to be a 'childOf'
    * the parent type in the schema (child-type is now a child of each given parent-type).
    */
  private[controllers] def updateParentLineage(
                                                caller: UUID,
                                                childType: GestaltResourceType,
                                                parents: Seq[UUID]): Try[Unit] = Try {

    // Update the parent-types with this new child type
    val results = TypeMethods.makeNewParents(caller, childType.id, parents)
    val errors = results.collect { case Failure(e) => e.getMessage }

    if (errors.nonEmpty) {
      val msg = errors.flatten.mkString(",")
      throw new RuntimeException(s"There were errors updating parent-type schemas: " + msg )
    }
  }

  /**
    * Set entitlements for a child-type on all instances of the given parent IDs.
    */
  private[controllers] def updateParentEntitlements(
                                                     caller: AuthAccountWithCreds,
                                                     childType: UUID,
                                                     parents: Seq[UUID]): Try[Unit] = Try {


    val t = parents.foldLeft(Try(())) { (_, parent) =>
      TypeMethods.updateInstanceEntitlements(parent, childType, root, caller, None) match {
        case Left(errs) =>
          throw new RuntimeException("There were errors setting instance entitlements: " + errs.mkString(","))
        case Right(_) => Success(())
      }
    }
  }

  private[controllers] def createTypeWithProperties(
                                                     org: UUID,
                                                     typeJson: JsValue,
                                                     caller: AuthAccountWithCreds): Try[GestaltResourceType] = {

    Try {
      val owner = caller.account.id

      log.debug("Converting input types to domain...")
      val (domain, propdefs) = deconstructType(org, owner, typeJson)

      log.debug("Creating new ResourceType...")
      val newtype = TypeFactory.create(owner)(domain) match {
        case Success(s) => s
        case Failure(e) =>
          log.error("Error creating new ResourceType",e)
          throw e
      }

      log.debug("Checking for TypeProperties...")
      if (propdefs.isDefined) {
        for (prop <- propdefs.get) {
          log.debug(s"Creating TypeProperty : ${prop.name}")
          PropertyFactory.create(owner)(prop).get
        }
      }

      log.debug("Setting type lineage...")
      /*
       * Get list of types declared as parent types.
       * Add current type to child list of each parent type.
       * 
       * 1.) Create type - get type-ID
       * 2.) Update all parent types /properties/lineage/child_types
       * 3.) Update all instances of all parent-types with entitlements for the new type.
       */

      // Get a list of all parent-type IDs
      val newParentTypes = TypeMethods.getParentTypes(newtype)

      val results = for {
        _ <- updateParentLineage(owner, newtype, newParentTypes)
        a <- updateParentEntitlements(caller, newtype.id, newParentTypes)
      } yield a

      results match {
        case Failure(e) => throw e
        case Success(_) => newtype
      }
    }
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
  private def OkTypeByIdResult(org: UUID, id: UUID, qs: Map[String, Seq[String]])(
    implicit request: RequestHeader) = {

    TypeFactory.findById(id).fold(NotFoundResult(Errors.TYPE_NOT_FOUND(id.toString))) { typ =>
      TypeMethods.renderType(typ, request.queryString, META_URL) match {
        case Failure(e) => HandleExceptions(e)
        case Success(jsonType) => Ok(jsonType)
      }
    }
  }

  /**
    * Convert GestaltResourceTypeInut to GestaltResourceType
    */
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



  private def safeGetTypeJson(json: JsValue): Try[GestaltResourceTypeInput] = Try {
    json.validate[GestaltResourceTypeInput].map{
      case resource: GestaltResourceTypeInput => resource
    }.recoverTotal{
      e => throw new BadRequestException(Js.errorString(e))
    }
  }


  def getPropertySchemaFqon(fqon: String, typeId: UUID) = Audited(fqon) { implicit request =>
    getSchemaResult(typeId, request.queryString)
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

  def getSchemaResult(typeId: UUID, qs: Map[String, Seq[String]]) = {
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


  private def renderTypePropertySchema(ps: Seq[GestaltTypeProperty], qs: Map[String, Seq[String]]) = {

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



}
