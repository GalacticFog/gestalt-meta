package controllers

import java.util.UUID

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds

import controllers.util.BadRequestResult
import controllers.util.GenericErrorResult
import controllers.util.trace
import play.api.mvc.RequestHeader
import com.galacticfog.gestalt.meta.auth.Authorization

object SearchController extends Authorization {
  
 case class Criterion(name: String, value: String)
 
 def AuthFqon(fqon: Option[String]) = new GestaltFrameworkAuthActionBuilder(Some({rh: RequestHeader => fqon}))
 
  def getAllResourcesByTypeOrg(org: UUID, typeId: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    Ok(Output.renderLinks(ResourceFactory.findAll(typeId, org)))
  }
  
  def getAllResourcesByTypeFqon(fqon: String, typeId: UUID) = AuthFqon(Some(fqon))/*GestaltFrameworkAuthAction(Some(fqon))*/ { implicit request =>
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)  
      case Some(org) => Ok(Output.renderLinks(ResourceFactory.findAll(typeId, org.id)))
    }     
  }

  /* /orgs/:org-id/resourcetypes/:type-id/resources/search?... */
  def getAllResourcesByTypePropertyOrg(org: UUID, typeId: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"getAllResourcesByTypeOrg($org)")
    
    val qs = request.queryString
    extractNameValue(qs) match {
      case Success((name,value)) => {
        Ok(Output.renderLinks(getResourceByPropertyOrg(org, ResourceIds.User, Criterion(name,value))))
      }
      case Failure(error) => error match {
        case ill: IllegalArgumentException => BadRequestResult(ill.getMessage)
        case _ => GenericErrorResult(500, error.getMessage)
      }
    }
  }

  /* /:fqon/resourcetypes/:type-id/resources/search?... */
  def getAllResourcesByTypePropertyFqon(fqon: String, typeId: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getAllResourcesByTypeFqon($fqon)")
    
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)
      case Some(org) => {
        val qs = request.queryString
        extractNameValue(qs) match {
          case Success((name,value)) => {
            Ok(Output.renderLinks(getResourceByPropertyOrg(org.id, ResourceIds.User, Criterion(name,value))))
          }
          case Failure(error) => error match {
            case ill: IllegalArgumentException => BadRequestResult(ill.getMessage)
            case _ => GenericErrorResult(500, error.getMessage)
          }
        }
      }
    }
  }  
  
  
  def getResourceByProperty(typeId: UUID, crtn: Criterion) = {
    trace(s"getResourceByProperty()")
    if (crtn.name == "name") ResourceFactory.findAllByName(typeId, crtn.value)
    else ResourceFactory.findAllByPropertyValue(typeId, crtn.name, crtn.value)
  }
  
  def getResourceByPropertyOrg(org: UUID, typeId: UUID, crtn: Criterion) = {
    trace(s"getResourceByPropertyOrg()")
    if (crtn.name == "name") ResourceFactory.findAllByName(org, typeId, crtn.value)
    else ResourceFactory.findAllByPropertyValueOrg(org, typeId, crtn.name, crtn.value)
  }
  
  
  def getUserByPropertyOrgId(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"getUserByPropertyOrgId($org)")

    val qs = request.queryString

    validateUserSearchCriteria(qs) match {
      case Success((name,value)) => {
        // TODO: workaround until passthrough to gestalt-security is implemented
        // Ok(Output.renderLinks(getResourceByPropertyOrg(org, ResourceIds.User, Criterion(name,value))))
        Ok(Output.renderLinks(getResourceByProperty(ResourceIds.User, Criterion(name,value))))
      }
      case Failure(error) => error match {
        case ill: IllegalArgumentException => BadRequestResult(ill.getMessage)
        case _ => GenericErrorResult(500, error.getMessage)
      }
    }
  }

  def getUserByPropertyFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getUserByPropertyFqon($fqon)")
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)
      case Some(org) => {

        val qs = request.queryString
        validateUserSearchCriteria(qs) match {
          case Success((name,value)) => {
            // TODO: workaround until passthrough to gestalt-security is implemented
            // Ok(Output.renderLinks(getResourceByPropertyOrg(org.id, ResourceIds.User, Criterion(name,value))))
            Ok(Output.renderLinks(getResourceByProperty(ResourceIds.User, Criterion(name,value))))
          }
          case Failure(error) => error match {
            case ill: IllegalArgumentException => BadRequestResult(ill.getMessage)
            case _ => GenericErrorResult(500, error.getMessage)
          }
        }
        
      }
    }
    
  }

  def getUserByPropertyGlobal() = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>
    trace(s"getUserByProperty()")

    val qs = request.queryString

    validateUserSearchCriteria(qs) match {
      case Success((name,value)) => {
        Ok(Output.renderLinks(getResourceByProperty(ResourceIds.User, Criterion(name,value))))
      }
      case Failure(error) => error match {
        case ill: IllegalArgumentException => BadRequestResult(ill.getMessage)
        case _ => GenericErrorResult(500, error.getMessage)
      }
    }
  }

  def getGroupByPropertyGlobal() = GestaltFrameworkAuthAction(nullOptString(None)) { implicit request =>
    trace(s"getGroupByProperty()")

    val qs = request.queryString

    validateGroupSearchCriteria(qs) match {
      case Success((name,value)) => {
        Ok(Output.renderLinks(getResourceByProperty(ResourceIds.Group, Criterion(name,value))))
      }
      case Failure(error) => error match {
        case ill: IllegalArgumentException => BadRequestResult(ill.getMessage)
        case _ => GenericErrorResult(500, error.getMessage)
      }
    }
  }

  def getGroupByPropertyOrgId(org: UUID) = GestaltFrameworkAuthAction(Some(org)) { implicit request =>
    trace(s"getGroupByPropertyOrgId($org)")

    val qs = request.queryString

    validateGroupSearchCriteria(qs) match {
      case Success((name,value)) => {
        // TODO: workaround until passthrough to gestalt-security is implemented
        // Ok(Output.renderLinks(getResourceByPropertyOrg(org, ResourceIds.Group, Criterion(name,value))))
        Ok(Output.renderLinks(getResourceByProperty(ResourceIds.Group, Criterion(name,value))))
      }
      case Failure(error) => error match {
        case ill: IllegalArgumentException => BadRequestResult(ill.getMessage)
        case _ => GenericErrorResult(500, error.getMessage)
      }
    }
  }

  def getGroupByPropertyFqon(fqon: String) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    trace(s"getGroupByPropertyFqon($fqon)")
    orgFqon(fqon) match {
      case None => OrgNotFound(fqon)
      case Some(org) => {

        val qs = request.queryString
        validateGroupSearchCriteria(qs) match {
          case Success((name,value)) => {
            // TODO: workaround until passthrough to gestalt-security is implemented
            // Ok(Output.renderLinks(getResourceByPropertyOrg(org.id, ResourceIds.Group, Criterion(name,value))))
            Ok(Output.renderLinks(getResourceByProperty(ResourceIds.Group, Criterion(name,value))))
          }
          case Failure(error) => error match {
            case ill: IllegalArgumentException => BadRequestResult(ill.getMessage)
            case _ => GenericErrorResult(500, error.getMessage)
          }
        }

      }
    }

  }

  private def extractNameValue(qs: Map[String, Seq[String]]) = Try {
    val key = qs.keys.toList
    if (key.isEmpty) badRequest(s"Must provide a search term.")
    if (key.size > 1) badRequest(s"Must provide a SINGLE search term.")
    val name = key(0)
    
    val value = qs(name)
    if (value.size > 1) badRequest(s"Must provide a SINGLE value for ${name}")
    if (value(0).isEmpty()) badRequest(s"Must provide a value for ${name}")
    
    (name, value(0))
  }
  
  private def validateUserSearchCriteria(qs: Map[String, Seq[String]]) = Try {
    val good = List("name", "email", "phoneNumber")
    val key = qs.keys.toList
    if (key.isEmpty) badRequest(s"Must provide a search term. One of : ${good.mkString(",")}")
    if (key.size > 1) badRequest(s"Must provide a SINGLE search term. One of : ${good.mkString(",")}")
    if (!good.contains(key(0))) badRequest(s"Unknown search term '${key(0)}. Valid terms: ${good.mkString(",")}")
    val name = key(0)
    
    val value = qs(name)
    
    // NOTE: value is never 'empty' - contains a Buffer()
    //if (value.isEmpty) badRequest(s"Must provide a value for ${name}")
    if (value.size > 1) badRequest(s"Must provide a SINGLE value for ${name}")
    if (value(0).isEmpty()) badRequest(s"Must provide a value for ${name}")
    (name, value(0))
  }

  private def validateGroupSearchCriteria(qs: Map[String, Seq[String]]) = Try {
    val good = List("name")
    val key = qs.keys.toList
    if (key.isEmpty) badRequest(s"Must provide a search term. One of : ${good.mkString(",")}")
    if (key.size > 1) badRequest(s"Must provide a SINGLE search term. One of : ${good.mkString(",")}")
    if (!good.contains(key(0))) badRequest(s"Unknown search term '${key(0)}. Valid terms: ${good.mkString(",")}")
    val name = key(0)

    val value = qs(name)

    // NOTE: value is never 'empty' - contains a Buffer()
    //if (value.isEmpty) badRequest(s"Must provide a value for ${name}")
    if (value.size > 1) badRequest(s"Must provide a SINGLE value for ${name}")
    if (value(0).isEmpty()) badRequest(s"Must provide a value for ${name}")
    (name, value(0))
  }

  def badRequest(message: String) = throw new BadRequestException(message)

}