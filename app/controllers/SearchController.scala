package controllers


import java.util.UUID

import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.output.Output
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import controllers.util.{BadRequestResult, GenericErrorResult, HandleExceptions, SecureController}
import play.api.mvc.RequestHeader
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import play.api.i18n.MessagesApi

import javax.inject.Singleton

@Singleton
class SearchController @Inject()(messagesApi: MessagesApi,
                                 env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  private case class Criterion(name: String, value: String)
  
  def getAllResourcesByTypeFqon(fqon: String, typeId: UUID) = Audited(fqon) { implicit request =>
    handleExpansion(ResourceFactory.findAll(typeId, fqid(fqon)),
      request.queryString, META_URL)     
  }

  // GET /{fqon}/resourcetypes/{typeId}/resources/search?... 
  def getAllResourcesByTypePropertyFqon(fqon: String, typeId: UUID) = GestaltFrameworkAuthAction(Some(fqon)) { implicit request =>
    extractNameValue(request.queryString) match {
      case Failure(error)        => HandleExceptions(error)
      case Success((name,value)) => handleExpansion(
        getByProperty(ResourceIds.User, Criterion(name,value)),
        request.queryString, META_URL)
    }
  }

  // GET /users/search?{name}={value}
  def getUserByPropertyGlobal() = Audited() { implicit request =>
    getResourcesByProperty(ResourceIds.User)(validateUserSearchCriteria)
  }

  // GET /users/search?{name}={value}
  def getGroupByPropertyGlobal() = Audited() { implicit request =>
    getResourcesByProperty(ResourceIds.Group)(validateGroupSearchCriteria)
  }

  // GET /{fqon}/users/search?{name}={value}  
  def getUserByPropertyFqon(fqon: String) = Audited(fqon) { implicit request =>
    getResourcesByProperty(
        ResourceIds.User, Option(fqid(fqon)))(validateUserSearchCriteria)
  }  

  // GET /{fqon}/users/search?{name}={value}  
  def getGroupByPropertyFqon(fqon: String) = Audited(fqon) { implicit request =>
    getResourcesByProperty(
        ResourceIds.Group, Option(fqid(fqon)))(validateGroupSearchCriteria)
  }

  
  private[controllers] def getResourcesByProperty(typeId: UUID, org: Option[UUID] = None)
      (f: Map[String, Seq[String]] => Try[(String,String)])(implicit request: SecuredRequest[_]) = {
    f(request.queryString) match {
      case Failure(error)        => HandleExceptions(error)
      case Success((name,value)) => {
        handleExpansion(getByProperty(typeId, Criterion(name,value), org), 
         request.queryString, META_URL)
      }
    }    
  }
  
  private[controllers] def getByProperty(typeId: UUID, crtn: Criterion, org: Option[UUID] = None) = {
    if (crtn.name == "name") {
      org.fold(ResourceFactory.findAllByName(typeId, crtn.value)) { oid =>
        ResourceFactory.findAllByName(org.get, typeId, crtn.value)
      }
    } else {
      org.fold(ResourceFactory.findAllByPropertyValue(typeId, crtn.name, crtn.value)) { oid =>
        ResourceFactory.findAllByPropertyValueOrg(oid, typeId, crtn.name, crtn.value)
      }
    }
  }
  
  /*
   * TODO: Factor these three functions into one.
   */
  private def extractNameValue(qs: Map[String, Seq[String]]) = Try {
    val key = qs.keys.toList
    if (key.isEmpty)  badRequest(s"Must provide a search term.")
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
    if (value.size > 1) badRequest(s"Must provide a SINGLE value for ${name}")
    if (value(0).isEmpty()) badRequest(s"Must provide a value for ${name}")
    (name, value(0))
  }

  private def badRequest(message: String) = throw new BadRequestException(message)

}