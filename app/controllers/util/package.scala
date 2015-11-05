package controllers

import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.models.{ResourceLink => GestaltLink}
import scala.util.{Try,Success,Failure}
import play.api.{Logger => log}
import play.api.mvc._
import play.api.mvc.Results._
import play.api.mvc.Action
import play.api.mvc.Controller

//import com.galacticfog.gestalt.meta.services._

import com.galacticfog.gestalt.meta.api._

import play.api.libs.json._

import com.galacticfog.gestalt.meta.api.output._

package object util {

  def renderLinks(rs: Seq[GestaltResourceInstance]) = {
    Json.prettyPrint(Json.toJson(rs map { toLink(_) }))
  }
  
  def toLink(r: GestaltResourceInstance) = {
    GestaltLink(r.typeId, r.id.toString, Some(r.name), Some(toHref( r )))
  }
  
  def toHref(r: GestaltResourceInstance) = {
    "http://dummy_host/orgs/%s/%s/%s".format(r.orgId, "{typename}", r.id)
  }  
  
  def toError(code: Int, message: String) = Json.prettyPrint {
    Json.parse(s"""{ "code": ${code}, "message": "${message}" }""")
  }    
//  def in2domain[T](org: UUID, in: GestaltResourceInput)(implicit request: SecuredRequest[T]) = {
//    GestaltResourceInstance(
//      id = if (in.id.isDefined) in.id.get else UUID.randomUUID,
//      typeId = ResourceType.id(in.resource_type),
//      orgId = org,
//      owner = ResourceOwnerLink(ResourceIds.User, request.identity.account.id),
//      name = in.name,
//      description = in.description,
//      properties = in.properties,
//      variables = in.variables,
//      tags = in.tags,
//      auth = in.auth)
//  }  
  
  
  abstract class TryHandler[A,B](success: A => B)(failure: Throwable => B) {
    def handle(in: Try[A]) = in match {
      case Success(out) => success(out)
      case Failure(err) => failure(err)
    }
  }
  
  private def trySuccess(in: String) = Ok(in)
  private def trySuccessNoResult(in: Unit) = Ok("")
  
  private def tryNotFoundFailure(err: Throwable) = {    
    log.debug("***tryNotFoundFailure : " + err.getMessage)
    err match {
      case rnf: ResourceNotFoundException => {
        log.info( rnf.getMessage )
        NotFound( rnf.toErrorString )
      }
      case unknown => {
        log.error( err.getMessage )
        InternalServerError( err.getMessage )
      }
    }
  }
  
  
  
  private def securedGetListFailure(err: Throwable) = {
    err match {
      case unauth: UnauthorizedException => {
        log.info( unauth.getMessage )
        Unauthorized( unauth.toErrorString )
      }
    }
  }
  
  class OkNotFoundNoResultHandler 
    extends TryHandler[Unit,Result](trySuccessNoResult)(tryNotFoundFailure)
  
  class OkNotFoundHandler 
    extends TryHandler[String,Result](trySuccess)(tryNotFoundFailure)
  
  def okNotFound(f: Try[String]) = new OkNotFoundHandler().handle( f )
  def okNotFoundNoResult(f: Try[Unit]) = new OkNotFoundNoResultHandler().handle( f )

  
//  abstract class ansi(text: String) {
//    val code: String
//    def colorize(code: String) = s"\u001B[${code}m${text}\u001B[0m"
//    override def toString = colorize(code)
//  }
//  
//  case class red(text: String) extends ansi(text) {
//    override val code = "0;31"
//  }
//  case class blue(text: String) extends ansi(text) {
//    override val code = "0;34"
//  }
//  case class green(text: String) extends ansi(text) {
//    override val code = "0;32"
//  }  
  
}