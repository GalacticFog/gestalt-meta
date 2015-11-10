package controllers.util


import com.galacticfog.gestalt.meta.api._

import com.galacticfog.gestalt.data.util._

import db._

import play.api.{Logger => log}

import scala.util.{Success,Failure}

import org.postgresql.util.PSQLException

import scalikejdbc._
import com.galacticfog.gestalt.data._
import scala.util.Try
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltBaseAuthProvider
import com.galacticfog.gestalt.security.play.silhouette.GestaltSecuredController

import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.impl.authenticators.{DummyAuthenticatorService, DummyAuthenticator}

import java.util.UUID

import com.galacticfog.gestalt.data.models._

import com.galacticfog.gestalt.security.api.errors.SecurityRESTException
import com.galacticfog.gestalt.security.api.errors.{ BadRequestException => SecurityBadRequestException }
import com.galacticfog.gestalt.security.api.errors.{ UnauthorizedAPIException => SecurityUnauthorizedAPIException }
import com.galacticfog.gestalt.security.api.errors.{ ForbiddenAPIException => SecurityForbiddenAPIException }
import com.galacticfog.gestalt.security.api.errors.{ ResourceNotFoundException => SecurityResourceNotFoundException }
import com.galacticfog.gestalt.security.api.errors.{ CreateConflictException => SecurityCreateConflictException }
import com.galacticfog.gestalt.security.api.errors.{ UnknownAPIException => SecurityUnknownAPIException }
import com.galacticfog.gestalt.security.api.errors.{ APIParseException => SecurityAPIParseException }


trait MetaController extends GestaltFrameworkSecuredController[DummyAuthenticator] {
  
  override def getAuthenticator: AuthenticatorService[DummyAuthenticator] = new DummyAuthenticatorService
  
  protected val connection = Session.connection
  protected def getApiPrefix() = "https://gf.com/api/v1.1"
  
  /* 
   * 'Null' functions for SecuredAuthAction - can't just pass None to function
   * since it can't figure out if it's a None:String, or None:UUID (???)
   */
  protected def nullOptString(n: Option[String]) = n
  protected def nullOptUUID(n: Option[UUID]) = n
  
  /*
   * Test connection to Meta's DataStore.
   */
  protected def verifyDataStore() = {
    MetaDS.assertOnline( onFail = System.exit(1), failMessage = "SHUTTING DOWN" )
  }
  
  /** Get an Org by FQON */
  protected[controllers] def orgFqon(fqon: String): Option[GestaltResourceInstance] = {
    ResourceFactory.findByPropertyValue(ResourceIds.Org, "fqon", fqon)
  }
  
  /** Format a 404 for 'Org Not Found' */
  protected[controllers] def OrgNotFound(orgIdentifier: String) = {
    NotFound(toError(404, Errors.ORG_NOT_FOUND(orgIdentifier)))
  }
  
 
  protected object Errors {
    def ORG_NOT_FOUND(id: String) = s"Org '${id}' not found."
    def PROPERTY_NOT_FOUND(id: String) = s"TypeProperty '${id}' not found."
    def TYPE_NOT_FOUND(id: String) = s"ResourceType '${id}' not found."
    def INVALID_RESOURCE_TYPE_ID(id: UUID) = s"Type ID must be Gestalt Org or User. Found: ${id.toString}"
  }    
  /**
   * Only return resources that the given account (user) has 'read' access to.
   */
  private def filterReads(rs: Seq[GestaltResourceInstance], account: AuthAccountWithCreds, auth: Option[Hstore]) = {
    // TODO: What do we do if there is no authorization data on a resource?
    if (auth.isEmpty) Seq()
    else {

    }
    ???
  }  


  def handleSecurityApiException(e: Throwable) = e.asInstanceOf[SecurityRESTException] match {
    case e: SecurityBadRequestException       => BadRequest(toError(400, e.getMessage))
    case e: SecurityUnauthorizedAPIException  => Unauthorized(toError(401, e.getMessage))
    case e: SecurityForbiddenAPIException     => Forbidden(toError(403, e.getMessage))
    case e: SecurityResourceNotFoundException => NotFound(toError(404, e.getMessage))
    case e: SecurityCreateConflictException   => Conflict(toError(409, e.getMessage))
    case e: SecurityUnknownAPIException       => BadRequest(toError(400, e.getMessage))
    case e: SecurityAPIParseException         => InternalServerError(toError(500, e.getMessage))
  }
  
  protected def trace(method: String) = {
    log.debug("%s::%s".format(this.getClass.getSimpleName, method))
  }
  
}


/**
 * Singleton holder for objects shared amongst Controllers.
 */
object Session {
  /*
   * JDBC Connection info for binding to Meta's DataStore.
   * Getting the connection info DOES NOT validate the data - it
   * only asserts that values were supplied for all connection attributes.
   */
  private[util] val connection = ConnectionManager.config
  
  implicit val session: DBSession = AutoSession  
}


object MetaDS extends {
  
  lazy val online = metaOnline( ConnectionManager.config )
  
  def assertOnline(onFail: => Unit, failMessage: String) = {
    if ( online ) {
      log.info("Meta Data-Store ONLINE")
    } 
    else {
      log.error("FATAL: Cannot connect to Meta Data-Store")
      log.error("Current configuration:")
      log.error(ConnectionManager.toString)
      log.error(failMessage)
      onFail
    }
  }
  
  /**
   * Ensure we can reach the server specified in configuration and
   * that the specified database exists.
   */
  def metaOnline(config: JdbcConnectionInfo): Boolean = {
    
    log.info("Pinging Meta Repository...")
    
    PostgresHealth.verifyDataStore( config.database ) match {
      case Success( _ )  => {
        log.info( "data-store: " + /*green(*/"Available"/*)*/ )
        log.info( ConnectionManager.toString )
        true
      }
      case Failure( ex ) => ex match {
        case p: PSQLException => {
          log.error( s"data-store: ${p.getMessage}" )
          false
        }
        case e: Throwable => {
          log.error( "Unexpected-Error : " + e.getMessage )
          false
        }
      }
    }  
  }

}