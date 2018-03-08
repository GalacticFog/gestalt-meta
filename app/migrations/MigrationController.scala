package migrations


import java.util.UUID

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.models.ResourceLike
import com.galacticfog.gestalt.data.string2uuid
import com.galacticfog.gestalt.data.uuid
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceInfo
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.meta.api.sdk.Resources
import com.galacticfog.gestalt.meta.api.sdk.resourceInfoFormat
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import play.api.i18n.MessagesApi
import play.api.libs.json.{JsObject, JsString, JsValue, Json}
import play.api.mvc.{Action, AnyContent, Result}
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.language.postfixOps
import javax.inject.Singleton

import com.galacticfog.gestalt.security.api.errors.ForbiddenAPIException
import play.api.Logger

/**
 * WIP: Allow administrators to manually run a meta-schema migration of a specific version.
 * Currently handles a single route:
 * 
 * ```POST /migrate?version={version}```
 * 
 * - Calling user MUST be 'root' (not yet enforced)
 * - `version` querystring param MUST be present and non-null
 * - Payload is currently empty
 */

class MigrationController @Inject()( 
    messagesApi: MessagesApi,
    env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator],
    security: Security,
    genericResourceMethods: GenericResourceMethods )
      extends SecureController(messagesApi = messagesApi, env = env) with Authorization {
  
  private[this] val log = Logger(this.getClass)
  
  /**
   * Perform a meta-schema migration. Expects querystring param '?version={version}'
   */
  def migrate() = Audited() { implicit request =>
    log.debug("migrate()")
    
    /*
     * `?version={version} querystring parameter must be present and it must have a value.
     */
    val version = QueryString.single(request.queryString, "version", strict = true) getOrElse {
      throw new BadRequestException("You must supply 'version' in the query string.")
    }
    
    executeMigration(version, request.identity.account.id) match {
      case Left(e) => InternalServerError(e)
      case Right(m) => Ok(m)
    }
  }

  /**
   * Find and perform a schema migration to the given version as the given identity.
   */
  def executeMigration(version: String, identity: UUID) = {
    lookupMigration(version).get.migrate(identity, None)
  }
  
  /**
   * Find and return a Migration component for the given version string.
   */
  private def lookupMigration(version: String): Try[MetaMigration] = Try {
    version match {
      case "V1" => new V1()
      case "V2" => new V2()
      case _ => throw new BadRequestException(s"No migration found for version '$version'")
    }
  }
  
  /*
   * TODO: Unused at the moment, but the intention is that 'migrations' may only be performed by
   * the admin/root user. This function will be used to determine who the caller is.
   */
  private[migrations] def validateAdminUser(auth: AuthAccountWithCreds) = {
    for {
      admin <- security.getRootUser(auth)
      out   <- Try {
        if (auth.account.id == admin.id) admin 
        else 
          throw new ForbiddenException(s"Insufficient permissions. POST /bootstrap may only be executed by the root/admin user.")
      }
    } yield out
  }  
  
}