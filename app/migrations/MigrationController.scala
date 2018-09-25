package migrations


import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.Materializer
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import play.api.Logger
import play.api.i18n.MessagesApi
import play.api.libs.json._

import scala.language.postfixOps
import scala.util.Try
import com.galacticfog.gestalt.json.Js

class MigrationController @Inject()( 
    messagesApi: MessagesApi,
    sec: GestaltFrameworkSecurity,
    security: Security,
    v7: V7,
    v8: V8,
    genericResourceMethods: GenericResourceMethods )
      extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
  
  def migrate() = AsyncAudited() { implicit request =>
    log.debug("migrate()")
    
    val ALL_MIGRATIONS = Seq("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16")
    
    val version = QueryString.single(request.queryString, "version", strict = true)
    val caller = request.identity.account.id

    val effectiveMigrations = version match {
      case None =>
        log.debug("No version given - running all migrations")
        ALL_MIGRATIONS
      case Some(v) =>
        log.debug(s"Running migration version '${version.get}'")
        Seq(v)
    }

    var failed = false
    val results = effectiveMigrations.map { v =>
      val args = getMigrationArgs(v, request.body)
      executeMigration(v, caller, args) match {
        case Left(j) =>
          failed = true
          Json.obj(v -> j)
        case Right(j) =>
          Json.obj(v -> j)
      }
    }

    val resp = if (failed) {
      log.error("There was an error during meta-schema migration.")
      Conflict(JsArray(results))
    } else {
      log.info("Meta-Schema migration complete.")
      Ok(JsArray(results))
    }

    log.debug(Json.prettyPrint(JsArray(results)))
    Future(resp)
  }  
  
  private[migrations] def getMigrationArgs(version: String, payload: JsValue): Option[JsObject] = {
    Js.find(payload.as[JsObject], s"/$version").map(_.as[JsObject])
  }
  
  /**
   * Find and perform a schema migration to the given version as the given identity.
   */
  def executeMigration(version: String, identity: UUID, payload: Option[JsValue] = None) = {
    lookupMigration(version).get.migrate(identity, payload)
  }
  
  /**
   * Find and return a Migration component for the given version string.
   */
  private def lookupMigration(version: String): Try[MetaMigration] = Try {
    version match {
      case "V1" => new V1()
      case "V2" => new V2()
      case "V3" => new V3()
      case "V4" => new V4()
      case "V5" => new V5()
      case "V6" => new V6()
      case "V7" => v7
      case "V8" => v8
      case "V9" => new V9()
      case "V10" => new V10()
      case "V11" => new V11()
      case "V12" => new V12()
      case "V13" => new V13()
      case "V14" => new V14()
      case "V15" => new V15()
      case "V16" => new V16()
      case _ =>
        throw new BadRequestException(s"No migration found for version '$version'")
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