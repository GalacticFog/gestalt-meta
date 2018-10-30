package controllers

import java.time.ZonedDateTime
import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import com.galacticfog.gestalt.data.util.PostgresHealth
import com.galacticfog.gestalt.meta.api.errors.ForbiddenException
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.meta.providers.ProviderManager
import com.galacticfog.gestalt.security.api.{GestaltAccount, GestaltBasicCredentials, GestaltOrg}
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltFrameworkSecurity}
import com.google.inject.Inject
import controllers.util.{HandleExceptions, SecureController, Security}
import javax.inject.Singleton
import migrations.MigrationController
import modules.SecurityKeyInit
import play.api.Logger
import play.api.i18n.MessagesApi
import play.api.libs.json._

import scala.util.{Failure, Success, Try}
import controllers.util.QueryString

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.{ResourceOwnerLink, ResourceStates}
import com.galacticfog.gestalt.meta.auth.DefaultMetaConfigManager

@Singleton
class BootstrapController @Inject()( 
       messagesApi: MessagesApi,
       sec: GestaltFrameworkSecurity,
       providerManager: ProviderManager,
       security: Security,
       securityInit: SecurityKeyInit,
       deleteController: DeleteController,
       db: play.api.db.Database,
       appconfig: play.api.Configuration,
       migration: MigrationController,
       metaConf: DefaultMetaConfigManager)
     extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
     
  val logger = Logger(this.getClass)
  
  def initProviders() = Audited() { implicit request =>
    val results = providerManager.loadProviders()
    Ok("TESTING PROVIDER LOADING...")
  }

  def bootstrap() = Audited() { implicit request =>
    log.debug("bootstrap()")
    val caller = request.identity    
    
    log.debug("Attempting to bootstrap Meta...")
    val maybeBootstrapped = for {
      admin  <- validateAdminUser(caller)
      org    <- security.getRootOrg(caller)
      _      <- cleanupExternals(request.queryString, caller)
      _      <- seedMetaDb(admin, org)
    } yield (admin, org)
    
    maybeBootstrapped match {
      case Failure(e) => { 
        log.error(s"Could not rebuild Meta DB: ${e.getMessage}")
        HandleExceptions(e)
      }
      case Success((admin, org)) => {
        log.info("Data migration complete.")
        log.info("Creating admin-user in Meta...")

        createAdminUser(admin, org) match {
          case Failure(e) => {
            log.error(s"Failed creating admin-user during bootstrap: ${e.getMessage}")
            HandleExceptions(e)
          }
          case Success(metaAdmin) => {

            log.info("Setting entitlements for admin-user on root-org...")
            setNewResourceEntitlements(org.id, org.id, caller, None)

            log.info("Stashing credentials in gestalt-security...")
            initGestaltSecurityCreds(org.id, caller)

            
            initializeRootUser().get
            
            /*
             * Test for migration (default is 'true', always migrate) pass ?migrate=false
             * to suppress.
             * If migrate param is missing, assume false, otherwise use given value
             */
            val performMigration: Boolean = {
              QueryString.single(request.queryString, "migrate").fold(true) {
                /*
                 * If we can't read the migrate value, set it to false. They
                 * can migrate manually.
                 */
                b => Try(b.toBoolean).recover { case _ =>
                  log.warn(s"Could not read value of `migrate` parameter found: '$b'. Setting to 'false'.")
                  false 
                }.get
              }
            }
            
            val report = if (performMigration) {
              log.info("Performing Meta-Schema migrations...")
              
              val result = {
                val migrations = Seq("V1", "V2", "V3", "V4", "V5", "V6", "V9", "V11", "V12", "V13")
                migrations.map { v =>
                    migration.executeMigration(v, caller.account.id) match {
                    case Left(e) => {
                      log.error("There was an error during meta-schema migration.")
                      e
                    }
                    case Right(s) => {
                      log.info("Meta-Schema migration complete.")
                      s
                    }
                  }
                }
              }
              result.lastOption
            } else None
            
            log.info("Bootstrap complete.")
            Ok(bootstrapOutputMessage(org, report))             
          }
        }
        
      }
    }
  }
  
  private[controllers] def initializeRootUser(/*user: GestaltResourceInstance*/): Try[Unit] = Try {
//    metaConfig.ensureReady.get
//    metaConfig.getRoot.getOrElse {
//      log.warn(s"Root identity not found in Meta-Configuration. This should not happen.")
//      log.warn(s"Setting root to gestalt-security admin")
//
//      metaConfig.setRoot(user.id) match {
//        case Failure(e) => throw new RuntimeException(s"Failed setting root user in Meta configuration.")
//        case Success(_) => user.id
//      }
//    }
    metaConf.initialize()
    ()
  }
  
  /**
   * Create root/admin user in Meta from corresponding Account in gestalt-security
   */
  private[controllers] def createAdminUser(
      admin: GestaltAccount, org: GestaltOrg): Try[GestaltResourceInstance] = {
    
    val props: Map[String,String] = Map(
        "firstName"    -> admin.firstName,
        "lastName"     -> admin.lastName,
        "email"        -> admin.email.getOrElse(""),
        "phoneNumber"   -> admin.phoneNumber.getOrElse(""),
        "gestalt_home" -> org.fqon)
        
    val gestaltAdmin = GestaltResourceInstance(
      id     = admin.id, 
      typeId = ResourceIds.User, 
      orgId  = org.id, 
      owner  = ResourceOwnerLink(ResourceIds.User, admin.id.toString), 
      name   = admin.name,
      state  = ResourceState.id(ResourceStates.Active),
      properties = Some(props)
    )
    ResourceFactory.create(ResourceIds.User, admin.id)(gestaltAdmin, Some(org.id))
  }
  
  private[controllers] def cleanupExternals(
      qs: Map[String, Seq[String]], auth: AuthAccountWithCreds) = Try {
    
    val clean = (qs.contains("clean") && qs("clean")(0) == "true")
    
    if (clean && isInitialized()) {
      log.info("Cleaning up containers...")
      val containers = ResourceFactory.findAll(ResourceIds.Container)
      if (containers.isEmpty) {
        log.info("No containers to clean - proceeding with bootstrap...")
      } else {
        containers foreach { c =>
          log.info(s"Deleting Container: ${c.id}, ${c.name}...")
          deleteController.manager.delete(c, auth, force = true)
        }
        log.info("Container deletes complete - proceeding with bootstrap...")
      }
    }
  }
  
  private[controllers] def validateAdminUser(auth: AuthAccountWithCreds) = {
    for {
      admin <- security.getRootUser(auth)
      out   <- Try {
        if (auth.account.id == admin.id) admin 
        else 
          throw new ForbiddenException(s"Insufficient permissions. POST /bootstrap may only be executed by the root/admin user.")
      }
    } yield out
  }

  private[controllers] def seedMetaDb(rootUser: GestaltAccount, rootOrg: GestaltOrg) = {
    
    log.debug("bootstrap : [root-org-id]  : " + rootOrg.id.toString)
    log.debug("bootstrap : [root-user-id] : " + rootUser.id.toString)
    
    val owner = ResourceOwnerLink(ResourceIds.User, rootUser.id.toString)
    val bootstrap = new Bootstrap(ResourceIds.User, rootUser.id, rootOrg.id, owner, db.dataSource)
    
    for {
      _ <- rebuildDatabase(bootstrap)
      _ <- resetClientConnections()
      x <- initializeDatabase(bootstrap)
    } yield x
  }
  
  private[controllers] def initGestaltSecurityCreds(org: UUID, auth: AuthAccountWithCreds) = {
    auth.creds match {
      case apiCreds: GestaltBasicCredentials =>
        securityInit.init(org, auth)
        log.info("Initializing gestalt-security-play with API credentials from /bootstrap")
      case _ =>
        log.warn("Bootstrap did not use API credentials; will not initialize security client")
    }
  }
  
  private[controllers] def bootstrapOutputMessage(rootOrg: GestaltOrg, migrationReport: Option[JsValue] = None): JsValue = {
    
    val report = migrationReport.fold {
      Json.obj("status" -> "No migration was performed.")
    }{ _.as[JsObject] }
    
    Json.obj(
      "start_time" -> ZonedDateTime.now.toString,
      "root_org" -> Json.obj(
          "id"   -> rootOrg.id.toString,
          "name" -> rootOrg.name
      ),
      "migration" -> report
    )      
  }  
  
  private[controllers] def isInitialized(): Boolean = {
    appconfig.getString("meta.db.name").fold {
      logger.warn("Configuration item 'meta.db.name' not set.")
      false
    }{ nm =>
      logger.info("Checking Meta repository initialization...")
      PostgresHealth.dbInitialized(nm) match {
        case Failure(e) => {
          logger.error("Failed checking repository initialization.")
          throw e
        }
        case Success(initialized) => {
          if(!initialized) logger.info("Meta repository is NOT initialized.") 
          initialized
        }
      }      
    }    
  }
  
  private[controllers] def rebuildDatabase(bootstrap: Bootstrap) = scalikejdbc.DB.autoCommit { session => 
    for {
      a <- bootstrap.clean
      b <- bootstrap.migrate
    } yield b
  }
  
  private[controllers] def resetClientConnections() = Try {
    scalikejdbc.config.DBs.closeAll()
    scalikejdbc.config.DBs.setupAll()
  }
  
  private[controllers] def initializeDatabase(bootstrap: Bootstrap) = scalikejdbc.DB.autoCommit { session => 
    for {
      c <- bootstrap.loadReferenceData
      d <- bootstrap.loadSystemTypes
      e <- bootstrap.initialize("root")
    } yield e
  }
  
  private def rte(message: String) = new RuntimeException(message)  
}
