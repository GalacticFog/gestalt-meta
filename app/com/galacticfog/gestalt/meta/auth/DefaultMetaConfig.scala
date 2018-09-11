package com.galacticfog.gestalt.meta.auth

import javax.inject.{Inject, Singleton}
import com.google.inject.AbstractModule
import play.api.inject.ApplicationLifecycle
import controllers.util.Security
import java.util.UUID
import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data.PostgresConfigManager

import org.slf4j.LoggerFactory
import java.nio.file.{Paths, Files}
import java.nio.file.{Paths, Files}

import com.galacticfog.gestalt.meta.api.sdk.GestaltConfigurationManager
import com.galacticfog.gestalt.meta.api.sdk.GestaltConfiguration
import modules.SecurityClientProvider


class DefaultMetaConfiguration extends GestaltConfiguration {

  object Keys {
    val MetaRootUser = UUID.fromString("af86f1da-82cf-44da-a01c-d392957f8083")  
  }
  
  def isReady(): Boolean = { 
    PostgresConfigManager.isReady()
  }

  def get(key: UUID): Option[String] = { 
    PostgresConfigManager.get(key)
  }

  def set(key: UUID, data: String): Try[Unit] = {
    PostgresConfigManager.set(key, data)
  }
  
  /**
   * Get the persisted root user identity
   */
  def getRoot(): Option[UUID] = {
    PostgresConfigManager.get(Keys.MetaRootUser).map(UUID.fromString(_))
  }
  
  /**
   * Persist the root user identity
   */
  def setRoot(id: UUID): Try[Unit] = {
    PostgresConfigManager.set(Keys.MetaRootUser, id.toString)
  }

  /**
   * Determine if given identity matches root-user ID
   */
  def isRoot(identity: UUID): Boolean = {
    getRoot.fold(false)(identity == _)
  }
  
  /**
   * Ensure the configuration datastore is ready and available
   */
  def ensureReady(): Try[Unit] = {
    if (isReady) Success(()) else PostgresConfigManager.setup(true)  
  }  
}



@Singleton
class DefaultMetaConfigManager @Inject()(
    config: GestaltConfigurationManager, 
    client: SecurityClientProvider,
    security: Security) 
      extends DefaultMetaConfiguration with GestaltConfigurationManager {
  
  private[this] val log = LoggerFactory.getLogger(this.getClass)
  
  val GESTALT_META_CONFIG_SECRETS_PATH = "GESTALT_META_CONFIG_SECRETS_PATH"
  val GESTALT_META_ROOT_IDENTITY = "GESTALT_META_ROOT_IDENTITY"
  val DEFAULT_CONFIG_SECRETS_PATH = "/gestalt/root"

  
  initialize()
  
  
  def setup(force: Boolean): Try[Unit] = {
    config.setup(force)
  }

  /**
   * Initialize Meta configuration.
   */
  def initialize(): Try[Unit] = {
    
    // Ensure the config store is ready and discover 'root'
    val root = for {
      _ <- ensureReady
      r <- findRootIdentity
    } yield r
    
    root match {
      case Failure(e) => {
        throw new RuntimeException("Could not determine 'root' identity.")
      }
      case Success(id) => {
        log.info("Setting root identity in meta configuration.")
        setRoot(id)
      }
    }
  }
  
  private[auth] def findRootIdentity(): Try[UUID] = Try {
    log.info("Establishing root-user identity...")
    
    val secretsPath = sys.env.get(GESTALT_META_CONFIG_SECRETS_PATH).getOrElse {
      DEFAULT_CONFIG_SECRETS_PATH
    }
    
    log.info(s"Checking for mounted secret @$secretsPath...")
    val maybeSecret = readMountedFile(secretsPath)

    val identity: Option[UUID] = {
      /*
       * Check secrets file
       */
      if (maybeSecret.isDefined) {
        log.info("Using root identity from secrets file.")
        maybeSecret
        
      } else {
        /*
         * Check Environment Var
         */
        log.info("Secrets file not found. Checking env vars for root identity...")
        val maybeEnvironment = 
          sys.env.get(GESTALT_META_ROOT_IDENTITY).flatMap(readEnvironmentVar(_))
          
        if (maybeEnvironment.isDefined) {
          log.info("Using root identity from Environment")
          maybeEnvironment
          
        } else {
          /*
           * Check Gestalt-Security
           */
          log.info("Identity Env var not found. Asking gestalt-security...")
          val maybeSecurity = readGestaltSecurity()
          
          if (maybeSecurity.isDefined) {
            log.info("Using root identity from gestalt-security")
            maybeSecurity
            
          } else {
            throw new RuntimeException(
              "Could not determine the root user identity. This is fatal. Contact an administrator.")
          }
        }
      }
    }

    identity.get
    
//    val maybeIdentity = {
//      readMountedFile(secretsPath) orElse 
//      sys.env.get(GESTALT_META_ROOT_IDENTITY).flatMap(readEnvironmentVar(_))
//    }
//    (maybeIdentity orElse readGestaltSecurity()).map(UUID.fromString(_))
  }

  private[auth] def readMountedFile(path: String): Option[UUID] = {
    if (Files.exists(Paths.get(path))) {
      val source = scala.io.Source.fromFile(path)
      try { 
        Option(toUserUuid(source.mkString.trim))
      } finally {
        source.close()
      }
    } else {
      log.warn(s"Could not find given path: '${GESTALT_META_CONFIG_SECRETS_PATH} = ${path}'") 
      None
    }
  }
  
  private[auth] def readEnvironmentVar(key: String): Option[UUID] = {
    sys.env.get(key).map(toUserUuid(_))
  }
  
  private[auth] def readGestaltSecurity(): Option[UUID] = {
    security.getRootUser()(client.client) match {
      case Failure(e) => throw new RuntimeException("FAILED talking to Gestalt-Security")
      case Success(r) => Some(toUserUuid(r.id.toString))
    }
  }
 
  /**
   * 
   * @param sid String representation of UUID to convert
   * @param validate if 'true' will test if gestalt-security knows the given user
   */
  private[auth] def toUserUuid(sid: String, validate: Boolean = true): UUID = {
    val result = Try(UUID.fromString(sid.trim))
    if (result.isFailure) {
      log.error("An error occurred converting identity string to UUID. found: '${sid}'")  
    }
    
//    if (validate) {
//      // Test that the user actually exists here.
//    }
    result.get
  }
  
  
}