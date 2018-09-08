package com.galacticfog.gestalt.meta.auth

import javax.inject.{Inject, Singleton}
import com.google.inject.AbstractModule
import play.api.inject.ApplicationLifecycle
import controllers.util.Security
import java.util.UUID
import scala.util.{Try,Success,Failure}
import com.galacticfog.gestalt.data.MetaConfigData
import org.slf4j.LoggerFactory

class MetaConfigModule extends AbstractModule {
  override def configure(): Unit = bind(classOf[DefaultMetaConfig]).asEagerSingleton()  
}

trait MetaConfiguration {
  
  object Keys {
    val MetaRootUser = UUID.fromString("af86f1da-82cf-44da-a01c-d392957f8083")  
  }  
  
  /**
   * Create the meta-configuration table.
   * @param force if true any existing table with the same given name will be recreated
   */
  def setup(force: Boolean): Try[Unit] = 
    MetaConfigData.setup(force)
  
  def isReady(): Boolean = 
    MetaConfigData.isReady()

  def get(key: UUID): Option[String] = 
    MetaConfigData.get(key)
  
  def set(key: UUID, data: String): Try[Unit] = 
    MetaConfigData.set(key, data)
  
}

@Singleton
class DefaultMetaConfig @Inject()(security: Security) extends MetaConfiguration {
  
  val GESTALT_META_CONFIG_SECRETS_PATH = "GESTALT_META_CONFIG_SECRETS_PATH"
  val GESTALT_META_ROOT_IDENTITY = "GESTALT_META_ROOT_IDENTITY"
  val DEFAULT_CONFIG_SECRETS_PATH = "/gestalt/root"
  
  private[this] val log = LoggerFactory.getLogger(this.getClass)
  
  /**
   * Get the persisted root user identity
   */
  def getRoot(): Option[UUID] = {
    MetaConfigData.get(Keys.MetaRootUser).map(UUID.fromString(_))
  }
  
  /**
   * Persist the root user identity
   */
  def setRoot(id: UUID): Try[Unit] = {
    MetaConfigData.set(Keys.MetaRootUser, id.toString)
  }  
  
  /**
   * Ensure the configuration datastore is ready and available
   */
  def ensureReady(): Try[Unit] = {
    if (isReady) Success(()) else setup(true)  
  }

  /**
   * Initialize Meta configuration.
   */
  def initConfiguration(): Try[Unit] = {
    
    // Ensure the config store is ready and discover 'root'
    val root = for {
      _ <- ensureReady
      r <- getRootIdentity
    } yield r
    
    root match {
      case Failure(e) => ???
      case Success(id) => {
        log.info("Setting root identity in meta configuration.")
        setRoot(id)
      }
    }
  }
  
  private[auth] def getRootIdentity(): Try[UUID] = Try {
    log.info("Establishing root-user identity...")
    
    val secretsPath = sys.env.get(GESTALT_META_CONFIG_SECRETS_PATH).getOrElse {
      DEFAULT_CONFIG_SECRETS_PATH
    }
    
    log.info(s"Checking for mounted secret @$secretsPath...")
    
    val maybeSecret = readMountedFile(secretsPath)

    val identity = {
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
    
    try { UUID.fromString(identity.get) } catch {
      case e: Throwable => {
        log.error(s"An error occurred converting identity to UUID. found: ${identity.get}")
        throw e
      }
    }
    
//    val maybeIdentity = {
//      readMountedFile(secretsPath) orElse 
//      sys.env.get(GESTALT_META_ROOT_IDENTITY).flatMap(readEnvironmentVar(_))
//    }
//    (maybeIdentity orElse readGestaltSecurity()).map(UUID.fromString(_))
  }
  
  private[auth] def readMountedFile(path: String): Option[String] = {
    val source = scala.io.Source.fromFile(path)
    try { Option(source.mkString) } finally {
      source.close()
    }
  }
  
  private[auth] def readEnvironmentVar(key: String): Option[String] = {
    sys.env.get(key)
  }
  
  private[auth] def readGestaltSecurity(): Option[String] = {
    security.getRootUser(auth = ???)
    ???
  }
  
}