package controllers.util.db


import com.galacticfog.gestalt.data.util._
import controllers.util.AppConf
import play.api.{Logger => log}


object ConnectionManager {
  
  log.debug("ConnectionManager::init()")
  
  lazy val config = loadConfig

  /**
   * Resolve database configuration info from either the
   * system environment (env) or /conf/application.conf
   */
  private def loadConfig: ScalikePostgresInfo = {
    if (EnvConfig.isValid) {
      log.info("Using Environment Database Info.")
      log.info(EnvConfig.toString)
      EnvConfig.getConnection()
    } 
    else if (AppConfConfig.isValid) {
      log.info("Using application.conf Database Info.")
      AppConfConfig.getConnection()
    } 
    else {
      val message = "Unable to find database configuration. Checked ENV and application.conf."
      log.error(message)
      throw new RuntimeException(message)
    }
  }

}