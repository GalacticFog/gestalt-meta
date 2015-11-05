package controllers.util.db

import com.galacticfog.gestalt.data.util._
import controllers.util.AppConf
import play.api.{ Logger => log }

object AppConfConfig extends JdbcConfiguration {
  private lazy val connection = AppConf.get("database")

  def isValid() = connection match {
    case None => false
    case Some(config) => {
      config.contains("host") &&
        config.contains("port") &&
        config.contains("user") &&
        config.contains("dbname") &&
        config.contains("password")
    }
  }

  def getConnection() = {
    val connection = AppConf.get("database") match {
      case None =>
        throw new RuntimeException("FATAL: Database configuration not found.")
      case Some(config) => {
        displayStartupSettings(config)
        ScalikePostgresInfo(
          host = config("host").toString,
          database = config("dbname").toString,
          port = config("port").toString.toInt,
          user = config("user").toString,
          password = config("password").toString,
          timeoutMs = config("timeoutMs").toString.toLong)
      }
    }
    println("CONNECTION : " + connection)
    connection
  }
  
  private def displayStartupSettings(config: Map[String, Object]) {
    log.debug("DATABASE SETTINGS:")
    for ((k, v) <- config) {
      if (k != "password")
        log.debug("%s = '%s'".format(k, v.toString))
    }
  }  
}