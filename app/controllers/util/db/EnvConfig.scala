package controllers.util.db

import com.galacticfog.gestalt.data.util._
import controllers.util.AppConf
import play.api.{ Logger => log }

object EnvConfig extends JdbcConfiguration {
  private val defaultTimeout = 5000
  private lazy val host = System.getenv("DATABASE_HOSTNAME")
  private lazy val port = System.getenv("DATABASE_PORT")
  private lazy val dbname = System.getenv("DATABASE_NAME")
  private lazy val username = System.getenv("DATABASE_USERNAME")
  private lazy val password = System.getenv("DATABASE_PASSWORD")
  private lazy val timeout = System.getenv("DATABASE_TIMEOUT_MS")

  def isValid() = {
    !(empty(host) && empty(port) && empty(dbname) && empty(username) && empty(password))
  }

  def getConnection() = {
    ScalikePostgresInfo(
      host,
      port.toInt,
      dbname,
      username,
      password,
      timeoutMs = (if (empty(timeout)) defaultTimeout else timeout.toInt))
  }

  private def empty(s: String) = (s == null || s.trim.isEmpty())
  override def toString = {
    s"EnvConfig(\n  host = ${host},\n  port = ${port},\n  dbname = ${dbname},\n  username = ${username},\n  password = '...'\n)"
  }
}