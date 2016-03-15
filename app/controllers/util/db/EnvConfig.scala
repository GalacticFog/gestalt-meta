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
  private lazy val apigateway = System.getenv("GESTALT_APIGATEWAY")
  private lazy val lambda = System.getenv("GESTALT_LAMBDA")
  private lazy val marathon = System.getenv("GESTALT_MARATHON_PROVIDER")
  
  def isValid() = {
    !(empty(host) && empty(port) && empty(dbname) && empty(username) && empty(password) &&
        empty(apigateway) && empty(lambda) && empty(marathon))
  }
  
  val gatewayUrl = apigateway
  val lambdaUrl = lambda
  val marathonUrl = marathon
  
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
    s"EnvConfig(\n  host = ${host},\n  port = ${port},\n  dbname = ${dbname},\n  username = ${username},\n  db_password = '...',\n  apigateway = ${apigateway},\n  lambda_service = ${lambda},\n  marathon = ${marathon})"
  }
}