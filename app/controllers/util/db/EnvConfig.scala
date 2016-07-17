package controllers.util.db


import com.galacticfog.gestalt.data.util._
import controllers.util.AppConf
import play.api.{ Logger => log }
import play.api.libs.json._

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

  lazy val securityKey = System.getenv("GESTALT_SECURITY_KEY")
  lazy val securitySecret = System.getenv("GESTALT_SECURITY_SECRET")
  
  lazy val rabbitHost = System.getenv("RABBIT_HOST")
  lazy val rabbitPort = System.getenv("RABBIT_PORT")
  lazy val rabbitHttpPort = Option(System.getenv("RABBIT_PORT")) getOrElse "80"
  lazy val rabbitExchange = System.getenv("RABBIT_EXCHANGE")
  lazy val rabbitRoute = System.getenv("RABBIT_ROUTE")
  
  private lazy val security_protocol = System.getenv("GESTALT_SECURITY_PROTOCOL")
  private lazy val security_hostname = System.getenv("GESTALT_SECURITY_HOSTNAME")
  private lazy val security_port = System.getenv("GESTALT_SECURITY_PORT")

  val gatewayUrl  = apigateway
  val lambdaUrl   = lambda

  val databaseUrl = {
    "jdbc:postgresql://%s:%s/%s?user=%s&password=*****".format(host, port, dbname, username)
  }
  
  val securityUrl = {
    val port = if (security_port.toInt > 0) ":"+security_port else ""
    "%s://%s%s".format(security_protocol, security_hostname, port)
  }
  
  val rabbitUrl = "amqp://%s:%s".format(rabbitHost, rabbitHttpPort)
  
  private val rabbit = Json.obj("url" -> rabbitUrl, "exchange" -> rabbitExchange, "route" -> rabbitRoute)
  
  
  def isValid() = {
    !(empty(host) && empty(port) && empty(dbname) && empty(username) && empty(password) &&
        empty(apigateway) && empty(lambda) &&
        empty(security_protocol) && empty(security_hostname) &&
        empty(rabbitHost) &&
        empty(rabbitPort) &&
        empty(rabbitHttpPort) &&
        empty(rabbitExchange) &&
        empty(rabbitRoute))
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
    s"""
      |EnvConfig(
      |  database = ${databaseUrl},
      |  apigateway = ${gatewayUrl},
      |  lambda = ${lambdaUrl},
      |  security = ${securityUrl},
      |  events = ${Json.prettyPrint(rabbit)})
    """.stripMargin
  }
}