package controllers.util


import javax.inject.{Inject,Singleton}
import scala.util.Failure
import scala.util.Success
import org.postgresql.util.PSQLException
import com.galacticfog.gestalt.data.util.JdbcConnectionInfo
import com.galacticfog.gestalt.data.util.PostgresHealth
import db.ConnectionManager
import play.api.Logger

@Singleton
class DataStore @Inject()(connectionManager: ConnectionManager) {
  
  val log = Logger(this.getClass)
  val online = metaOnline( connectionManager.config )
  
  def assertOnline(onFail: => Unit, failMessage: String) = {
    if ( online ) {
      log.info("Meta Data-Store ONLINE")
    } 
    else {
      log.error("FATAL: Cannot connect to Meta Data-Store")
      log.error("Current configuration:")
      log.error(connectionManager.toString)
      log.error(failMessage)
      onFail
    }
  }

  /**
   * Ensure we can reach the server specified in configuration and
   * that the specified database exists.
   */
  def metaOnline(config: JdbcConnectionInfo): Boolean = {
    
    log.info(s"Pinging Meta Repository: [${config.host}, ${config.database}]...")
    
    PostgresHealth.verifyDataStore( config.database ) match {
      case Success( _ )  => {
        log.info( "data-store: Available" )
        log.info( connectionManager.toString )
        true
      }
      case Failure( ex ) => ex match {
        case p: PSQLException => {
          log.error( s"data-store: ${p.getMessage}" )
          false
        }
        case e: Throwable => {
          log.error( "Unexpected-Error : " + e.getMessage )
          false
        }
      }
    }  
  }
}