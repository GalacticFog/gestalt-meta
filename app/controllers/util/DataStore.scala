package controllers.util


import javax.inject.{Inject,Singleton}
import scala.util.Failure
import scala.util.Success
import org.postgresql.util.PSQLException
import com.galacticfog.gestalt.data.util.JdbcConnectionInfo
import com.galacticfog.gestalt.data.util.PostgresHealth
//import db.ConnectionManager
import play.api.Logger

import play.api.db.Database

@Singleton
class DataStore @Inject()(db: Database) {
  
  scalikejdbc.config.DBs.setupAll()
  
  val log = Logger(this.getClass)
  val online = metaOnline()
  
  def assertOnline(onFail: => Unit, failMessage: String) = {
    if ( online ) {
      log.info("Meta Data-Store ONLINE")
    } 
    else {
      log.error("FATAL: Cannot connect to Meta Data-Store")
      log.error("Current configuration:")
      log.error(db.dataSource.getConnection.getMetaData.getURL)
      log.error(failMessage)
      onFail
    }
  }

  /**
   * Ensure we can reach the server specified in configuration and
   * that the specified database exists.
   */
  def metaOnline(): Boolean = {
    val database = db.dataSource.getConnection.getCatalog
    val jdbcurl  = db.dataSource.getConnection.getMetaData.getURL    

    log.info(s"Pinging Meta Repository: [${jdbcurl}]...")
    
    PostgresHealth.verifyDataStore( database ) match {
      case Success( _ )  => {
        log.info( "data-store: Available" )
        //log.info( connectionManager.toString )
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