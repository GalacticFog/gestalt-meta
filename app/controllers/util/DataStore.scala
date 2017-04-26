package controllers.util


import javax.inject.{Inject,Singleton}
import scala.util.Failure
import scala.util.Success
import org.postgresql.util.PSQLException
import com.galacticfog.gestalt.data.util.JdbcConnectionInfo
import com.galacticfog.gestalt.data.util.PostgresHealth
import play.api.Logger

import play.api.db.Database
import play.api.Configuration
import com.zaxxer.hikari.HikariDataSource

trait DataStoreSettings {
  DataStore.initialize()
}

@Singleton
class DataStore @Inject()(db: Database, config: Configuration) extends DataStoreSettings {
  
  private val log = Logger(this.getClass)
  
  val dataSource = db.dataSource
  
  /**
   * 
   */
  def assertOnline(onFail: => Unit, failMessage: String) = {
    if (repositoryOnline()) {
      log.info("Meta Repository ONLINE")
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
   * Write the current ConnectionPool settings to the log. If current config is either
   * not a pooled datasource a warning is written to the log.
   * Currently only writes HikariCP settings.
   */
  private[util] def logPoolInfo(config: play.api.Configuration) = {

    val noPoolInfo = {
      DataSourceConfig.poolType(config) match {
        case None => Some(s"Datasource is not a pooled datasource.")
        case Some(tpe) => {
          if (DataSourceConfig.isHikariCp(tpe.trim)) None
          else Some(s"No configuration reader for ConnectionPool type '${tpe}'. This is NOT an error.")
        }
      }
    }

    if (noPoolInfo.isDefined)
      log.warn(noPoolInfo.get)
    else {
      def line(k: String, v: String) = "%-25s%s".format(k, v).replace(' ', '.')
      val ds = db.dataSource.asInstanceOf[HikariDataSource]
      
      log.info(line("jdbcUrl", ds.getJdbcUrl))
      log.info(line("dataSourceClassName", ds.getDataSourceClassName))
      log.info(line("driverClassName", ds.getDriverClassName))
      log.info(line("username", ds.getUsername))
      log.info(line("password", "<masked>"))
      log.info(line("autoCommit", ds.isAutoCommit.toString))
      log.info(line("minimumIdle", ds.getMinimumIdle.toString))
      log.info(line("maximumPoolSize", ds.getMaximumPoolSize.toString))
      log.info(line("maxLifetime", ds.getMaxLifetime.toString))
    }
  }
  
  /**
   * Ensure we can reach the database server specified in configuration and
   * that the specified database exists.
   */
  def repositoryOnline(): Boolean = {

    val connection = db.dataSource.getConnection()
    
    try {

      val database = connection.getCatalog
      val jdbcurl  = connection.getMetaData.getURL

//      val database = db.name
//      val jdbcurl = db.url
      
      log.info(s"Testing Meta Repository: [${jdbcurl}]...")
    
      /*
       * verifyDataStore checks that the named database exists
       * and has tables created.
       */
      PostgresHealth.verifyDataStore(database) match {
        case Success(_) => {
          log.info( "Repository is AVAILABLE" )
          logPoolInfo(config)
          true
        }
        case Failure(ex) => ex match {
          case p: PSQLException => {
            log.error(s"Could not verify repository: ${p.getMessage}")
            false
          }
          case e: Throwable => {
            log.error("Unexpected error occurred contacting the Meta repository : " + e.getMessage)
            false
          }
        }
      }
    }
    finally {
      connection.close()
    }
  }

//  import scala.util.Try
//  def repositoryOnline(): Boolean = {
//
//    
//    Try(db.dataSource.getConnection()).flatMap { cnn =>
//      try {
//        ???
//      } finally {
//        cnn.close()
//      }
//    }
//    
//    try {
//      val connection = db.dataSource.getConnection()
//      
//      db.name
//      db.url
//      
//      try {
//
//        val database = connection.getCatalog
//        val jdbcurl = connection.getMetaData.getURL
//
//        log.info(s"Testing Meta Repository: [${jdbcurl}]...")
//
//      /*
//       * verifyDataStore checks that the named database exists
//       * and has tables created.
//       */
//        PostgresHealth.verifyDataStore(database) match {
//          case Success(_) => {
//            log.info("Repository is AVAILABLE")
//            logPoolInfo(config)
//            true
//          }
//          case Failure(ex) => ex match {
//            case p: PSQLException => {
//              log.error(s"Could not verify repository: ${p.getMessage}")
//              false
//            }
//            case e: Throwable => {
//              log.error("Unexpected error occurred contacting the Meta repository : " + e.getMessage)
//              false
//            }
//          }
//        }
//      } finally {
//        connection.close()
//      }
//    
//  }  
  
}

object DataStore {
  private val log = Logger(this.getClass)
  private var initialized = false
  
  def initialize(): Unit = this.synchronized {
    log.info("Received request to initialize repository connection...")
    if (initialized) {
      log.info("Connection already initialized. Nothing to do.")
      return
    }
    else {
      scalikejdbc.config.DBs.setupAll()
      initialized = true
      log.info("Connection initialized.")
    }
  }
}

import play.api.Configuration

object DataSourceConfig {
  
  def isPooled(config: Configuration): Boolean = { 
    poolType(config).isDefined
  }

  def poolType(config: Configuration): Option[String] = {
    config.getString("play.db.pool")
  }
  
  def isHikariCp(config: Configuration): Boolean = {
    poolType(config).fold(false) { name =>
      name.trim.equalsIgnoreCase("hikaricp") ||
      name.trim.equalsIgnoreCase("default")
    }
  }
  
  def isHikariCp(name: String) = {
    name.trim.equalsIgnoreCase("hikaricp") ||
    name.trim.equalsIgnoreCase("default")
  }
}