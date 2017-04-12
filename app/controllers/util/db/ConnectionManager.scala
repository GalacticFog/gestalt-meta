//package controllers.util.db
//
//import com.galacticfog.gestalt.data.util._
//import javax.inject.{Inject,Singleton}
//import play.api.Logger
//import org.apache.commons.dbcp2.BasicDataSource
//
//@Singleton
//class ConnectionManager @Inject()() {
//  println("*********************************************************")
//  println("CONNECTION MANAGER INITIALIZED")
//  println("*********************************************************")
//  private[this] val log = Logger(this.getClass)
//  
//  log.debug("ConnectionManager::init()")
//
//  val config = loadConfig
//  
//  def dataSource = {
//    val ds = new BasicDataSource()
//    ds.setDriverClassName(config.driver)
//    ds.setUsername(config.username.get)
//    ds.setPassword(config.password.get)
//    ds.setUrl(config.url())
//
//    ds
//  }
//  dataSource.getCacheState
//  def currentDataSource(): BasicDataSource = dataSource
//  
//  //def currentConnection() = dataSource.getConnection()
//  
//  /**
//   * Resolve database configuration info from either the
//   * system environment (env) or /conf/application.conf
//   */
//  private def loadConfig: ScalikePostgresInfo = {
//    if (EnvConfig.isValid) {
//      log.info("Using Environment Database Info.")
//      log.info(EnvConfig.toString)
//      EnvConfig.getConnection()
//    } 
//    else if (AppConfConfig.isValid) {
//      log.info("Using application.conf Database Info.")
//      AppConfConfig.getConnection()
//    } 
//    else {
//      val message = "Unable to find database configuration. Checked ENV and application.conf."
//      log.error(message)
//      throw new RuntimeException(message)
//    }
//  }
//
//}