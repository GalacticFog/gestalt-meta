//package modules
//
//import com.google.inject.AbstractModule
//import com.google.inject.name.Names
//import net.codingwell.scalaguice.ScalaModule
//
//class RepositorySettingsModule extends AbstractModule with ScalaModule {
//  override def configure(): Unit = {
//    bind[RepositoryInit].asEagerSingleton()
//  }
//}
//
//trait RepositorySettings {
//  RepositoryInit.initialize()
//}
//
//class RepositoryInit() {
//  
//  
//  
//}
//
//object RepositoryInit {
//  
//  private var initialized = false
//  
//  def initialize(): Unit = this.synchronized {
//    if (initialized) {
//      return
//    }
//    else {
//      scalikejdbc.config.DBs.setupAll()  
//      initialized = true
//    }
//  }
//}