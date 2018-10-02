package modules

import javax.inject.{Inject, Singleton}
import com.google.inject.AbstractModule
import play.api.inject.ApplicationLifecycle
import controllers.util.Security
import java.util.UUID
import com.galacticfog.gestalt.meta.auth.DefaultMetaConfigManager
import com.galacticfog.gestalt.meta.api.sdk.{GestaltConfiguration, GestaltConfigurationManager}
import com.galacticfog.gestalt.data.PostgresConfigManager
import scala.util.{Try,Success,Failure}


class MetaConfigModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[GestaltConfigurationManager]).toInstance(PostgresConfigManager)
    bind(classOf[DefaultMetaConfigManager]).asEagerSingleton()  
  }
}
