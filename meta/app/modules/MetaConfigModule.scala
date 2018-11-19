package modules

import com.google.inject.AbstractModule
import com.galacticfog.gestalt.meta.auth.DefaultMetaConfigManager
import com.galacticfog.gestalt.meta.api.sdk.GestaltConfigurationManager
import com.galacticfog.gestalt.data.PostgresConfigManager


class MetaConfigModule extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[GestaltConfigurationManager]).toInstance(PostgresConfigManager)
    bind(classOf[DefaultMetaConfigManager]).asEagerSingleton()  
  }
}
