package modules

import com.galacticfog.gestalt.events.{AmqpClient, AmqpConnection}
import com.galacticfog.gestalt.meta.genericactions.{DefaultGenericProviderManager, GenericProviderManager}
import com.google.inject.AbstractModule
import controllers.util.{ContainerService, ContainerServiceImpl}
import net.codingwell.scalaguice.ScalaModule

class MetaDefaultServices extends AbstractModule with ScalaModule {

  override def configure(): Unit = {
    bind[GenericProviderManager].to[DefaultGenericProviderManager]
    bind[ContainerService].to[ContainerServiceImpl]
    bind[AmqpClient].toInstance({
      AmqpClient(
          AmqpConnection(sys.env("RABBIT_HOST"), sys.env("RABBIT_PORT").toInt, heartbeat = 300))
    })
  }

}
