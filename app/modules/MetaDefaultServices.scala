package modules

import actors.SystemConfigActor
import com.galacticfog.gestalt.events.{AmqpClient, AmqpConnection}
import com.galacticfog.gestalt.meta.genericactions.{DefaultGenericProviderManager, GenericProviderManager}
import com.google.inject.AbstractModule
import controllers.util.{ContainerService, ContainerServiceImpl, GenericResourceMethods, GenericResourceMethodsImpl}
import net.codingwell.scalaguice.ScalaModule
import play.api.libs.concurrent.AkkaGuiceSupport

class MetaDefaultServices extends AbstractModule with ScalaModule with AkkaGuiceSupport {

  override def configure(): Unit = {
    bind[GenericProviderManager].to[DefaultGenericProviderManager]
    bind[GenericResourceMethods].to[GenericResourceMethodsImpl]
    bind[ContainerService].to[ContainerServiceImpl]
    bindActor[SystemConfigActor](SystemConfigActor.name)
    bind[AmqpClient].toInstance({
      AmqpClient(
          AmqpConnection(sys.env("RABBIT_HOST"), sys.env("RABBIT_PORT").toInt, heartbeat = 300))
    })
  }

}
