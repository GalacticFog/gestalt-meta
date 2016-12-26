package modules

import com.galacticfog.gestalt.events.{AmqpClient, AmqpConnection}
import com.google.inject.AbstractModule
import controllers.util.{ContainerService, ContainerServiceImpl}

class MetaDefaultServices extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[ContainerService]).to(classOf[ContainerServiceImpl])
    bind(classOf[AmqpClient]).toInstance({
      AmqpClient(
          AmqpConnection(sys.env("RABBIT_HOST"), sys.env("RABBIT_PORT").toInt, heartbeat = 300))
    })
  }

}
