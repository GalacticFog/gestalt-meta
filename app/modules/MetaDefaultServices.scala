package modules

import com.galacticfog.gestalt.events.{AmqpClient, AmqpConnection}
import com.google.inject.AbstractModule
import controllers.util.{ContainerService, ContainerServiceImpl}

class MetaDefaultServices extends AbstractModule {

  override def configure(): Unit = {
    bind(classOf[ContainerService]).to(classOf[ContainerServiceImpl])
    bind(classOf[AmqpClient]).toInstance({
      // AmqpClient(AmqpConnection(RABBIT_HOST, RABBIT_PORT, heartbeat = 300))
      throw new RuntimeException("Not sure what to do here, it used to be the code above")
    })
  }

}
