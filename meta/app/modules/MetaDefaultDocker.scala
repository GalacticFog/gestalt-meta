package modules

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import services.{DefaultDockerClientFactory, DockerClientFactory}

class MetaDefaultDocker extends AbstractModule with ScalaModule {

  override def configure(): Unit = {
    bind[DockerClientFactory].to[DefaultDockerClientFactory]
    ()
  }

}
