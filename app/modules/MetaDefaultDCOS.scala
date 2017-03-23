package modules

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import services.{DefaultMarathonClientFactory, MarathonClientFactory}

class MetaDefaultDCOS extends AbstractModule with ScalaModule {

  override def configure(): Unit = {
    bind[MarathonClientFactory].to[DefaultMarathonClientFactory]
  }

}
