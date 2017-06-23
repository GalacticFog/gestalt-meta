package modules

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import play.api.libs.concurrent.AkkaGuiceSupport
import services.{DCOSAuthTokenActor, DefaultMarathonClientFactory, MarathonClientFactory}

class MetaDefaultDCOS extends AbstractModule with ScalaModule with AkkaGuiceSupport {

  override def configure(): Unit = {
    bind[MarathonClientFactory].to[DefaultMarathonClientFactory]
    bindActor[DCOSAuthTokenActor](DCOSAuthTokenActor.name)
  }

}
