package modules

import com.google.inject.AbstractModule
import controllers.util.{DefaultUpgraderService, UpgraderService}
import net.codingwell.scalaguice.ScalaModule
import play.api.libs.concurrent.AkkaGuiceSupport

class UpgraderServiceModule extends AbstractModule with ScalaModule with AkkaGuiceSupport {

  override def configure(): Unit = {
    bind[UpgraderService].to[DefaultUpgraderService]
    ()
  }

}
