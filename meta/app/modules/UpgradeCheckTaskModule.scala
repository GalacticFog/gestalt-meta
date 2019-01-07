package modules


import actors.{UpgradeCheckTask, UpgradeCheckScheduler}
import com.google.inject.AbstractModule
import play.api.libs.concurrent.AkkaGuiceSupport


class UpgradeCheckTaskModule extends AbstractModule with AkkaGuiceSupport {
  
  override def configure() = {
    bindActor[UpgradeCheckTask](UpgradeCheckTask.name)
    bind(classOf[UpgradeCheckScheduler]).asEagerSingleton()
  }  
}