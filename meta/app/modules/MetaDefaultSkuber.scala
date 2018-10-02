package modules

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import play.api.libs.concurrent.AkkaGuiceSupport
import services.{DefaultSkuberFactory, KubeTokenActor, SkuberFactory}

class MetaDefaultSkuber extends AbstractModule with ScalaModule with AkkaGuiceSupport {

  override def configure(): Unit = {
    bind[SkuberFactory].to[DefaultSkuberFactory]
    bindActor[KubeTokenActor](KubeTokenActor.name)
  }

}
