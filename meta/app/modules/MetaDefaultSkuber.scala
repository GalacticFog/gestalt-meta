package modules

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import play.api.libs.concurrent.AkkaGuiceSupport
import com.galacticfog.gestalt.integrations.kubernetes.KubeTokenActor
import services.{DefaultSkuberFactory, SkuberFactory}

class MetaDefaultSkuber extends AbstractModule with ScalaModule with AkkaGuiceSupport {

  override def configure(): Unit = {
    bind[SkuberFactory].to[DefaultSkuberFactory]
    bindActor[KubeTokenActor](KubeTokenActor.name)
  }

}
