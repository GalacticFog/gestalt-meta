package modules

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import services.{DefaultSkuberFactory, SkuberFactory}

class MetaDefaultSkuber extends AbstractModule with ScalaModule {

  override def configure(): Unit = {
    bind[SkuberFactory].to[DefaultSkuberFactory]
  }

}
