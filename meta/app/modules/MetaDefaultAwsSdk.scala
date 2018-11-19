package modules

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule
import services.{DefaultAwsSdkFactory, AwsSdkFactory}

class MetaDefaultAwsSdk extends AbstractModule with ScalaModule {

  override def configure(): Unit = {
    bind[AwsSdkFactory].to[DefaultAwsSdkFactory]
    ()
  }

}
