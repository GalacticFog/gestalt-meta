package modules

import akka.stream.Materializer
import com.google.inject.{AbstractModule, Inject, Singleton}
import io.netty.handler.ssl.SslContextBuilder
import net.codingwell.scalaguice.ScalaModule
import org.asynchttpclient.DefaultAsyncHttpClientConfig
import org.asynchttpclient.netty.ssl.InsecureTrustManagerFactory
import play.api.libs.concurrent.AkkaGuiceSupport
import play.api.libs.ws.WSClient
import play.api.libs.ws.ahc.AhcWSClient
import services.{DCOSAuthTokenActor, DefaultMarathonClientFactory, MarathonClientFactory}

@Singleton
class MetaDefaultDCOS @Inject()(mat: Materializer) extends AbstractModule with ScalaModule with AkkaGuiceSupport {

  override def configure(): Unit = {
    bind[MarathonClientFactory].to[DefaultMarathonClientFactory]
    bindActor[DCOSAuthTokenActor](DCOSAuthTokenActor.name)

    val permissiveClient = {
      val config = new DefaultAsyncHttpClientConfig.Builder()
        .setFollowRedirect(true)
        .setSslContext(SslContextBuilder.forClient.trustManager(InsecureTrustManagerFactory.INSTANCE).build)
        .build()
      new AhcWSClient(config)(mat)
    }

    bind[WSClient].annotatedWithName("permissive-wsclient").toInstance(permissiveClient)
  }

}
