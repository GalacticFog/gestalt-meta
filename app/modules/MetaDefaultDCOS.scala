package modules

import akka.stream.Materializer
import javax.inject.{Inject, Singleton}
import com.google.inject.{AbstractModule, Provider}
import io.netty.handler.ssl.SslContextBuilder
import net.codingwell.scalaguice.ScalaModule
import org.asynchttpclient.DefaultAsyncHttpClientConfig
import org.asynchttpclient.netty.ssl.InsecureTrustManagerFactory
import play.api.libs.concurrent.AkkaGuiceSupport
import play.api.libs.ws.WSClient
import play.api.libs.ws.ahc.AhcWSClient
import services.{DCOSAuthTokenActor, DefaultMarathonClientFactory, MarathonClientFactory}

class MetaDefaultDCOS extends AbstractModule with ScalaModule with AkkaGuiceSupport {

  override def configure(): Unit = {
    bind[MarathonClientFactory].to[DefaultMarathonClientFactory]
    bindActor[DCOSAuthTokenActor](DCOSAuthTokenActor.name)
    bind[WSClient].annotatedWithName("permissive-wsclient").toProvider[InsecureClientProvider]
  }

}

@Singleton
class InsecureClientProvider @Inject()(mat: Materializer) extends Provider[WSClient] {
  override def get(): WSClient = {
    val config = new DefaultAsyncHttpClientConfig.Builder()
      .setFollowRedirect(true)
      .setSslContext(SslContextBuilder.forClient.trustManager(InsecureTrustManagerFactory.INSTANCE).build)
      .build()
    new AhcWSClient(config)(mat)
  }
}
