package modules

import com.google.inject.AbstractModule
import com.ning.http.client.AsyncHttpClientConfigBean
import net.codingwell.scalaguice.ScalaModule
import play.api.libs.concurrent.AkkaGuiceSupport
import play.api.libs.ws.WSClient
import play.api.libs.ws.ning.NingWSClient
import services.{DCOSAuthTokenActor, DefaultMarathonClientFactory, MarathonClientFactory}

class MetaDefaultDCOS extends AbstractModule with ScalaModule with AkkaGuiceSupport {

  override def configure(): Unit = {
    bind[MarathonClientFactory].to[DefaultMarathonClientFactory]
    bindActor[DCOSAuthTokenActor](DCOSAuthTokenActor.name)

    val config = new AsyncHttpClientConfigBean()
    config.setAcceptAnyCertificate(true)
    config.setFollowRedirect(true)
    val permissiveClient = NingWSClient(config)
    bind[WSClient].annotatedWithName("permissive-wsclient").toInstance(permissiveClient)
  }



}
