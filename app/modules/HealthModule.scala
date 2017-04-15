package modules

import javax.inject.Inject
import com.google.inject.AbstractModule
import controllers.util.MetaHealth
import net.codingwell.scalaguice.ScalaModule
import play.api.Logger
import play.api.libs.json.{JsValue, Json}


class HealthModule extends AbstractModule with ScalaModule {
  override def configure(): Unit = {
    bind[MetaServiceStatus].asEagerSingleton()
  }
}

class MetaServiceStatus @Inject()(metaHealth: MetaHealth) {

  val log = Logger(this.getClass)

  private[this] def setServiceStatus() = {
    log.info("Checking Meta service health...")
    health = metaHealth.selfCheck(verbose = true) match {
      case Left(error) => {
        log.warn("Service health is compromised.")
        error
      }
      case Right(info) => {
        log.info("Meta is healthy.")
        info
      }
    }
    log.info("Self-check results:\n" + Json.prettyPrint(health))
    status = (health \ "status").as[String]
  }

  private[this] var status: String = null
  private[this] var health: JsValue = null

  setServiceStatus()
}
