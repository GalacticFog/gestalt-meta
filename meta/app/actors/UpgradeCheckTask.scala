package actors

import javax.inject.Inject
import com.google.inject.Singleton

import akka.actor.Actor
import akka.actor.ActorSystem
import scala.concurrent.ExecutionContext

import play.api.libs.ws._
import play.api.Logger
import play.api.cache._

import scala.concurrent.Future
import scala.util.{Try,Success,Failure}
import scala.util.{Either,Left,Right}

/*
 * This is the task actor that is run on a schedule by UpgradeTaskScheduler
 */
@Singleton
class UpgradeCheckTask @Inject()(
    actorSystem: ActorSystem, ws: WSClient,
    @NamedCache("upgrade-cache") cache: CacheApi)(implicit ec: ExecutionContext) extends Actor {
    
  val log = Logger(this.getClass)
  
  override def receive: Receive = {
    case _ => {
      log.info("Received request to check for Meta upgrades...")
      val _ = performCheck map { status =>
        status match {
          case Left(s)  => log.error(s)
          case Right(_) => log.info("Upgrade check completed successfully.")
        }
      }
    }
  }

  /**
   * Perform the check for a Meta upgrade file. If found the upgrade info is
   * written to the 'upgrade-cache' where it can be picked up by the /upgradeavailable
   * endpoint at any time.
   */
  protected def performCheck(): Future[Either[String, Unit]] = {
    UpgradeVars.url match {
      case None => Future(Left(s"Could not find URL to perform upgrade check. Nothing to do."))
      case Some(url) => {
        log.info("Checking for Meta upgrade at: ${url}")
        val response = ws.url(url)
          .withRequestTimeout(UpgradeVars.DEFAULT_UPGRADE_CHECK_TIMEOUT_SECONDS)
          .get()
          
        response.map { response =>
          if (response.status == 404) {
            log.info(s"No upgrade found at ${UpgradeVars.url}")
            Right(())
          } else {
            Try(cache.set(UpgradeVars.UPGRADE_CACHE_KEY, response.json)) match {
              case Failure(e) => Left(s"Failed writing upgrade info to cache: ${e.getMessage}")
              case Success(x) => {
                log.info(s"New upgrade file found. Data written to cache.")
                Right(())
              }
            }
          }
        } recover {
          case a: java.net.ConnectException => Left(s"Connection Refused at ${url}")
          case b: java.net.UnknownHostException => Left(s"Unknown Host: ${url}")
          case e => Left(e.getMessage)
        }
      }
    }

  }
}

object UpgradeCheckTask {
  final val name = "meta-upgrade-check-actor"
}