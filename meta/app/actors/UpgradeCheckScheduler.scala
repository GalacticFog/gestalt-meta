package actors


import javax.inject.{Inject, Named}
import akka.actor.{ActorRef, ActorSystem}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import play.api.Logger

/**
 * Scheduler that starts the UpgradeCheckTask to check for Meta upgrades.
 */
class UpgradeCheckScheduler @Inject() (
    val actorSystem: ActorSystem, 
    @Named(UpgradeCheckTask.name) val task: ActorRef)(implicit ec: ExecutionContext) {

  private val log = Logger(this.getClass)

  if (!UpgradeVars.checksEnabled) {
    log.info("Automatic upgrade checking is disabled.")
  } else {
    UpgradeVars.baseUrl match {
      case None => {
        log.warn(s"Env var ${UpgradeVars.UrlName} not found. Upgrade checking WILL NOT be performed.")
        log.warn(s"To resume upgrade-checks, set ${UpgradeVars.UrlName} in the environment and restart Meta")
        ()      
      }
      case Some(url) => {
        log.info(s"Upgrade Check configured for every ${UpgradeVars.interval} hours at ${UpgradeVars.baseUrl}")
        val actor = actorSystem.scheduler.schedule(
            initialDelay = 5.seconds, 
            interval = UpgradeVars.interval.seconds, task, "check")      
      }
    }
  }

}