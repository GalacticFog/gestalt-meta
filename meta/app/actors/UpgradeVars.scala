package actors

import play.api.libs.json._
import play.api.Logger
import scala.util.Try
import scala.concurrent.duration._
import com.galacticfog.gestalt.json.Js

/*
  [format of upgrade file]
  {
    "upgradeImage": "gcr.io/galactic-public-2018/upgradeFrom2.4.1TO2.4.2",
    "upgradeNotes": "http://docs.galacticfog.com/docs/patchnotes2.4.2.html",
    "severity": "recommended"
  }
  
  [when an upgrade is available]
  {
    "upgradeAvailable": true,
    "upgradeImage": "gcr.io/galactic-public-2018/upgradeFrom2.4.1TO2.4.2",
    "upgradeNotes": "http://docs.galacticfog.com/docs/patchnotes2.4.2.html",
    "severity" : "recommended"
  }
  
  [when there is no upgrade]
  {
    "upgradeAvailable": false
  }
*/

object UpgradeVars {
  
  val log = Logger(this.getClass)
  
  val META_CURRENT_VERSION = "2.4.1"
  val DEFAULT_UPGRADE_CHECK_INTERVAL_HOURS = 24
  val DEFAULT_UPGRADE_CHECK_TIMEOUT_SECONDS = 10.seconds
  val UPGRADE_CACHE_KEY = "meta.upgrade"
  val DEFAULT_UPGRADE_CHECK_ENABLED = true
  
  // Environment variable names
  val UrlName = "META_UPGRADE_URL"
  val CheckIntervalName = "META_UPGRADE_CHECK_HOURS"
  val EnableUpgradesName = "META_UPGRADE_CHECK_ENABLED"
  
  // Base URL to check for upgrade files
  val baseUrl: Option[String] = sys.env.get(UrlName)
  
  // Number of hours between checks for upgrade files - default: 24 hours.
  val interval: Int = {
    sys.env.get(CheckIntervalName).fold(DEFAULT_UPGRADE_CHECK_INTERVAL_HOURS) { n =>
      Try(n.toInt).toOption.getOrElse {
        log.warn(s"Bad ENV var ${CheckIntervalName}. found: ${n}, expected: Integer")
        log.info(s"Using default upgrade check interval of ${DEFAULT_UPGRADE_CHECK_INTERVAL_HOURS}.")
        DEFAULT_UPGRADE_CHECK_INTERVAL_HOURS
      }
    }
  }
  
  // Name of the next upgrade file to check for
  val fileName = META_CURRENT_VERSION + "_upgrade"
  
  // Fully-qualified URL to next upgrade file
  val url: Option[String] = baseUrl.map { u =>
    "%s/%s".format(u.stripSuffix("/"), fileName)
  }
  
  val checksEnabled: Boolean = {
    sys.env.get(EnableUpgradesName).fold(false) { given =>
      given.trim.toLowerCase match {
        case "true"  => true
        case "false" => false
        case _ => {
          log.warn(s"Bad value given for ENV var ${EnableUpgradesName}. found: ${given}, expected: true|false. Defaulting to 'true'")
          true
        }
      }
    }
  }  
}

case class UpgradeInfo(upgradeImage: String, upgradeNotes: String, severity: String)
object UpgradeInfo {
  implicit lazy val upgradeInfoFormat = Json.format[UpgradeInfo]
}
case class UpgradeMessage(
    upgradeAvailable: Boolean, 
    upgradeImage: Option[String] = None, 
    upgradeNotes: Option[String] = None, 
    severity: Option[String] = None)
object UpgradeMessage {
  implicit lazy val upgradeInfoFormat = Json.format[UpgradeMessage]
  
  def fromUpgradeInfo(info: UpgradeInfo): UpgradeMessage = {
    UpgradeMessage(true, Some(info.upgradeImage), Some(info.upgradeNotes), Some(info.severity))
  }
  
  def fromUpgradeInfo(info: JsObject): Try[UpgradeMessage] = {
    Js.parse[UpgradeInfo](info).map { info => fromUpgradeInfo(info) }
  }
}

