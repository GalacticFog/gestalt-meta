package com.galacticfog.gestalt.marathon

import play.api.libs.json.JsValue

import scala.concurrent.duration._

case class AppUpdate(id: Option[String] = None,
                     container: Option[Container] = None,
                     cpus: Option[Double] = None,
                     mem: Option[Double] = None,
                     disk: Option[Double] = None,
                     instances: Option[Int] = None,
                     cmd: Option[String] = None,
                     constraints: Option[Seq[Seq[String]]] = None,
                     acceptedResourceRoles: Option[Seq[String]] = None,
                     args: Option[Seq[String]] = None,
                     portDefinitions: Option[Seq[AppUpdate.PortDefinition]] = None,
                     labels: Option[Map[String, String]] = None,
                     healthChecks: Option[Seq[AppUpdate.HealthCheck]] = None,
                     env: Option[Map[String, AppInfo.EnvVarValue]] = None,
                     ipAddress: Option[AppUpdate.IPPerTaskInfo] = None,
                     upgradeStrategy: Option[UpgradeStrategy] = None,
                     user: Option[String] = None,
                     secrets: Option[Map[String,AppUpdate.SecretSource]] = None)

case object AppUpdate {

  case class HealthCheck( protocol: Option[String],
                          path: Option[String],
                          portIndex: Option[Int],
                          port: Option[Int],
                          gracePeriodSeconds: Option[Int],
                          intervalSeconds: Option[Int],
                          timeoutSeconds: Option[Int],
                          maxConsecutiveFailures: Option[Int],
                          command: Option[JsValue] = None
                        )

  case object HealthCheck {
    val DefaultProtocol = "HTTP"
    val DefaultPath = "/"
    val DefaultGracePeriod = 5.minutes
    val DefaultInterval = 1.minute
    val DefaultTimeout = 20.seconds
    val DefaultMaxConsecutiveFailures = 3
  }

  case class PortDefinition( port: Int,
                             protocol: String = "tcp",
                             name: Option[String] = None,
                             labels: Map[String,String] = Map.empty )

  case class IPPerTaskInfo(discovery: Option[IPPerTaskInfo.DiscoveryInfo], networkName: Option[String] = None)

  case object IPPerTaskInfo {
    case class DiscoveryInfo(ports: Option[Seq[DiscoveryInfo.PortDiscovery]] = None)

    case object DiscoveryInfo {
      case class PortDiscovery(number: Int, name: String, protocol: String)
    }
  }

  case class SecretSource(source: String)

}
