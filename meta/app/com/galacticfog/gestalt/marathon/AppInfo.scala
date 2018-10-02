package com.galacticfog.gestalt.marathon

import scala.concurrent.duration._

// modified from marathon source code

case class AppInfo(
                    app: AppDefinition,
//                    maybeTasks: Option[Seq[EnrichedTask]] = None,
                    maybeCounts: Option[TaskCounts] = None,
                    maybeDeployments: Option[Seq[String]] = None
//                    maybeReadinessCheckResults: Option[Seq[ReadinessCheckResult]] = None,
//                    maybeLastTaskFailure: Option[TaskFailure] = None,
//                    maybeTaskStats: Option[TaskStatsByVersion] = None
                  )

object AppInfo {
  sealed trait Embed
  object Embed {
    case object Tasks extends Embed
    case object Deployments extends Embed
    case object Readiness extends Embed
    case object Counts extends Embed
    case object LastTaskFailure extends Embed
    case object TaskStats extends Embed
  }

  sealed trait EnvVarValue

  case class EnvVarString(value: String) extends EnvVarValue

  case class EnvVarSecretRef(secret: String) extends EnvVarValue

  object EnvVarValue {
    def apply(m: Map[String, String]): Map[String, EnvVarValue] =
      m.map { case (k, v) => k -> v.toEnvVar }

    implicit class FromString(val string: String) {
      def toEnvVar: EnvVarValue = EnvVarString(string)
    }
  }

}


case class AppDefinition(
                          id: String = "",

                          cmd: Option[String] = AppDefinition.DefaultCmd,

                          args: Option[Seq[String]] = AppDefinition.DefaultArgs,

                          user: Option[String] = AppDefinition.DefaultUser,

                          env: Map[String, AppInfo.EnvVarValue] = AppDefinition.DefaultEnv,

                          instances: Int = AppDefinition.DefaultInstances,

                          cpus: Double = AppDefinition.DefaultCpus,

                          mem: Double = AppDefinition.DefaultMem,

                          disk: Double = AppDefinition.DefaultDisk,

                          gpus: Int = AppDefinition.DefaultGpus,

                          executor: String = AppDefinition.DefaultExecutor,

                          constraints: Seq[Seq[String]] = AppDefinition.DefaultConstraints,

                          fetch: Seq[FetchUri] = Seq.empty[FetchUri],

                          storeUrls: Seq[String] = AppDefinition.DefaultStoreUrls,

                          portDefinitions: Seq[AppUpdate.PortDefinition] = AppDefinition.DefaultPortDefinitions,

                          requirePorts: Boolean = AppDefinition.DefaultRequirePorts,

                          backoff: FiniteDuration = AppDefinition.DefaultBackoff,

                          backoffFactor: Double = AppDefinition.DefaultBackoffFactor,

                          maxLaunchDelay: FiniteDuration = AppDefinition.DefaultMaxLaunchDelay,

                          container: Option[Container] = AppDefinition.DefaultContainer,

                          healthChecks: Seq[_ <: AppUpdate.HealthCheck] = AppDefinition.DefaultHealthChecks,

                          readinessChecks: Seq[ReadinessCheck] = AppDefinition.DefaultReadinessChecks,

                          taskKillGracePeriod: Option[FiniteDuration] = AppDefinition.DefaultTaskKillGracePeriod,

                          dependencies: Seq[String] = Seq.empty,

                          upgradeStrategy: UpgradeStrategy = AppDefinition.DefaultUpgradeStrategy,

                          labels: Map[String, String] = Map.empty,

                          acceptedResourceRoles: Option[Seq[String]] = None,

                          ipAddress: Option[IpAddress] = None,

                          // versionInfo: VersionInfo = VersionInfo.NoVersion,

                          version: Option[String] = None,

                          residency: Option[Residency] = AppDefinition.DefaultResidency,

                          secrets: Map[String, SecretSource] = AppDefinition.DefaultSecrets
                        ) {

  val portNumbers: Seq[Int] = portDefinitions.map(_.port)

  val isResident: Boolean = residency.isDefined

  val volumes: Iterable[Container.Volume] = container.fold(Iterable.empty[Container.Volume])(_.volumes.toIterable)

  val servicePorts: Seq[Int] = container.flatMap(_.servicePorts.filter(_.nonEmpty)).getOrElse(portNumbers)

}

case class FetchUri(uri: String)

case class ReadinessCheck(empty: String)

object AppDefinition {

  val RandomPortValue: Int = 0

  val RandomPortDefinition: AppUpdate.PortDefinition = AppUpdate.PortDefinition(RandomPortValue, "tcp", None, Map.empty[String, String])

  // App defaults
  // val DefaultId: PathId = PathId.empty

  val DefaultCmd: Option[String] = None

  val DefaultArgs: Option[Seq[String]] = None

  val DefaultUser: Option[String] = None

  val DefaultEnv: Map[String, AppInfo.EnvVarValue] = Map.empty

  val DefaultInstances: Int = 1

  val DefaultCpus: Double = 1.0

  val DefaultMem: Double = 128.0

  val DefaultDisk: Double = 0.0

  val DefaultGpus: Int = 0

  val DefaultExecutor: String = ""

  //  val DefaultConstraints: Seq[Constraint] = Seq.empty
  val DefaultConstraints: Seq[Seq[String]] = Seq.empty

  val DefaultUris: Seq[String] = Seq.empty

  val DefaultFetch: Seq[FetchUri] = Seq.empty

  val DefaultStoreUrls: Seq[String] = Seq.empty

  val DefaultPortDefinitions: Seq[AppUpdate.PortDefinition] = Seq(RandomPortDefinition)

  val DefaultRequirePorts: Boolean = false

  val DefaultBackoff: FiniteDuration = 1.second

  val DefaultBackoffFactor = 1.15

  val DefaultMaxLaunchDelay: FiniteDuration = 1.hour

  val DefaultContainer: Option[Container] = None

  val DefaultHealthChecks: Seq[AppUpdate.HealthCheck] = Seq.empty

   val DefaultReadinessChecks: Seq[ReadinessCheck] = Seq.empty

  val DefaultTaskKillGracePeriod: Option[FiniteDuration] = None

  // val DefaultDependencies: Seq[PathId] = Seq.empty

  val DefaultUpgradeStrategy: UpgradeStrategy = UpgradeStrategy.empty

  val DefaultSecrets: Map[String, SecretSource] = Map.empty

  val DefaultResidency: Option[Residency] = None

}

/**
  * @param tasksStaged snapshot of the number of staged tasks
  * @param tasksRunning snapshot of the number of running tasks
  * @param tasksHealthy snapshot of the number of healthy tasks (does not include tasks without health info)
  * @param tasksUnhealthy snapshot of the number of unhealthy tasks (does not include tasks without health info)
  */
case class TaskCounts(
                       tasksStaged: Int,
                       tasksRunning: Int,
                       tasksHealthy: Int,
                       tasksUnhealthy: Int) {
  def +(counts: TaskCounts): TaskCounts = {
    copy(
      tasksRunning = tasksRunning + counts.tasksRunning,
      tasksStaged = tasksStaged + counts.tasksStaged,
      tasksHealthy = tasksHealthy + counts.tasksHealthy,
      tasksUnhealthy = tasksUnhealthy + counts.tasksUnhealthy
    )
  }

  def -(counts: TaskCounts): TaskCounts = {
    copy(
      tasksRunning = tasksRunning - counts.tasksRunning,
      tasksStaged = tasksStaged - counts.tasksStaged,
      tasksHealthy = tasksHealthy - counts.tasksHealthy,
      tasksUnhealthy = tasksUnhealthy - counts.tasksUnhealthy
    )
  }
}

object TaskCounts {
  def zero: TaskCounts = TaskCounts(tasksStaged = 0, tasksRunning = 0, tasksHealthy = 0, tasksUnhealthy = 0)

//  def apply(appTasks: Iterable[Task], healthStatuses: Map[Task.Id, Seq[Health]]): TaskCounts = {
//    TaskCounts(TaskForStatistics.forTasks(Timestamp(0), appTasks, healthStatuses))
//  }

//  def apply(appTasks: Iterable[TaskForStatistics]): TaskCounts = {
//    TaskCounts(
//      tasksStaged = appTasks.count(_.staging),
//      tasksRunning = appTasks.count(_.running),
//      tasksHealthy = appTasks.count(_.healthy),
//      tasksUnhealthy = appTasks.count(_.unhealthy)
//    )
//  }
}

case class IpAddress(
                      groups: Seq[String] = Seq.empty,
                      labels: Map[String, String] = Map.empty[String, String],
                      discoveryInfo: DiscoveryInfo = DiscoveryInfo.empty,
                      networkName: Option[String] = None)

object IpAddress {
  def empty: IpAddress = IpAddress()
}

case class DiscoveryInfo(ports: Seq[DiscoveryInfo.Port] = Seq.empty) {
  def isEmpty: Boolean = DiscoveryInfo.empty.equals(this)
  def nonEmpty: Boolean = !isEmpty
}

object DiscoveryInfo {
  def empty: DiscoveryInfo = DiscoveryInfo()

  case class Port(
                   number: Int,
                   name: String,
                   protocol: String,
                   labels: Map[String, String] = Map.empty[String, String]) {
    require(Port.AllowedProtocols(protocol), "protocol can only be 'tcp' or 'udp'")
  }

  object Port {
    val AllowedProtocols: Set[String] = Set("tcp", "udp")
  }
}

object PortAssignment {
  /**
    * If you change this, please also update AppDefinition.json.
    */
  val PortNamePattern = """^[a-z0-9-]+$""".r
}

case class SecretSource(source: String)
