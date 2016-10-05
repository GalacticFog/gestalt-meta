package com.galacticfog.gestalt.marathon

import scala.collection.immutable.Seq
import scala.concurrent.duration._

// modified from marathon source code

case class AppDefinition(
                          id: String = "",

                          cmd: Option[String] = AppDefinition.DefaultCmd,

                          args: Option[Seq[String]] = AppDefinition.DefaultArgs,

                          user: Option[String] = AppDefinition.DefaultUser,

                          env: Map[String, String] = AppDefinition.DefaultEnv,

                          instances: Int = AppDefinition.DefaultInstances,

                          cpus: Double = AppDefinition.DefaultCpus,

                          mem: Double = AppDefinition.DefaultMem,

                          disk: Double = AppDefinition.DefaultDisk,

                          gpus: Int = AppDefinition.DefaultGpus,

                          executor: String = AppDefinition.DefaultExecutor,

                          constraints: Seq[Seq[String]] = AppDefinition.DefaultConstraints,

                          // fetch: Seq[FetchUri] = AppDefinition.DefaultFetch,

                          // storeUrls: Seq[String] = AppDefinition.DefaultStoreUrls,

                          portDefinitions: Seq[AppUpdate.PortDefinition] = AppDefinition.DefaultPortDefinitions,

                          requirePorts: Boolean = AppDefinition.DefaultRequirePorts,

                          backoff: FiniteDuration = AppDefinition.DefaultBackoff,

                          backoffFactor: Double = AppDefinition.DefaultBackoffFactor,

                          maxLaunchDelay: FiniteDuration = AppDefinition.DefaultMaxLaunchDelay,

                          container: Option[Container] = AppDefinition.DefaultContainer,

                          healthChecks: Seq[_ <: AppUpdate.HealthCheck] = AppDefinition.DefaultHealthChecks,

                          // readinessChecks: Seq[ReadinessCheck] = AppDefinition.DefaultReadinessChecks,

                          taskKillGracePeriod: Option[FiniteDuration] = AppDefinition.DefaultTaskKillGracePeriod,

                          dependencies: Seq[String] = Seq.empty,

                          upgradeStrategy: UpgradeStrategy = AppDefinition.DefaultUpgradeStrategy,

                          labels: Map[String, String] = Map.empty,

                          acceptedResourceRoles: Option[Seq[String]] = None,

                          // ipAddress: Option[IpAddress] = None,

                          // versionInfo: VersionInfo = VersionInfo.NoVersion,

                          // version: Option[String] = None,

                          residency: Option[Residency] = AppDefinition.DefaultResidency

                          // secrets: Map[String, Secret] = AppDefinition.DefaultSecrets
                        ) {
  // extends RunSpec with plugin.RunSpec with MarathonState[Protos.ServiceDefinition, AppDefinition] {

  val portNumbers: Seq[Int] = portDefinitions.map(_.port)

  val isResident: Boolean = residency.isDefined

  val volumes: Iterable[Container.Volume] = container.fold(Iterable.empty[Container.Volume])(_.volumes.toIterable)
}

object AppDefinition {

  val RandomPortValue: Int = 0

  val RandomPortDefinition: AppUpdate.PortDefinition = AppUpdate.PortDefinition(RandomPortValue, "tcp", None, Map.empty[String, String])

  // App defaults
  // val DefaultId: PathId = PathId.empty

  val DefaultCmd: Option[String] = None

  val DefaultArgs: Option[Seq[String]] = None

  val DefaultUser: Option[String] = None

  val DefaultEnv: Map[String, String] = Map.empty

  val DefaultInstances: Int = 1

  val DefaultCpus: Double = 1.0

  val DefaultMem: Double = 128.0

  val DefaultDisk: Double = 0.0

  val DefaultGpus: Int = 0

  val DefaultExecutor: String = ""

  //  val DefaultConstraints: Seq[Constraint] = Seq.empty
  val DefaultConstraints: Seq[Seq[String]] = Seq.empty

  val DefaultUris: Seq[String] = Seq.empty

  //  val DefaultFetch: Seq[FetchUri] = FetchUri.empty

  val DefaultStoreUrls: Seq[String] = Seq.empty

  val DefaultPortDefinitions: Seq[AppUpdate.PortDefinition] = Seq(RandomPortDefinition)

  val DefaultRequirePorts: Boolean = false

  val DefaultBackoff: FiniteDuration = 1.second

  val DefaultBackoffFactor = 1.15

  val DefaultMaxLaunchDelay: FiniteDuration = 1.hour

  val DefaultContainer: Option[Container] = None

  val DefaultHealthChecks: Seq[AppUpdate.HealthCheck] = Seq.empty

  // val DefaultReadinessChecks: Seq[ReadinessCheck] = Seq.empty

  val DefaultTaskKillGracePeriod: Option[FiniteDuration] = None

  // val DefaultDependencies: Seq[PathId] = Seq.empty

  val DefaultUpgradeStrategy: UpgradeStrategy = UpgradeStrategy.empty

  // val DefaultSecrets: Map[String, Secret] = Map.empty

  val DefaultResidency: Option[Residency] = None

}



