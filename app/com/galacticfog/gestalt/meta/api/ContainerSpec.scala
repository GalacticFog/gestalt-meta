package com.galacticfog.gestalt.meta.api

import java.util.UUID

case class ContainerSpec(container_type: String,
                         image: String,
                         provider: ContainerSpec.InputProvider,
                         port_mappings: Seq[ContainerSpec.PortMapping] = Seq(),
                         cpus: Double = 0.2,
                         memory: Double = 128.0,
                         disk: Double = 0.0,
                         num_instances: Int = 1,
                         network: Option[String] = None,
                         cmd: Option[String] = None,
                         constraints: Seq[String] = Seq(),
                         accepted_resource_roles: Option[Seq[String]] = None,
                         args: Option[Seq[String]] = None,
                         force_pull: Boolean = false,
                         health_checks: Seq[ContainerSpec.HealthCheck] = Seq(),
                         volumes: Seq[ContainerSpec.VolumeSpec] = Seq(),
                         labels: Map[String,String] = Map(),
                         env: Map[String,String] = Map(),
                         user: Option[String] = None)

case object ContainerSpec {

  case class PortMapping(protocol: String,
                         container_port: Int,
                         host_port: Int = 0,
                         service_port: Int = 0,
                         name: Option[String] = None,
                         labels: Map[String,String] = Map())

  case class VolumeSpec(
                         container_path: String,
                         host_path: Option[String],
                         persistent: Option[VolumeSpec.PersistentVolumeInfo],
                         mode: String) {
    def isPersistent: Boolean = persistent.isDefined
  }

  case object VolumeSpec {

    case class PersistentVolumeInfo(size: Long)

  }

  case class InputProvider(id: UUID,
                           name: Option[String] = None,
                           locations: Option[Seq[String]] = None)

  // TODO: Marathon health checks require a port index, to specify which port the check is run against
  // ours aren't well defined without something similar, like a label
  case class HealthCheck(protocol: String,
                         path: String,
                         grace_period_seconds: Int = 300,
                         interval_seconds: Int = 60,
                         timeout_seconds: Int = 10,
                         max_consecutive_failures: Int = 3)


}
