package com.galacticfog.gestalt.marathon

// modified from marathon source

case class Container(docker: Option[Container.Docker],
                     `type`: String,
                     volumes: Seq[Container.Volume]) {

  val portMappings: Option[Seq[Container.Docker.PortMapping]] = docker flatMap (_.network) match {
    case Some(networkMode) if networkMode == Container.Docker.Network.BRIDGE =>
      docker.flatMap(_.portMappings).map(_.map {
        // backwards compat: when in BRIDGE mode, missing host ports default to zero
        case Container.Docker.PortMapping(x, None, y, z, w, a) => Container.Docker.PortMapping(x, Some(Container.Docker.PortMapping.HostPortDefault), y, z, w, a)
        case m => m
      })
    case Some(networkMode) if networkMode == Container.Docker.Network.USER => docker.flatMap(_.portMappings)
    case _ => None
  }

  def servicePorts: Option[Seq[Int]] =
    for (pms <- portMappings) yield pms.flatMap(_.servicePort)
}

case object Container {

  case class Docker(image: String,
                    network: Option[String],
                    forcePullImage: Option[Boolean],
                    parameters: Option[Seq[Docker.Parameter]],
                    portMappings: Option[Seq[Docker.PortMapping]],
                    privileged: Option[Boolean])

  case object Docker {

    case class Parameter(key: String, value: String)

    /**
      * @param containerPort The container port to expose
      * @param hostPort      The host port to bind
      * @param servicePort   The well-known port for this service
      * @param protocol      Layer 4 protocol to expose (i.e. "tcp", "udp" or "udp,tcp" for both).
      * @param name          Name of the service hosted on this port.
      * @param labels        This can be used to decorate the message with metadata to be
      *                      interpreted by external applications such as firewalls.
      */
    case class PortMapping( containerPort: Option[Int] = None,
                            hostPort: Option[Int] = None,
                            servicePort: Option[Int] = None,
                            protocol: Option[String] = None,
                            name: Option[String] = None,
                            labels: Option[Map[String, String]] = None)

    object PortMapping {
      val TCP = "tcp"
      val UDP = "udp"

      val HostPortDefault = AppDefinition.RandomPortValue // HostPortDefault only applies when in BRIDGE mode
    }

    case object Network {
      val BRIDGE = "BRIDGE"
      val HOST = "HOST"
      val USER = "USER"
    }

  }

  case class PersistentVolumeInfo(size: Long)

  case class Volume(containerPath: String,
                    hostPath: Option[String],
                    persistent: Option[PersistentVolumeInfo],
                    mode: String) {
    def isPersistent: Boolean = persistent.isDefined
  }

}
