package com.galacticfog.gestalt.marathon

case class Container(docker: Option[Container.Docker],
                     `type`: String,
                     volumes: Seq[Container.Volume])

case object Container {

  case class Docker(image: String,
                    network: Option[String],
                    forcePullImage: Option[Boolean],
                    parameters: Option[Seq[Docker.Parameter]],
                    portMappings: Option[Seq[Docker.PortMapping]],
                    privileged: Option[Boolean])

  case object Docker {

    case class Parameter(key: String, value: String)

    case class PortMapping(protocol: Option[String] = Some("tcp"),
                           containerPort: Int,
                           labels: Option[Map[String,String]] = None,
                           hostPort: Option[Int] = Some(0),
                           servicePort: Option[Int] = Some(0))

  }

  case class PersistentVolumeInfo(size: Long)

  case class Volume(containerPath: String,
                    hostPath: Option[String],
                    persistent: Option[PersistentVolumeInfo],
                    mode: String) {
    def isPersistent: Boolean = persistent.isDefined
  }

}
