package com.galacticfog.gestalt.marathon

import org.joda.time.DateTime

case class ContainerStats(id: String,
                          containerType: String,
                          status: String,
                          cpus: Double,
                          memory: Double,
                          image: String,
                          age: DateTime,
                          numInstances: Int,
                          tasksStaged: Int,
                          tasksRunning: Int,
                          tasksHealthy: Int,
                          tasksUnhealthy: Int,
                          taskStats: Option[Seq[ContainerStats.TaskStat]]
                         )

case object ContainerStats {
  case class TaskStat(host: String,
                      ipAddresses: Option[Seq[TaskStat.IPAddress]],
                      ports: Seq[Int],
                      startedAt: Option[DateTime])

  case object TaskStat {
    case class IPAddress(ipAddress: String, protocol: String)
  }
}

