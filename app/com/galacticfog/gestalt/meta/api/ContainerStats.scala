package com.galacticfog.gestalt.meta.api

import org.joda.time.DateTime
import play.api.libs.json.Json

case class ContainerStats(external_id: String,
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
                          taskStats: Option[Seq[ContainerStats.TaskStat]] )

case object ContainerStats {
  case class TaskStat( id: String,
                       host: String,
                       ipAddresses: Option[Seq[TaskStat.IPAddress]],
                       ports: Seq[Int],
                       startedAt: Option[String] )

  case object TaskStat {
    case class IPAddress(ipAddress: String, protocol: String)
  }


  implicit val formatIPAddress = Json.format[TaskStat.IPAddress]
  implicit val formatTaskStat = Json.format[TaskStat]
}

