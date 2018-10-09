package com.galacticfog.gestalt.meta.api

import com.galacticfog.gestalt.meta.api.ContainerStats.{ContainerStateStat, EventStat}
import com.galacticfog.gestalt.util.Helpers.JodaJsonFormats._

import org.joda.time.DateTime
import play.api.libs.json._

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
                          taskStats: Option[Seq[ContainerStats.TaskStat]],
                          events: Option[Seq[EventStat]] = None,
                          states: Option[Seq[ContainerStateStat]] = None,
                          lb_address: Option[String]) {

  def getStatusDetail(): String = {
    val maybeStatusDetail = states.flatMap (_.sortBy(_.priority).lastOption)
    maybeStatusDetail.map(_.format()).getOrElse("")
  }
}

case object ContainerStats {
  implicit val formatIPAddress = Json.format[TaskStat.IPAddress]
  implicit val formatTaskStat = Json.format[TaskStat]
  implicit val formatEventStat = Json.format[EventStat]
  implicit val formatContainerStateStat = Json.format[ContainerStateStat]

  case class TaskStat( id: String,
                       host: String,
                       ipAddresses: Option[Seq[TaskStat.IPAddress]],
                       ports: Seq[Int],
                       startedAt: Option[String] )

  case object TaskStat {
    case class IPAddress(ipAddress: String, protocol: String)
  }

  case class EventStat(objectName: String,
                       objectType: String,
                       eventType: String,
                       reason: String,
                       age: DateTime,
                       sourceComponent: String,
                       sourceHost: String,
                       message: String )

  case class ContainerStateStat(objectName: String,
                                objectType: String,
                                stateId: String = "unknown",
                                reason: Option[String] = None,
                                message: Option[String] = None,
                                finishedAt: Option[DateTime] = None,
                                priority: Int = 0
                               ) {

    def format(): String = {
      priority match {
        case 0 => s"${stateId.toUpperCase}"
        case _ => {
          val objectPart = s"${objectType} ${objectName}"
          val statePart = s" is in ${stateId.toUpperCase} state."
          val maybeMessagePart = message.map(message => s" (${message})")
          val maybeDetailsPart = reason.map(reason => s" Reason: ${reason}${maybeMessagePart.getOrElse(".")}")
          objectPart + statePart + maybeDetailsPart.getOrElse("")
        }
      }
    }
  }
}

