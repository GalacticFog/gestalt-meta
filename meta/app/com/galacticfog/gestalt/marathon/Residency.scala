package com.galacticfog.gestalt.marathon

case class Residency(relaunchEscalationTimeoutSeconds: Long, taskLostBehavior: String)

/**
  * Created by cgbaker on 10/4/16.
  */
object Residency {
  def default: Residency = Residency(defaultRelaunchEscalationTimeoutSeconds, defaultTaskLostBehaviour)
  val defaultTaskLostBehaviour = "WAIT_FOREVER"
  val defaultRelaunchEscalationTimeoutSeconds: Long = 3600
  val defaultResidency: Residency = Residency(defaultRelaunchEscalationTimeoutSeconds, defaultTaskLostBehaviour)
}
