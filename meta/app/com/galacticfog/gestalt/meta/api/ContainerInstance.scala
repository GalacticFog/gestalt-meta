package com.galacticfog.gestalt.meta.api

class ContainerInstance(spec: ContainerSpec) extends SpecInstance[ContainerSpec] {
  override def getSpec: ContainerSpec = spec

  def isHealthy: Boolean = false // TODO
  def isRunning: Boolean = false // TODO
  def isStaged: Boolean = false // TODO
  def isUnhealthy: Boolean = false // TODO
}
