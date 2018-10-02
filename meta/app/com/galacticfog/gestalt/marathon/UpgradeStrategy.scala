package com.galacticfog.gestalt.marathon

case class UpgradeStrategy(minimumHealthCapacity: Double,

                           maximumOverCapacity: Double)

case object UpgradeStrategy {
  def empty = UpgradeStrategy(1.0, 1.0)
}

