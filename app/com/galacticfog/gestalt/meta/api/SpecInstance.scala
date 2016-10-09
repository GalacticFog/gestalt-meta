package com.galacticfog.gestalt.meta.api

trait SpecInstance[A <: Spec] {
  def getSpec: A
}
