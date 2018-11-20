package com.galacticfog.gestalt.meta.providers.faas

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.patch.PatchDocument

trait FaasProviderImplementation[F[_]] {
  def createLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): F[GestaltResourceInstance]
  def updateLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance, patch: PatchDocument): F[GestaltResourceInstance]
  def deleteLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): F[Unit]
  def importLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): F[GestaltResourceInstance]
}