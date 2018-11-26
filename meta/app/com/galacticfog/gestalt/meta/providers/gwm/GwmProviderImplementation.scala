package com.galacticfog.gestalt.meta.providers.gwm

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.patch.PatchDocument

trait GwmProviderImplementation[F[_]] {
  def getPublicUrl(resource: GestaltResourceInstance): Option[String]

  def createApi(provider: GestaltResourceInstance, resource: GestaltResourceInstance): F[GestaltResourceInstance]
  def deleteApi(provider: GestaltResourceInstance, resource: GestaltResourceInstance): F[Unit]
  
  def createEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance): F[GestaltResourceInstance]
  def updateEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance, patch: PatchDocument): F[GestaltResourceInstance]
  def deleteEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance): F[Unit]
}