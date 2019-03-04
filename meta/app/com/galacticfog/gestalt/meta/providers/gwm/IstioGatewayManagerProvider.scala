package com.galacticfog.gestalt.meta.providers.gwm

import scala.concurrent.Future
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.patch.PatchDocument
// import com.galacticfog.gestalt.util.ResourceSerde
// import com.galacticfog.gestalt.util.Error
// import cats.implicits._

class IstioGatewayManagerProvider extends GwmProviderImplementation[Future] {
  // import GwmSpec.Implicits._

  def getPublicUrl(resource: GestaltResourceInstance): Option[String] = ???

  def createApi(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    ???
  }
  def deleteApi(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[Unit] = {
    ???
  }
  
  def createEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    ???
  }
  def updateEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance, patch: PatchDocument): Future[GestaltResourceInstance] = ???
  def deleteEndpoint(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[Unit] = ???
}