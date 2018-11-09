package com.galacticfog.gestalt.meta.providers.faas

import scala.concurrent.Future
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.patch.PatchDocument

class AWSLambdaProvider extends FaasProviderImplementation[Future] {
  def createLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = ???
  def updateLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance, patch: PatchDocument): Future[GestaltResourceInstance] = ???
  def deleteLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[Unit] = ???
}