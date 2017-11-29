package com.galacticfog.gestalt.meta.genericactions

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import scala.concurrent.Future
import scala.util.Try

trait GenericProviderManager {
  def getProvider(provider: GestaltResourceInstance): Try[GenericProvider]
}

trait GenericProvider {
  def invokeAction(context: GenericActionInvocation): Future[Either[GestaltResourceInstance,(Option[Int],Option[String],Option[String])]]
}



class DefaultGenericProviderManager extends GenericProviderManager {
  override def getProvider(provider: GestaltResourceInstance) = Try{???}
}
