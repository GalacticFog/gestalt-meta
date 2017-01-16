package services

import java.util.UUID

import scala.language.postfixOps

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.meta.api.errors._
import play.api.mvc.Request


case class ProviderContext(
    request: Request[_],
    providerId: UUID,
    target: Option[GestaltResourceInstance]) extends GestaltProviderService{
  
  val m = mapUri(request.uri)

  val fqon          = m(Resource.Fqon)
  val environmentId = UUID.fromString(m(Resource.ParentId))
  
  lazy val provider    = resource(ResourceIds.Provider, providerId)
  lazy val environment = resource(ResourceIds.Environment, environmentId)
  lazy val workspace   = parent(ResourceIds.Workspace, environmentId)
  
  def withTarget(resource: GestaltResourceInstance): ProviderContext =
    this.copy(target = Some(resource))
    
  private def parent(typeId: UUID, child: UUID) = ResourceFactory.findParent(typeId: UUID, child) getOrElse {
    throw new UnprocessableEntityException(notFound(typeId, child))
  }
  
  private def resource(typeId: UUID, id: UUID) = ResourceFactory.findById(id) getOrElse {
    throw new UnprocessableEntityException(s"Resource with ID '$id' not found.")
  }
  
  private def notFound(typeId: UUID, id: UUID): String = {
    s"${ResourceLabel(typeId)} with ID '$id' not found."
  }
  
  private def mapUri(uri: String) = {
    if (Resource.isList(uri)) Resource.mapListPathData(uri)
    else Resource.mapPathData(uri)
  }    
}
