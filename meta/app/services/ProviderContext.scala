package services


import java.security.cert.X509Certificate
import java.util.UUID

import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel
import com.galacticfog.gestalt.meta.api.errors._
import play.api.mvc.RequestHeader
import play.api.Logger

/*
 * TODO: This class needs updating. 
 * 
 * https://gitlab.com/galacticfog/gestalt-meta/issues/636
 * 
 * - Remove the 'request' parameter with 'request.uri' - the object is only used
 * to get the URI - which is essentially the path to the environment where we'll create the
 * container. Request is difficult and unnecessary outside of a controller.
 * 
 * - Change 'workspace' to 'environmentParent' - because that's what we really need.
 */
case class ProviderContext( request: RequestHeader,
                            providerId: UUID,
                            target: Option[GestaltResourceInstance] ) extends GestaltProviderService{
  
  private[this] val log = Logger(this.getClass)
  
  val m = mapUri(request.uri) 
  val fqon = m(Resource.Fqon)
  
  /*
   * TODO: This is a hack - need a better way to definitively locate 
   * the environment-id in the resource map.
   */
  val environmentId = {
    /*
     * TODO: With the changes to mapUri(), ParentType should ALWAYS be defined.
     * The logic here does not make obvious that we'll end up with an environment
     * ID in environmentId. Extract this to a separate function that can be 
     * tested in isolation.
     * 
     * https://gitlab.com/galacticfog/gestalt-meta/issues/636
     */
    val parentType = m.get(Resource.ParentType)
    if (parentType.isDefined && parentType.get == "environments")
      UUID.fromString(m(Resource.ParentId))
    else UUID.fromString(m(Resource.TargetId))
  }
  
  lazy val provider    = resource(ResourceIds.Provider, providerId)
  lazy val environment = resource(ResourceIds.Environment, environmentId)
  
  /*
   * TODO: Hack - the dcos provider uses the 'workspace' name to build the
   * dcos path/name of the container. What it really needs is the name of
   * the environment parent, which has just been extended to allow providers
   * as parents.
   */
  lazy val workspace   = {
    val tpe = ResourceFactory.findParent(environmentId).get.typeId
    log.debug(s"Looking up parent for environment: ${environmentId}")
    log.debug(s"Parent Type: ${tpe} [${ResourceLabel(tpe)}]")

    parent(tpe, environmentId)
  }
  
  def withTarget(resource: GestaltResourceInstance): ProviderContext =
    this.copy(target = Some(resource))
  
  private def parent(typeId: UUID, child: UUID) = ResourceFactory.findParent(typeId: UUID, child) getOrElse {
    throw new UnprocessableEntityException(notFound(typeId, child))
  }
  
  private def resource(typeId: UUID, id: UUID) = ResourceFactory.findById(id) getOrElse {
    throw new UnprocessableEntityException(s"${ResourceLabel(typeId)} with ID '$id' not found.")
  }
  
  private def notFound(typeId: UUID, id: UUID): String = {
    s"${ResourceLabel(typeId)} with ID '$id' not found."
  }

  /**
   * Convert a URI to a Map of individual components. If the URI is in meta short-form,
   * meaning it doesn't contain parent data (/fqon/type/id), lookup the parent and
   * inject type and ID into the Map. This effectively 'expands' a first-level URI to
   * a second-level URI 
   * (i.e. `/{fqon}/containers/{id}` becomes `/{fqon}/environments/{id}/containers/{id}`
   */
  private[services] def mapUri(uri: String) = {
    val m = if (Resource.isList(uri)) Resource.mapListPathData(uri)
    else Resource.mapPathData(uri)
    
    m ++ (if (m.get(Resource.ParentType).isDefined)
      Map.empty
    else {
      val par = ResourceFactory.findParent(UUID.fromString(m(Resource.TargetId))).getOrElse {
        throw new RuntimeException(s"Could not find parent of resource ID '${m(Resource.TargetId)}'")
      }
      val restName = TypeFactory.findApiPrefix(par.typeId).getOrElse {
        throw new RuntimeException(s"Resource ID '${par.typeId}' either does not exist or has no api.prefix set.")
      }
      Map(Resource.ParentType -> restName, Resource.ParentId -> par.id.toString)      
    }) 
  }
  
}

case class FakeURI(url: String) extends RequestHeader {
  override def uri(): String = url
  def headers: play.api.mvc.Headers = play.api.mvc.Headers()
  def id: Long = -1L
  def method: String = "foo"
  def path: String = "foo"
  def queryString: Map[String,Seq[String]] = Map.empty
  def remoteAddress: String = "foo"
  def secure: Boolean = false
  def tags: Map[String,String] = Map.empty
  def version: String = "foo"
  override def clientCertificateChain: Option[Seq[X509Certificate]] = None
}



