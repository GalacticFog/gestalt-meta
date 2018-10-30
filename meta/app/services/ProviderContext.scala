package services


import java.security.cert.X509Certificate
import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
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
 * - Remove the 'request' parameter - the object is only used
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
    //if (UUID.fromString(m(Resource.ParentId)) != ResourceIds.Environment)
    if (m(Resource.ParentType) != "environments")
      UUID.fromString(m(Resource.TargetId))
    else UUID.fromString(m(Resource.ParentId))
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
    val parentid = UUID.fromString(m(Resource.ParentId))
    //val tpe = ResourceFactory.findById(parentid).get.typeId
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
  
  private def mapUri(uri: String) = {
    if (Resource.isList(uri)) Resource.mapListPathData(uri)
    else Resource.mapPathData(uri)
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



