package services

import scala.concurrent.Future
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.marathon._
import scala.concurrent.ExecutionContext

trait GestaltProviderService

trait CaasService extends GestaltProviderService {

  type *** = Any

  type CaasContainer = ***
  type CaasPod = ***
  type CaasService = ***
  type CaasDeployment = ***

  def find(context: ProviderContext, container: GestaltResourceInstance): Future[Option[ContainerStats]]

  def listInEnvironment(context: ProviderContext): Future[Seq[ContainerStats]]

//  def findAll(fqon: String, criteria: ***): Future[Seq[***]]

  def create(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def destroyContainer(container: GestaltResourceInstance): Future[Unit]

//  def start(id: UUID): Future[JsValue]
//  
//  def restart(id: UUID): Future[JsValue]

  def scale(context: ProviderContext, container: GestaltResourceInstance, numInstances: Int): Future[GestaltResourceInstance]

//  /**
//   * Convert input container JSON to a Meta Container Resource.
//   */
//  def toResource(fqon: String, user: AuthAccountWithCreds, input: JsValue): Try[GestaltResourceInstance]

}