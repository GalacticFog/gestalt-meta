package services


import java.util.UUID

import com.galacticfog.gestalt.events.{AmqpClient, AmqpConnection, AmqpEndpoint, PolicyEvent}

import play.api.{Logger => log}

import com.galacticfog.gestalt.events.{AmqpEndpoint, PolicyEvent, AmqpClient, AmqpConnection}

import play.api.Logger

import play.api.libs.ws.WS
import play.api.Play.current
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import com.galacticfog.gestalt.data.{Instance, ResourceFactory}
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, ResourceLike}
import com.galacticfog.gestalt.marathon.MarathonClient
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.errors.ResourceNotFoundException
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, ResourceIds}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.marathon._
import com.galacticfog.gestalt.meta.api.{ContainerInstance, ContainerSpec, Resource, ResourcePath}
import com.galacticfog.gestalt.events._
import com.google.inject.Inject

import skuber._
import skuber.api.client._
import skuber.json.format._
import skuber.ext._
import skuber.json.ext.format._
import org.yaml.snakeyaml._
import play.api.libs.json._
import skuber.api.client.ObjKind

import com.galacticfog.gestalt.caas.kube._

import controllers.util._
import com.galacticfog.gestalt.json.Js
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

//  def find(fqon: String, criteria: ***): Future[Option[***]]
//  
//  def findAll(fqon: String, criteria: ***): Future[Seq[***]]

  def create(context: ProviderContext, container: GestaltResourceInstance)
            (implicit ec: ExecutionContext): Future[GestaltResourceInstance]

  def destroyContainer(id: GestaltResourceInstance): Future[Unit]

//  
//  def start(id: UUID): Future[JsValue]
//  
//  def suspend(id: UUID): Future[JsValue]
//  
//  def restart(id: UUID): Future[JsValue]
//  
//  def scale(id: UUID, quantity: UUID): Future[JsValue]
//  
//  def migrate(id: UUID, to: ***): Future[JsValue]
//  
//  /**
//   * Convert input container JSON to a Meta Container Resource.
//   */
//  def toResource(fqon: String, user: AuthAccountWithCreds, input: JsValue): Try[GestaltResourceInstance]
//

}