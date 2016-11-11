package controllers.util


import java.util.UUID
import java.net.URL
import play.api.http.HttpVerbs
import play.api.libs.ws.WS
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{ PatchOp, PatchDocument, PatchHandler }
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController

import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db._

import play.api.Logger
import play.api.libs.json._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.meta.api._
import play.api.mvc.Result
import play.api.mvc.Action
import com.galacticfog.gestalt.laser._
import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import com.galacticfog.gestalt.meta.api.sdk.ApiResponse
import com.galacticfog.gestalt.meta.auth.Actions 
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.keymgr.GestaltLicense
import com.galacticfog.gestalt.keymgr.GestaltFeature

import com.galacticfog.gestalt.events._
import com.galacticfog.gestalt.meta.policy._


abstract class Operation[T](val args: T) {
  def proceed(opts: RequestOptions): OperationResponse[Option[UUID]]
}

case class Feature(override val args: String*) extends Operation(args) {
  private[this] val log = Logger(this.getClass)
  
  def proceed(opts: RequestOptions) = {
    log.debug("Checking license for feature: " + args(0))
    val feature = s2f(args(0))
    
    if (GestaltLicense.instance.isFeatureActive(feature)) Continue
    else Halt("Feature is not licensed.")
  }
  
  private def s2f(s: String): GestaltFeature = GestaltFeature.valueOf(s)
}


case class Authorize(override val args: String*) extends Operation(args) with AuthorizationMethods {
  private[this] val log = Logger(this.getClass)
  
  def proceed(opts: RequestOptions) = {
    
    val user = opts.user
    val action = args(0)
    val target = opts.authTarget.get
    
    log.debug(s"Checking Authorization : action=$action, user=${user.account.id}, target=$target")
    
    isAuthorized(target, action, user) match {
      case Success(_) => Continue
      case Failure(e) => Halt(e.getMessage)
    }
  }
}


case class PolicyCheck(override val args: String*) extends Operation(args) {
  
  private val log = Logger(this.getClass)
  
  def proceed(opts: RequestOptions) = {
    log.debug(s"PolicyCheck[${args.mkString(",")}]")
    
    val user = opts.user
    
    // policyOwner is the resource on which the policy was set.
    val policyOwner = ResourceFactory.findById(opts.policyOwner.get) getOrElse {
      throw new ResourceNotFoundException(s"Given policy-owner '${opts.policyOwner.get}' not found.")
    }
    
    // This is the resource we're executing policy against.    
    val target = opts.policyTarget getOrElse policyOwner
    val eventName = args(0)
    
    evaluateLimitRules(user, policyOwner, Option(target), eventName, opts) match {
      case Success(_) => Continue
      case Failure(e) => Halt(e.getMessage)
    }
  }
  
  def evaluateLimitRules(
      user: AuthAccountWithCreds, 
      policyOwner: GestaltResourceInstance, 
      target: Option[GestaltResourceInstance], 
      action: String,
      opts: RequestOptions): Try[Unit] = Try {
    
    effectiveRules(policyOwner.id, Some(ResourceIds.RuleLimit), args) foreach { rule =>
      val decision = decideJson(user, (target getOrElse policyOwner), rule, None, opts)
      
      if (decision.isLeft) {
        //val message = s"Failed Policy Assertion: ${predicateMessage(predicate)}. Rule: ${rule.id}"
        throw new ConflictException(decision.left.get)
      }
    }
  }
  
  private def predicateMessage(p: Predicate[Any]) = {
    val value = p.value.toString.replaceAll("\"", "")
    "[%s %s %s]".format(p.property, p.operator, value)
  }
}

private object EventType {
  val Pre = "pre"
  val Post = "post"
}

trait EventMethods {
  
  private[this] val log = Logger(this.getClass)
  
  def findEffectiveEventRules(parentId: UUID, event: Option[String] = None): Option[GestaltResourceInstance] = {
    //val policies = ResourceFactory.findChildrenOfType(ResourceIds.Policy, parentId)//ResourceFactory.findAncestorsOfSubType(ResourceIds.Policy, parentId)
    
    val rs = for {
      p <- ResourceFactory.findChildrenOfType(ResourceIds.Policy, parentId)
      r <- ResourceFactory.findChildrenOfType(ResourceIds.RuleEvent, p.id)
    } yield r
    
    log.debug(s"Found ${rs.size} Event Rules:")
    
    rs foreach { r => log.debug(r.name) }
    
    val fs = if (event.isEmpty) rs else {
      rs filter { _.properties.get("actions").contains(event.get) }
    }
    // TODO: This is temporary. Need a strategy for multiple matching rules.
    if (fs.isEmpty) None else Some(fs(0))
  }
  
  
  def publishEvent( 
      actionName: String, 
      eventType: String,
      opts: RequestOptions): Try[OperationResponse[Option[UUID]]] = Try {
    
    val eventName = s"${actionName}.${eventType}"
    
    val target = opts.policyOwner.get //opts.policyTarget.get.id
    
    findEffectiveEventRules(target, Option(eventName)) match { 
      case None => {
        log.debug(s"***No events matching $eventName found.")
        Continue
      }
      case Some(rule) => {
        
        val event = EventMessage.make(
              id       = UUID.randomUUID, 
              identity = opts.user.account.id, 
              /*resource = "http://dummy.href", // TODO: Add RequestOptions.host*/
              event    = eventName, 
              action   = actionName, 
              rule     = rule, 
              payload  = opts.policyTarget)
        
        publishEvent(event, opts) match {
          case Success(_) => Accepted
          case Failure(e) => Halt(e.getMessage)
        }
      }
    }  
  }  
  
  
  //
  // TODO: Clean this up once we know the message works!
  //
  def publishEvent(event: EventMessage, opts: RequestOptions): Try[Unit] = {
    val json = event.toJson

    val eventJson = opts.data.fold(json) { dat =>
      val rule = (json \ "args" \ "rule").as[JsObject]
      val payload = (json \ "args" \ "payload").as[JsObject]
      val payloadPlus = dat.foldLeft[JsObject](payload)(
        (p, kv) => p ++ Json.obj(kv._1 -> kv._2)
      )
      json ++ Json.obj(
        "args" -> Json.obj(
          "rule" -> rule,
          "payload" -> payloadPlus
        )
      )
    }
    log.debug("Publishing event message:\n" + Json.prettyPrint(eventJson))
    eventsClient.publish(AmqpEndpoint(RABBIT_EXCHANGE, RABBIT_ROUTE), eventJson/*event.toJson*/)
  }
  
  def eventsClient() = {  
    AmqpClient(AmqpConnection(RABBIT_HOST, RABBIT_PORT, heartbeat = 300))
  }
}

case class EventsPre(override val args: String*) extends Operation(args)  with EventMethods {

  private val log = Logger(this.getClass)
  
  def proceed(opts: RequestOptions) = {
    log.debug(s"EventsPre[${args.mkString(",")}]")
    
    val user = opts.user
    
    val actionName = args(0)
    val eventName = s"${actionName}.${EventType.Pre}"
    
    // policyOwner is the resource on which the policy was set.
    val policyOwner = ResourceFactory.findById(opts.policyOwner.get) getOrElse {
      throw new ResourceNotFoundException(
        s"Given policy-owner '${opts.policyOwner.get}' not found.")
    }
    
    log.debug(s"Publishing PRE event: ${eventName}")
    publishEvent(actionName, EventType.Pre, opts) match {
      case Success(_) => {
        log.debug(s"Successfully published $eventName")
        Continue
      }
      case Failure(e) => {
        log.error(s"Failure publishing event : '$eventName'")
        Continue
      }
    }    
  }

  private def predicateMessage(p: Predicate[Any]) = {
    val value = p.value.toString.replaceAll("\"", "")
    "[%s %s %s]".format(p.property, p.operator, value)
  }  
}

case class EventsPost(override val args: String*) extends Operation(args) with EventMethods {
  private val log = Logger(this.getClass)
  
  def proceed(opts: RequestOptions) = {
    log.debug("entered EventsPost.proceed()")
    val actionName = args(0)
    val eventName = s"${actionName}.${EventType.Post}"
    
    log.debug("Publishing post event...")
    publishEvent(actionName, EventType.Post, opts) match {
      case Success(_) => Continue
      case Failure(e) => {
        log.error(s"Failure publishing event : '$eventName'")
        Continue
      }
    }
  }
}

case class RequestOptions(
    user: AuthAccountWithCreds, 
    authTarget: Option[UUID], 
    policyOwner: Option[UUID], 
    policyTarget: Option[GestaltResourceInstance],
    data: Option[Map[String,String]] = None)

import scala.annotation.tailrec


class SafeRequest(operations: List[Operation[Seq[String]]], options: RequestOptions) {
  
  private val log = Logger(this.getClass)
  
  type OptIdResponse = OperationResponse[Option[UUID]]
  type SeqStringOp = Operation[Seq[String]]
  
  def Protect[T](f: Option[UUID] => Result): Result = {

    @tailrec def evaluate(os: List[SeqStringOp], proceed: OptIdResponse): OptIdResponse = {
      os match {
        case Nil => proceed
        case op :: tail => op.proceed(options) match {
          case Continue => evaluate(tail, Continue)
          case Accepted => evaluate(tail, Accepted)
          case e: Halt  => e
        }
      }
    }
    
    /*
     * Separate operations list into pre and post ops.
     */
    val (beforeOps, afterOps) = sansPost(operations)
    
    val result = evaluate(beforeOps, Continue).toTry match {
      case Success(state) => f( state )
      case Failure(error) => HandleExceptions(error) 
    }
    
    if (afterOps.isDefined) {
      log.debug(s"Found *.post events: ${afterOps.get}")
    } else {
      log.debug("No *.post events found.")
    }
    
    afterOps map( _.proceed(options) )
    
    result
  }
  
  type OpList = List[Operation[Seq[String]]]
  
  def sansPost(ops: OpList): (OpList, Option[Operation[Seq[String]]]) = {
    val post = ops filter(_.isInstanceOf[EventsPost])
    if (post.isEmpty) (operations -> None)
    else ((operations diff post) -> Option(post(0)))
  }
  
}

object SafeRequest {
  def apply(operations: List[Operation[Seq[String]]], options: RequestOptions) = {
    new SafeRequest(operations, options)
  }
}