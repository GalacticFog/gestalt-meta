package controllers.util


import java.util.UUID

import scala.annotation.tailrec
//import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent.Future

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.events._
import com.galacticfog.gestalt.keymgr.{GestaltFeature,GestaltLicense}

import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.meta.policy._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import play.api.Logger
import play.api.libs.json.JsObject
import play.api.libs.json.Json
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.mvc.Result
import com.galacticfog.gestalt.meta.validation._


abstract class Operation[T](val args: T) {
  def proceed(opts: RequestOptions): OperationResponse[Option[UUID]]
}

case class Feature(override val args: String*) extends Operation(args) {
  private[this] val log = Logger(this.getClass)
  
  def proceed(opts: RequestOptions) = {
    log.debug(s"Checking License: Feature[${args.mkString(",")}]")
    val feature = s2f(args(0))
    
    if (GestaltLicense.instance.isFeatureActive(feature)) Continue
    else Halt("Feature is not licensed.")
  }
  
  private def s2f(s: String): GestaltFeature = GestaltFeature.valueOf(s)
}


case class Validate(override val args: String*) extends Operation(args) {
  def proceed(opts: RequestOptions) = {
    val r = opts.policyTarget
    
    val parentId = for {
      data <- opts.data
      pid  <- data.get("parentId")
      uid = UUID.fromString(pid)
    } yield uid
        
    parentId.fold {
        Halt(s"Could not find 'parentId' to perform validation. This is a bug.")
          .asInstanceOf[OperationResponse[Option[UUID]]]
    } { pid =>
      opts.policyTarget.fold {
        Halt(s"Could not find 'policyTarget' in request options. This is a bug.")
          .asInstanceOf[OperationResponse[Option[UUID]]]
      }{ resource =>
        DefaultValidation.validate(resource, pid) match {
          case Failure(e) => Halt(e.getMessage)
          case Success(_) => Continue
        }
      }
    }
  }
}

case class Authorize(override val args: String*) extends Operation(args) with AuthorizationMethods {

  override def proceed(opts: RequestOptions): OperationResponse[Option[UUID]] = {
    
    val user = opts.user
    val action = args(0)
    val target = opts.authTarget.get
    
    log.debug(s"Checking Authorization : action=$action, user=${user.account.id}, target=$target")

    opts.providerIdOpt.fold(isAuthorized(target, action, user)) { providerId =>
      log.debug(s"Checking Authorization : action=provider.view, user=${user.account.id}, target=$providerId")
      for {
        _ <- isAuthorized(providerId, "provider.view", user)
       result <- isAuthorized(target, action, user)
      } yield result
    } match {
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

trait EventMethodsTrait {
  
  private[this] val log = Logger(this.getClass)
  
  def findEffectiveEventRules(parentId: UUID, event: Option[String] = None): Option[GestaltResourceInstance] = {
    
    log.debug(s"findEffectiveEventRules(_, ${event})")
    
    val rs = for {
      p <- ResourceFactory.findChildrenOfType(ResourceIds.Policy, parentId)
      r <- ResourceFactory.findChildrenOfType(ResourceIds.RuleEvent, p.id)
    } yield r

    val matchingRules = if (event.isEmpty) rs 
      else { 
        rs.filter { r =>
          val matchArray = for {
              ss <- RuleMatchAction.seqFromResource(r)
              ma = ss.map(_.action)
            } yield ma
          matchArray.get.contains(event.get)
        }
      }

    // TODO: This is temporary. Need a strategy for multiple matching rules.
    if (matchingRules.isEmpty) {
      log.debug("No effective event rules found. Nothing to publish")
      None 
    } else Some(matchingRules.head)
    
  }

  /* DO NOT DELETE
   * TODO: This *will be* the correct way to find effective event rules once we agree
   * upon what should happen in the event of multiple rule matches.
   
  def findEffectiveEventRules2(targetId: UUID, event: Option[String] = None): Seq[GestaltResourceInstance] = {
    ResourceFactory.findAncestorsOfSubType(ResourceIds.RuleEvent, targetId).filter { r =>
      r.properties.get("actions").contains(event.get)
    }
  }
  
   * 
   */

  def publishEvent( 
      actionName: String, 
      eventType: String,
      opts: RequestOptions): Try[OperationResponse[Option[UUID]]] = Try {
    
    val eventName = s"${actionName}.${eventType}"
    val target = {
      val parentId = opts.data.fold(Option.empty[UUID]){ 
        x => x.get("parentId") map { UUID.fromString(_) }
      }

      val owner = (parentId getOrElse opts.policyOwner.get)

      log.debug("Selecting policy root...")
      ResourceFactory.findById(owner).fold {
        throw new RuntimeException("Could not determine policy owner from request-options.")
      }{ r => 
        
        if (Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment).contains(r.typeId)) {
          log.debug(s"Policy root found : ${r.id}")
          r.id          
        } else {
          
          log.debug("Searching for ancestor Environment...")
          
          val env = ResourceFactory.findContainerTypeAncestors(r.id).lastOption.getOrElse {
            throw new RuntimeException("Could not find policy owner.")
          }
          log.debug(s"Policy root found : ${r.id}")
          env.id
        }
      }
    }
    
    findEffectiveEventRules(target, Option(eventName)) match { 
      case None => {
        log.info("No matching event rules found. Nothing to do.")
        Continue
      }
      case Some(rule) => {
        val event = EventMessage.make(
              id       = UUID.randomUUID, 
              identity = opts.user.account.id, 
              event    = eventName, 
              action   = actionName, 
              rule     = rule, 
              payload  = opts.policyTarget)

        val suppressMetaFunction: Boolean = isSuppressed(rule, eventName) match {
          case Success(v) => v
          case Failure(e) => {
            log.error("Failure testing meta_function directive: ${e.getMessage}")
            false
          }
        }
        
        (for {
          eventJson <- formatEventJson(event, opts)
          response <- {
            log.info(s"Publishing '${eventName}'")
            eventsClient.publish(AmqpEndpoint(RABBIT_EXCHANGE, RABBIT_ROUTE), eventJson)
          }
        } yield response) match {
          case Success(_) => {
            log.info(s"Successfully published '${eventName}'")
            if (suppressMetaFunction) {
              log.debug("Meta function is suppressed.")
              Accepted 
            } else {
              log.debug("Meta function is NOT suppressed.")
              Continue
            }
          }
          case Failure(e) => {
            log.error(s"Failed publishing event '${eventName}': ${e.getMessage}")
            Halt(e.getMessage)
          }
        }
      }
    }  
  }  
  
  private[controllers] def formatEventMessage(
                              identity: UUID, 
                              eventName: String, 
                              actionName: String, 
                              rule: GestaltResourceInstance, 
                              payload: Option[GestaltResourceInstance],
                              opts: RequestOptions) = {

    val event = EventMessage.make(
          id       = UUID.randomUUID, 
          identity = opts.user.account.id, 
          event    = eventName, 
          action   = actionName, 
          rule     = rule, 
          payload  = opts.policyTarget)
  
    formatEventJson(event, opts)      
  }
  
  private[controllers] def formatEventJson(
                              event: EventMessage, 
                              opts: RequestOptions): Try[JsObject] = Try {
    val json = event.toJson
    opts.data.fold(json) { dat =>
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
    eventsClient.publish(AmqpEndpoint(RABBIT_EXCHANGE, RABBIT_ROUTE), eventJson)
  }
  
  
  private[controllers] val SUPPRESS_META_FUNCTION = "suppress"
  
  /**
   * Determine if the meta_function for a given event is 'suppressed'.
   */
  private[controllers] def isSuppressed(rule: GestaltResourceInstance, eventName: String): Try[Boolean] = {
    for {
      mas <- RuleMatchAction.seqFromResource(rule)
      ma <- Try(mas.find(_.action == eventName))
      suppressed = for {
        mf <- ma
        x  <- mf.meta_function
        y = x.trim.toLowerCase
        z = y == "suppress"
      } yield z
      
    } yield suppressed.getOrElse(false)
  }  
  
  def eventsClient(): AmqpClient
}

object EventMethods extends EventMethodsTrait {
  def eventsClient() = {
    AmqpClient(AmqpConnection(RABBIT_HOST, RABBIT_PORT, heartbeat = 300))
  }
}

case class EventsPre(override val args: String*) extends Operation(args) {

  private val log = Logger(this.getClass)
  
  def proceed(opts: RequestOptions) = {
    log.debug(s"EventsPre[${args.mkString(",")}]")
    
    val user = opts.user
    val actionName = args(0)
    val eventName = s"${actionName}.${EventType.Pre}"
    
    /*
     * policyOwner is the resource on which the policy was set.
     */
    val policyOwner = ResourceFactory.findById(opts.policyOwner.get) getOrElse {
      throw new ResourceNotFoundException(
        s"Given policy-owner '${opts.policyOwner.get}' not found.")
    }
    
    EventMethods.publishEvent(actionName, EventType.Pre, opts) match {
      case Success(status) => {
        log.info(s"Pre-event processing complete. Received status : $status")
        status
      }
      case Failure(e) => {
        log.error(s"Failure publishing event: '$eventName' : ${e.getMessage}")
        log.warn("Current strategy on publish failure is 'Continue'. Continuing...")
        Continue
      }
    }
  }

  private def predicateMessage(p: Predicate[Any]) = {
    val value = p.value.toString.replaceAll("\"", "")
    "[%s %s %s]".format(p.property, p.operator, value)
  }
}

case class EventsPost(override val args: String*) extends Operation(args) {
  private val log = Logger(this.getClass)
  
  def proceed(opts: RequestOptions) = {
    log.debug("entered EventsPost.proceed()")
    val actionName = args(0)
    val eventName = s"${actionName}.${EventType.Post}"

    EventMethods.publishEvent(actionName, EventType.Post, opts) match {
      case Success(_) => Continue
      case Failure(e) => {
        log.error(s"Failure publishing event : '$eventName' : ${e.getMessage}")
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
    data: Option[Map[String,String]] = None,
    providerIdOpt: Option[UUID] = None)


class SafeRequest(operations: List[Operation[Seq[String]]], options: RequestOptions) {
  
  private val log = Logger(this.getClass)
  
  type OptIdResponse = OperationResponse[Option[UUID]]
  type SeqStringOp = Operation[Seq[String]]


  def Execute[T](f: GestaltResourceInstance => Result): Result = {

    @tailrec def evaluate(os: List[SeqStringOp], proceed: OptIdResponse): OptIdResponse = {
      os match {
        case Nil => proceed
        case op :: tail => op.proceed(options) match {
          case Continue => evaluate(tail, Continue)
          case Accepted => evaluate(tail, Accepted)
          case e: Halt  => {
            /*
             * It might be fine to just throw whatever exception we want right here.
             * As it exists, we still run the *.post event when the operation fails.
             * That might not be what we want.
             */
            e 
          }
        }
      }
    }

    /*
     * Separate operations list into pre and post ops.
     */
    val (beforeOps, afterOps) = sansPost(operations)

    /*
     * Per comment above, this checks if there was an exception during evaluation - if
     * there was, 1) we hang on to it, 2) execute *.post event, 3) return the error.
     * The correct behavior might be to throw the error as soon as it occurs.
     */
    val (result, exception) = evaluate(beforeOps, Continue).toTry match {
      case Success(state) => {
        val resource = {
          val res = options.policyTarget.get
          state.fold(res)(st => res.copy(state = st))
        }
        f(resource) -> None
      }
      case Failure(error) => {
        HandleExceptions(error) -> Some(error)
      }
    }

    if (afterOps.isDefined) {
      log.debug(s"Found *.post events: ${afterOps.get}")
    } else {
      log.debug("No *.post events found.")
    }

    afterOps map( _.proceed(options) )

    if (exception.nonEmpty) throw exception.get
    else result
  }


  def ExecuteAsyncT[T](f: GestaltResourceInstance => Future[T]): Future[T] = {
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
      case Success(state) => {
        val resource = {
          val res = options.policyTarget.get
          state.fold(res)(st => res.copy(state = st))
        }
        f(resource)
      }
      case Failure(error) => Future.failed(error)
    }

    if (afterOps.isDefined) {
      log.debug(s"Found *.post events: ${afterOps.get}")
    } else {
      log.debug("No *.post events found.")
    }

    afterOps map( _.proceed(options) )
    result
  }

  def ExecuteAsync(f: GestaltResourceInstance => Future[Result]): Future[Result] = {

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
      case Success(state) => {
        val resource = {
          val res = options.policyTarget.get
          state.fold(res)(st => res.copy(state = st))
        }
        f(resource)
      }
      case Failure(error) => HandleExceptionsAsync(error) 
    }
    
    if (afterOps.isDefined) {
      log.debug(s"Found *.post events: ${afterOps.get}")
    } else {
      log.debug("No *.post events found.")
    }
    
    afterOps map( _.proceed(options) )
    result
  }    
  
  
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
      case Success(state) => f(state)
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
  
  
  def ProtectT[T](f: Option[UUID] => T): Try[T] = {

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
      case Success(state) => Success(f(state))
      case Failure(error) => Failure(error)
    }
    
    if (afterOps.isDefined) {
      log.debug(s"Found *.post events: ${afterOps.get}")
    } else {
      log.debug("No *.post events found.")
    }
    
    afterOps map( _.proceed(options) )
    result
  }

  
  def ProtectAsync[T](f: Option[UUID] => Future[T]): Future[T] = {
    log.debug("Entered SafeRequest::ProtectAsync...")
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

    val pre = evaluate(beforeOps, Continue)

    for {
      result <- pre.toTry match {
        case Success(state) => f(state)
        case Failure(error) => Future.failed(error)
      }
      _ = afterOps match {
        case Some(ops) =>
          log.debug(s"Found *.post events: ${afterOps.get}")
          ops.proceed(options)
        case None =>
          log.debug("No *.post events found.")
      }
    } yield result
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

object ComposableSafeRequest {
  def Protect(operations: List[Operation[Seq[String]]], options: RequestOptions) = {
    new {
      val sr = new SafeRequest(operations, options)

      def map(f: Option[UUID] => Result): Future[Result] = Future.successful(sr.Protect(f))
      def flatMap(f: Option[UUID] => Future[Result]): Future[Result] = sr.ProtectAsync(f)
    }
  }
  def Execute(operations: List[Operation[Seq[String]]], options: RequestOptions) = {
    new {
      val sr = new SafeRequest(operations, options)

      def map(f: GestaltResourceInstance => Result): Future[Result] = Future.successful(sr.Execute(f))
      def flatMap(f: GestaltResourceInstance => Future[Result]): Future[Result] = sr.ExecuteAsync(f)
    }
  }
}


// SafeRequest itself needs to be refactored
object ComposableSafeRequest2 {
  def Protect(operations: List[Operation[Seq[String]]], options: RequestOptions) = {
    new {
      val sr = new SafeRequest(operations, options)

      def map[T](f: Option[UUID] => T): Future[T] = Future.fromTry(sr.ProtectT(f))
      def flatMap[T](f: Option[UUID] => Future[T]): Future[T] = sr.ProtectAsync(f)
    }
  }
  def Execute(operations: List[Operation[Seq[String]]], options: RequestOptions) = {
    new {
      val sr = new SafeRequest(operations, options)

      def map[T](f: GestaltResourceInstance => T): Future[T] = ???
      def flatMap[T](f: GestaltResourceInstance => Future[T]): Future[T] = sr.ExecuteAsyncT(f)
    }
  }
}