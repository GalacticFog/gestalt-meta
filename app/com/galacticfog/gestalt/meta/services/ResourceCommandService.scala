//package com.galacticfog.gestalt.meta.services
//
//
//
//import play.api.{Logger => log}
//
//import com.galacticfog.gestalt.meta.domain._
//
//import com.galacticfog.gestalt.meta.models._
//
//import com.galacticfog.gestalt.meta.domain.output._
//
//import java.util.UUID
//
//import scala.util.{Try, Success, Failure}
//
//import play.api.libs.json._
//
//import com.galacticfog.gestalt.tasks.io._
//
//object ResourceCommandService {
//  
//  
//  def createResource(event: ApiTaskEvent): Try[Option[ApiTaskResult]] = {
//    log.debug("ResourceCommandService::createResource(...)")
//    val result = for {
//      res <- unwrapResource( event )
//      out <- create( res )
//    } yield out
//    
//    writeTask( event, result.get )
//    result
//  }
//
//  /**
//   * Attempt instantiation of a GestaltResourceInstance from the request
//   * payload found in the TaskEvent.
//   */
//  private def unwrapResource( event: ApiTaskEvent ): Try[GestaltResourceInstance] = Try {
//    log.debug("ResourceCommandService::unwrapResource(...)")
//    val js = event.task.request_data getOrElse {
//      throw new IllegalArgumentException( "Request payload is empty." )
//    } 
//    log.debug("JSON: " + js)
//    js.validate[GestaltResourceInstance] match {
//      case JsSuccess(resource, _) => resource
//      case err: JsError => {
//        throw new RuntimeException( JsError.toFlatJson( err ).toString )
//      }
//    }
//  }
//  
//  
//  
///* 
//      override val id: UUID,
//      override val resourceTypeId: UUID,
//      override val orgId: UUID,
//      override val ownerId: UUID,
//      override val name: String,
//      override val description: Option[String] = None,
//      override val resourceState: UUID,
//      override val created: Option[Hstore] = None,
//      override val modified: Option[Hstore] = None,    
//      override val properties: Option[Hstore] = None,
//      override val variables: Option[Hstore] = None,
//      override val tags: Option[Hstore] = None,
//      override val auth: Option[Hstore] = None)    
// */
//  
//  
//  /**
//   * TODO: HACK: This is temporary to get around an issue with the global task-listener.
//   * This writes the completed task directly to the data store instead of posting
//   * the the event bus.
//   */
//  import org.joda.time.DateTime
//  private def writeTask(event: ApiTaskEvent, result: Option[ApiTaskResult]) = {
//    //val out = event.complete.withTaskResult
//    val task = event.task
//    val time = DateTime.now.toString
//    
//    println("orgId: " + event.actionArgs("org_id").toString.replaceAll(""""""", ""))
//    
//    val orgId   = UUID.fromString( event.actionArgs("org_id").toString.replaceAll(""""""", "") )
//    val ownerId = UUID.fromString( event.actionArgs("owner_id").toString.replaceAll(""""""", "") )
//    
//    println("ResourceCommandController::writeTask")
//    println("org_id: %s\nowner_id: %s\n".format(orgId.toString, ownerId.toString))
//    
//    val obj = GestaltResourceInstance(
//        id = uuid(), //UUID.fromString( task.uuid ),
//        resourceTypeId = ResourceType.id( Resources.Task ),
//        orgId = orgId,
//        ownerId = ownerId,
//        name = uuid.toString(),
//        description = None,
//        resourceState = ResourceState.id( ResourceStates.Active ),
//        created = Some(Map("timestamp"  -> time)),
//        modified = Some(Map("timestamp" -> time)),
//        properties = Some(Map(
//            "service_id" -> uuid().toString,
//            "status_id"  -> TaskStatus.Completed, //uuid().toString,
//            "created"    -> time,
//            "detail"     -> Json.prettyPrint( Json.toJson(task.detail) ) ) ),
//        variables = None,
//        tags = None,
//        auth = None
//    )
//    ResourceFactory.createResource( obj ) match {
//      case Success( _ ) => println("SUCCESS : Task persisted.")
//      case Failure( ex ) => println("FAILURE : Could not write task => " + ex.getMessage)
//    }
//  }
//  
//  private def create(res: GestaltResourceInstance) = Try {
//    ResourceFactory.createResource( res ) match {
//      case Success( _ ) => successResult( res )
//      case Failure( ex ) => failureResult(
//        Some( "Could not create resource." ),
//        ApiError( None, ex.getMessage) )
//    }      
//  }
//  
//  
//  private def failureResult(message: Option[String], errors: ApiError*) = {
//    ApiTaskResult( message = message, 
//        errors = if (errors.isEmpty) None else Some(Seq( errors : _*)) ).asOpt
//  }
//  
//  private def successResult(res: GestaltResource) = {
//    ApiTaskResult( message = Some( "IT ACTUALLY WORKED!!!" ) ).asOpt
//  }
//  
//  
//  import scala.reflect.runtime.universe.TypeTag
//  
//  def unwrapJsResult[T: TypeTag](json: JsValue)(implicit writes: Reads[T]) = {
//    println("JsonUtils::unwrapJsResult(...)")
//    json.validate[T] match {
//      case JsSuccess(event, _) => event
//      case err: JsError => {
//        println("JsonUtils::unwrapJsResult(...) => ERROR: " +
//            JsError.toFlatJson(err).toString)
//        throw new IllegalArgumentException {
//          JsError.toFlatJson(err).toString
//        }
//      }
//    }
//  }
//  
//  
////  def createResource(json: JsValue) = Try {
////    val r = json.validate[GestaltResourceInstance].get
////    ResourceFactory.assertValidResourceType( r.resourceTypeId )
////    ResourceFactory.createResource( r ) match {
////      case Success( _ ) => "Resource Created."
////      case Failure( ex ) => throw ex
////    }
////  }
//  
//}