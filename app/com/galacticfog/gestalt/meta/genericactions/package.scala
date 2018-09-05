package com.galacticfog.gestalt.meta

import play.api.libs.json._

package object genericactions {
  
  sealed trait FunctionVerb {
    val description: Option[String]
    val body: Option[RequestBody]
    val query_parameters: Option[Seq[RequestQueryParameter]]
    val responses: Seq[FunctionResponse]    
  }
  
  object FunctionVerb {
    implicit lazy val reader =
      __.read[GetVerb].map(x=>x:FunctionVerb) orElse
      __.read[PostVerb].map(x=>x:FunctionVerb) orElse
      __.read[PutVerb].map(x=>x:FunctionVerb) orElse
      __.read[PatchVerb].map(x=>x:FunctionVerb) orElse
      __.read[DeleteVerb].map(x=>x:FunctionVerb)
      
    implicit lazy val writer =
      Writes[FunctionVerb] {
        case a: GetVerb => Json.toJson(a)(GetVerb.format)
        case b: PostVerb => Json.toJson(b)(PostVerb.format)
        case c: PutVerb  => Json.toJson(c)(PutVerb.format)
        case d: PatchVerb => Json.toJson(d)(PatchVerb.format)
        case e: DeleteVerb => Json.toJson(e)(DeleteVerb.format)
    }
  }
  
  case class RequestQueryParameter(
    name: String, 
    description: Option[String], 
    example: Option[String],
    required: Boolean = false,
    data_type: String = "string")    
  
  object RequestQueryParameter {
    implicit lazy val formatRequestQueryParameter = Json.format[RequestQueryParameter]    
  }  
  
  case class GetVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse]) extends FunctionVerb
      
  case class PostVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse]) extends FunctionVerb
    
  case class PutVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse]) extends FunctionVerb
    
  case class PatchVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse]) extends FunctionVerb
      
  case class DeleteVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse]) extends FunctionVerb
      
  object GetVerb {
    implicit lazy val format = Json.format[GetVerb]
  }      
  object PostVerb {
    implicit lazy val format = Json.format[PostVerb]
  }      
  object PutVerb {
    implicit lazy val format = Json.format[PutVerb]
  }    
  object PatchVerb {
    implicit lazy val format = Json.format[PatchVerb]
  }
  object DeleteVerb {
    implicit lazy val format = Json.format[DeleteVerb]
  }

  case class ActionIcon(svg: Option[String])
  case class GestaltUi(render: String, locations: Seq[String], icon: Option[ActionIcon])
  case class FunctionResponse(code: Int, content_type: Option[String], gestalt_ui: Option[GestaltUi])
  case class RequestBody(content_type: String)

  case class GestaltFunction(
      name: String, 
      display_name: Option[String] = None,
      description: Option[String] = None,
      get: Option[FunctionVerb] = None,
      post: Option[FunctionVerb] = None,
      put: Option[FunctionVerb] = None,
      patch: Option[FunctionVerb] = None,
      delete: Option[FunctionVerb] = None) {
    
    /*
     * Ensure exactly one FunctionVerb is given.
     */
    private val theVerb = {
      val v = Seq(get, post, put, patch, delete).filter(_.isDefined)
      v.size match {
        case 1 => v.head.get
        case 0 => throw new RuntimeException("Must provide an HTTP verb. None found.")
        case _ => throw new RuntimeException("Multiple HTTP verbs found. There can be only one.")
      }
    }
    
    def verbString() = theVerb match {
      case a: GetVerb => "GET"
      case b: PostVerb => "POST"
      case c: PutVerb => "PUT"
      case d: PatchVerb => "PATCH"
      case e: DeleteVerb => "DELETE"
    }
  }

  case class GestaltEndpoint(kind: String, url: String, actions: Seq[GestaltFunction])
  case class GestaltFunctionConfig(endpoints: Seq[GestaltEndpoint])
  
  implicit lazy val formatActionIcon = Json.format[ActionIcon]
  implicit lazy val formatGestaltUi = Json.format[GestaltUi]
  implicit lazy val formatFunctionResponse = Json.format[FunctionResponse]
  implicit lazy val formatRequestBody = Json.format[RequestBody]
  implicit lazy val formatGestaltFunction = Json.format[GestaltFunction]
  implicit lazy val formatGestaltEndpoint = Json.format[GestaltEndpoint]
  implicit lazy val formatGestaltFunctionConfig = Json.format[GestaltFunctionConfig]    

  
//  case class ActionIcon(svg: Option[String])
//  case class GestaltUi(render: String, locations: Seq[String], icon: Option[ActionIcon])
//  case class FunctionResponse(code: Int, content_type: Option[String], gestalt_ui: Option[GestaltUi])
//  case class RequestBody(content_type: String)
//  
//  case class PostAction(body: RequestBody, responses: Seq[FunctionResponse])
//  case class PutAction(body: RequestBody, responses: Seq[FunctionResponse])
//  case class PatchAction(body: RequestBody, responses: Seq[FunctionResponse])
//  case class DeleteAction(responses: Seq[FunctionResponse])
//  case class GestaltFunction(
//      name: String, 
//      display_name: Option[String],
//      description: Option[String],
//      
//      post: Option[PostAction],
//      put: Option[PutAction],
//      patch: Option[PatchAction],
//      delete: Option[DeleteAction]) {
//    
//    def method(): String = {
//      if (this.post.isDefined) "POST"
//      else if (this.put.isDefined) "PUT"
//      else if (this.patch.isDefined) "PATCH"
//      else if (this.delete.isDefined) "DELETE"
//      else "POST"
//    }
//    
//    
//  }
//  case class GestaltEndpoint(kind: String, url: String, actions: Seq[GestaltFunction])
//  case class GestaltFunctionConfig(endpoints: Seq[GestaltEndpoint])
//  
//  implicit lazy val formatActionIcon = Json.format[ActionIcon]
//  implicit lazy val formatGestaltUi = Json.format[GestaltUi]
//  implicit lazy val formatFunctionResponse = Json.format[FunctionResponse]
//  implicit lazy val formatRequestBody = Json.format[RequestBody]
//  implicit lazy val formatPostAction = Json.format[PostAction]
//  implicit lazy val formatPutAction = Json.format[PutAction]
//  implicit lazy val formatPatchAction = Json.format[PatchAction]
//  implicit lazy val formatDeleteAction = Json.format[DeleteAction]
//  implicit lazy val formatGestaltFunction = Json.format[GestaltFunction]
//  implicit lazy val formatGestaltEndpoint = Json.format[GestaltEndpoint]
//  implicit lazy val formatGestaltFunctionConfig = Json.format[GestaltFunctionConfig]    
  
}