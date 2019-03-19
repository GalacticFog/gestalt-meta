package com.galacticfog.gestalt.meta

import java.util.UUID
import play.api.libs.json._
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import scala.util.Try

package object genericactions {
  
  implicit lazy val formatRequestBody = Json.format[RequestBody]
  implicit lazy val formatRequestQueryParameter = Json.format[RequestQueryParameter]  
  
  implicit lazy val formatActionIcon = Json.format[ActionIcon]
  implicit lazy val formatGestaltUi = Json.format[GestaltUi]
  implicit lazy val formatFunctionResponse = Json.format[FunctionResponse]
  implicit lazy val formatPostVerb = Json.format[PostVerb]  
  implicit lazy val formatGetVerb = Json.format[GetVerb]
  implicit lazy val formatPutVerb = Json.format[PutVerb]
  implicit lazy val formatPatchVerb = Json.format[PatchVerb]
  implicit lazy val formatDeleteVerb = Json.format[DeleteVerb]

  implicit lazy val formatGestaltFunction = Json.format[GestaltFunction]
  implicit lazy val formatGestaltEndpointImpl = Json.format[GestaltEndpointImpl]
  implicit lazy val formatGestaltEndpoint = Json.format[GestaltEndpoint]
  lazy implicit val formatGestaltFunctionConfig = Json.format[GestaltFunctionConfig]    
  
    
  /**
   * Extract a GestaltFunctionConfig object from a Provider instance.
   */
  def getFunctionConfig(provider: GestaltResourceInstance): Try[GestaltFunctionConfig] = {
    for {
      ps  <- Try(provider.properties.getOrElse(throw new RuntimeException("No properties found.")))
      cfg <- Try(ps.get("config").getOrElse(throw new RuntimeException("No 'config' property found.")))
      out = {
        val configJson = Json.parse(cfg).as[JsObject]
        (Js.parse[GestaltFunctionConfig](configJson)(formatGestaltFunctionConfig)).get
      }
    } yield out
  }
  
  case class RequestQueryParameter(
    name: String, 
    description: Option[String] = None, 
    example: Option[String] = None,
    required: Option[Boolean] = None, //Some(false),
    data_type: Option[String] = None, //Some("string"),
    value: Option[String] = None)    

  trait FunctionVerb {
    val responses: Seq[FunctionResponse]
    
    def getUiResponse(): Option[FunctionResponse] = {
      responses.filter(_.gestalt_ui.isDefined).headOption
    }
  }
    
  case class GetVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse]) // extends FunctionVerb
      
  case class PostVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse]) extends FunctionVerb
    
  case class PutVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse])// extends FunctionVerb
    
  case class PatchVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse])// extends FunctionVerb
      
  case class DeleteVerb(
      description: Option[String] = None, 
      body: Option[RequestBody] = None, 
      query_parameters: Option[Seq[RequestQueryParameter]] = None,
      responses: Seq[FunctionResponse])// extends FunctionVerb
      
  case class ActionIcon(svg: Option[String])
  
  case class GestaltUi(render: String, locations: Seq[String], icon: Option[ActionIcon]) {
    if (locations.isEmpty) throw new RuntimeException("Must provide at least one 'location' value.")
  }
  
  case class FunctionResponse(
      code: Int, 
      content_type: Option[String] = None, 
      gestalt_ui: Option[GestaltUi] = None)
  
  case class RequestBody(content_type: String)
  
  case class GestaltFunction(
      name: String, 
      display_name: Option[String] = None,
      description: Option[String] = None,
      
      get: Option[GetVerb] = None,
      post: Option[PostVerb] = None,
      put: Option[PutVerb] = None,
      patch: Option[PatchVerb] = None,
      delete: Option[DeleteVerb] = None) {
      
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
    
    def hasUi() = {
      post.fold(false)(_.getUiResponse.isDefined)
    }
    
    def getUiResponse(): Option[FunctionResponse] = {
      for {
        v <- post
        r <- v.getUiResponse()
      } yield r
    }
    
    def verbString() = theVerb match {
      case a: GetVerb => "GET"
      case b: PostVerb => "POST"
      case c: PutVerb => "PUT"
      case d: PatchVerb => "PATCH"
      case e: DeleteVerb => "DELETE"
    }
  }
  case class GestaltEndpointImpl(kind: String, id: UUID)
  case class GestaltEndpoint(
                              kind: String, 
                              url: String, 
                              implementation: Option[GestaltEndpointImpl] = None,
                              actions: Seq[GestaltFunction], 
                              authentication: Option[String] = None) {
    if (actions.isEmpty) throw new RuntimeException("Must specify at least one action for an Endpoint.")
    
    private val duplicateActions = actions.map(_.name).groupBy(identity).collect { 
      case (k, vs) if vs.lengthCompare(1) > 0 => k 
    }
    
    if (duplicateActions.nonEmpty) {
      throw new RuntimeException(
          s"Action names MUST be unique. Found duplicates of the following: ${duplicateActions.mkString(",")}") 
    }
    
    def getUiActions(): Seq[GestaltFunction] = {
      actions.filter(a => a.getUiResponse.isDefined)
    }
    
    def getActionByName(name: String): Option[GestaltFunction] = {
      actions.find(_.name == name)
    }
  }
  
  case class GestaltFunctionConfig(endpoints: Seq[GestaltEndpoint]) {
    
    def getImplementationByActionName(action: String): Option[GestaltEndpointImpl] = {
      for {
        endpoint <- endpoints.find { _.actions.find(_.name == action).isDefined }
        implementation <- endpoint.implementation
      } yield implementation      
    }
    
    def getEndpointByActionName(action: String): Option[GestaltEndpoint] = {
      endpoints.find { _.actions.find(_.name == action).isDefined }      
    }
  }

  
}