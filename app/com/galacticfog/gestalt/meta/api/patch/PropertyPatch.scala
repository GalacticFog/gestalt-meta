package com.galacticfog.gestalt.meta.api.patch


import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.patch._
import com.galacticfog.gestalt.meta.api.output._
import play.api.libs.json._
import scala.util.{Try,Success,Failure}
import play.api.Logger


object PropertyPatch {

  private val log = Logger(this.getClass)
  
  import com.galacticfog.gestalt.json.Js
     
  def applyOps(r: GestaltResourceInstance, ops: Seq[PatchOp]): Try[GestaltResourceInstance] = {

    (for {
      o <- PatchDocument(ops:_*).applyPatch(toJson(r))
      p <- Js.parse[Map[String,JsValue]](Js.find(o, "/properties").get)
      s <- Try( toStringMap(p) )
      r <- fromJson[GestaltResourceInstance](o ++ Json.obj("properties" -> s))
      
    } yield (r,p)) map { case (res, props) =>
      res.copy(properties = Option(toStringMap(props)) )  
    }
  }
  
  private[patch] def toJson(r: GestaltResourceInstance): JsObject = {
    val raw = r.properties.get
    Json.toJson(r).as[JsObject] ++ Json.obj("properties" -> jsonProps(raw))    
  }
  
  
  /**
   * Convert a JSON object to an instance of T
   * TODO: No longer need this method - use Js.parse directly.
   */
  private[patch] def fromJson[T](json: JsValue)(implicit rt: Reads[T]): Try[T] = {
    Js.parse[T](json)
  }
  
  /**
   * Convert a resource Map[String,String] to a JSON object. Decodes the
   * escaped strings stored as the map values.
   */
  private[patch] def jsonProps(props: Map[String,String]): JsObject = {
    Json.toJson(props map { p => (p._1, unescape(p._2)) }).as[JsObject]    
  }
  
  
  /**
   * Convert a Map[String,JsValue] to a Map[String,String]
   */
  private[patch] def toStringMap(m: Map[String,JsValue]): Map[String,String] = {
    m map { x => val value = x._2 match {
      case s: JsString => s.as[String]
      case v => Json.stringify(v)
    }
    (x._1, value) }
  }
  
  
  /**
   * Convert an escaped string to a JsValue of the appropriate type. Works for
   * unescaped strings as well.
   * 
   * @param s String to convert to JSON
   */
  private[patch] def unescape(s: String): JsValue = Try {
    Json.parse(s)
  } match {
    case Success(js) => js
    case Failure(e) => Json.toJson(s)
  }
  
  
}