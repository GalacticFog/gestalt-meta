//package com.galacticfog.gestalt.laser
//
//
//import play.api.Logger
//import play.api.libs.json._
//import scala.util.{Try,Success,Failure}
//import com.galacticfog.gestalt.meta.api.sdk.HostConfig
//
///**
// * 
// * @tparam C type of web client
// * @tparam R return type for commands
// * @tparam L type of Lambda object
// */
//abstract class LambdaProvider[R,L](config: HostConfig) {
//  def lambdas(): Seq[L]
//  def lambdas(id: String): Option[L]
//  def createLambda(lambda: L): Try[R]
//  def updateLambda(lambda: L): Try[R]
//  def deleteLambda(id: String): Try[R]
//  
//  
//  private[laser] def base64(s: String, charset: Charset = StandardCharsets.UTF_8): String = {
//    Base64.getEncoder().encodeToString(s.getBytes(charset))
//  }
//  
//  private[laser] def configureClient(config: HostConfig) = {
//    val authHeader = if (key.isDefined && secret.isDefined) {
//      Option("Authorization" -> base64("%s:%s".format(key.get, secret.get)))
//    } else None
//    new JsonWebClient(config, authHeader)
//  }  
//}
//
//
//class FakeLambdaProvider(config: HostConfig) 
//    extends LambdaProvider[ApiResponse,LaserLambda](config) {
//  
//  def lambdas(): Seq[LaserLambda] = {
//    Seq.empty
//  }
//  
//  def lambdas(id: String): Option[LaserLambda] = {
//    None
//  }
//
//  def createLambda(lambda: LaserLambda): Try[ApiResponse] = {
//    Try(ApiResponse(201, Option(Json.toJson(lambda))))
//  }
//  
//  def updateLambda(lambda: LaserLambda): Try[ApiResponse] = {
//    Try(ApiResponse(200, Option(Json.toJson(lambda))))
//  }
//  
//  def deleteLambda(id: String): Try[ApiResponse] = {
//    Try(ApiResponse(204, None))
//  }
//}
//
//
//class LambdaProviderImpl(config: HostConfig) 
//    extends LambdaProvider[ApiResponse,LaserLambda](config) {
//  
//  private[this] val log = Logger(this.getClass)
//  
//  def lambdas(): Seq[LaserLambda] = {
//    getSeq[LaserLambda](client, "/lambdas", Seq(200))
//  }
//  
//  def lambdas(id: String): Option[LaserLambda] = {
//    get[LaserLambda](client, s"/lambdas/$id", Seq(200))  
//  }
//  
//  def createLambda(lambda: LaserLambda): Try[ApiResponse] = {
//    val json = Json.toJson(lambda)
//    log.debug("Creating new Lambda in gestalt-lambda:\n" + Json.prettyPrint(json))
//    client.post("/lambdas", json, Seq(200,201,202))
//  }
//  
//  def updateLambda(lambda: LaserLambda): Try[ApiResponse] = {
//    val id = lambda.id getOrElse {
//      throw new RuntimeException("Cannot update Lambda without ID. found: " + lambda)
//    }
//    val json = Json.toJson(lambda)
//    log.debug("Updating Lambda in gestalt-lambda:\n" + Json.prettyPrint(json))
//    client.put(s"/lambdas/${id}", json, Seq(200,201,202))
//  }
//  
//  def deleteLambda(id: String): Try[ApiResponse] = {
//    client.delete(s"/lambdas/${id}", Seq(200,204,404))
//  }
//  
//  private[laser] def get[T](client: JsonWebClient, resource: String, expected: Seq[Int])
//      (implicit fmt: Format[T]): Option[T] = {
//    
//    client.get(resource, expected) match {
//      case Failure(err) => throw err
//      case Success(res) => res.output match {
//        case Some(out) => out.validate[T] match {
//          case s: JsSuccess[T] => Some(s.get)
//          case e: JsError => 
//            throw new RuntimeException(JsError.toFlatJson(e).toString)
//        }
//        case None => None
//      }
//    }    
//  }  
//  
//  private[laser] def getSeq[T](client: JsonWebClient, resource: String, expected: Seq[Int])
//      (implicit fmt: Format[T]): Seq[T] = {
//    client.get(resource, expected) match {
//      case Failure(err) => throw err
//      case Success(res) => res.output match {
//        case Some(out) => out.validate[Seq[T]] match {
//          case s: JsSuccess[Seq[T]] => s.get
//          case e: JsError =>
//            throw new RuntimeException(JsError.toFlatJson(e).toString)
//        }
//        case None => Seq()
//      }
//    } 
//  }  
//}
