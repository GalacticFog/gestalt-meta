package com.galacticfog.gestalt

import com.galacticfog.gestalt.data.models._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._
import java.util.UUID
import com.galacticfog.gestalt.meta.api.errors.BadRequestException

package object laser {

  implicit lazy val laserApiFormat = Json.format[LaserApi]
  implicit lazy val laserGatewayInfoFormat = Json.format[LaserGatewayInfo]
  implicit lazy val laserGatewayFormat = Json.format[LaserGateway]
  implicit lazy val laserEndpointFormat = Json.format[LaserEndpoint]
  implicit lazy val laserArtifactDescriptionFormat = Json.format[LaserArtifactDescription]
  implicit lazy val laserLambdaFormat = Json.format[LaserLambda]
  implicit lazy val laserProviderFormat = Json.format[LaserProvider]
  implicit lazy val laserLocationFormat = Json.format[LaserLocation]
  
  
  case class LaserGatewayInfo(host: String, port: Int, username: String, password: String)
  
  case class LaserGateway(
      id: Option[String], 
      name: String, 
      locationId: String,
      gatewayInfo: JsValue)  
  
  case class LaserApi(
      id: Option[String], 
      name: String, 
      gatewayId: Option[String] = None,
      provider: Option[JsValue] = None, 
      description: Option[String] = None)

  case class LaserProvider(id: Option[String], name: String, href: Option[String] = None, providerInfo: Option[JsValue] = None)
  case class LaserLocation(id: Option[String], name: String, providerId: String)
  
  case class LaserEndpoint(
      id: Option[String], 
      apiId: String, 
      upstreamUrl: String,
      path: String, 
      domain: Option[JsValue] = None,
      url: Option[String] = None,
      provider: Option[JsValue],
      endpointInfo: Option[JsValue] = None, 
      authentication: Option[JsValue] = None)

  case class LaserArtifactDescription(
      artifactUri: Option[String],
      runtime: String,
      functionName: String,
      handler: String,
      memorySize: Int,
      cpus: Double,
      description: Option[String] = None,
      publish: Boolean = false,
      role: String = "none",
      timeoutSecs: Int = 180,
      code: Option[String] = None)
  
  case class LaserLambda(
      id: Option[String], 
      eventFilter: Option[String],
      public: Boolean,
      provider: Option[JsValue],
      artifactDescription: LaserArtifactDescription)


//  def toLaserGateway(input: GestaltResourceInput) = {
//    LaserGateway(id = None, name = input.name, gatewayInfo = input.properties.get("gateway_info"))
//  }

  def getApiId(apiName: String) = ???
  
  // lambdaId is the Laser string ID
  def buildEndpointUri(lambdaHost: String, lambdaId: String) = {
    "%s/lambdas/%s/invoke".format(lambdaHost, lambdaId)
  }
  
  // uri is implementation.uri
  def getEndpointPath(uri: String) = uri.drop(uri.lastIndexOf("/"))
  

  def toLaserApi(api: GestaltResourceInput, provider: UUID, location: String) = {
    // One LaserApi for each given provider.
    val input = Json.toJson(api)
    println("API-INPUT:\n" + Json.prettyPrint(input))
    val id = api.id.getOrElse(UUID.randomUUID).toString
    LaserApi(
        id = Some(id),
        name = api.name,
        description = api.description,
        provider = Some(Json.obj("id" -> provider.toString, "location" -> location)))
  }
  
  
  def toLaserEndpoint(input: GestaltResourceInput, apiId: UUID, upstreamUrl: String, provider: JsValue) = {
    val id = Some(input.id.getOrElse(UUID.randomUUID).toString)
    val props = input.properties.get
    LaserEndpoint(
        id = id,  
        apiId = apiId.toString,           // argument
        upstreamUrl = upstreamUrl,        // argument
        path = props("resource").as[String],        // from properties
        domain = if (props.contains("domain")) Some(props("domain")) else None,      // from properties
        provider = Some(provider))    // argument
  }
  
  
  def toLaserEndpoint(input: GestaltResourceInput) = {
    val id = Some(input.id.getOrElse(UUID.randomUUID).toString)
    LaserEndpoint(
        id = id,
        apiId = ???,
        upstreamUrl = ???,
        path = ???,
        domain = ???,
        provider = ???)
  }
  
  /*
   * TODO: This may translate into multiple LaserLambdas
   */
  def toLaserLambda(lambda: GestaltResourceInput, providerId: String, location: String) = {
    
    println("toLaserLambda(...)")
    
    val props = lambda.properties.get
    val (handler,function) = parseHandlerFunction(lambda)
    val artifactUri = {
      if (props.contains("package_url")) Some(props("package_url").as[String])
      else None
    }
    
    
    val isPublic = if (props.contains("public")) props("public").as[Boolean] else false
    
    LaserLambda(
      id = Some(lambda.id.get.toString), 
      eventFilter = Some(UUID.randomUUID.toString),
      public = isPublic,
      provider = Some(Json.parse(s"""{ "id": "${providerId.toString}", "location": "$location", "href": "/foo/bar" }""")), //props("provider"),
      LaserArtifactDescription(
          artifactUri = artifactUri,
          description = if (props.contains("description")) props("description").asOpt[String] else None,
          functionName = function,
          handler = handler,
          memorySize = props("memory").as[Int],
          cpus = props("cpus").as[Double],
          publish = false,             // <- currently not used
          role = "placeholder.role",   // <- currently not used
          runtime = props("runtime").as[String],
          timeoutSecs = props("timeout").as[Int],
          code = if (props.contains("code")) props("code").asOpt[String] else None
          ))
  }
  
  
  def syncLaserProviders(toResource: UUID) = ???
  
  
  
  def requiredProperty(props: Map[String,JsValue], key: String) = {
    if (props.contains(key)) props(key) else
      throw new BadRequestException(s"Missing property '$key'.")
  }
  
  def parseHandlerFunction(lambda: GestaltResourceInput) = {
    val props = lambda.properties.get
    val runtime = requiredProperty(props, "runtime") //props("runtime")
    val mapJson = requiredProperty(props, "function_mappings")
    val fmap = mapJson.validate[Map[String, JsValue]].map {
      case m: Map[String,JsValue] => m
    }.recoverTotal { e =>
      throw new BadRequestException(
        "Could not parse property 'function_mappings': " + 
          JsError.toFlatJson(e).toString)
    }
    
    //
    // TODO: Validation parsing.
    // TODO: I think i have the naming of this backward,
    // What I'm calling the function is actually the handler. It still works
    // just confusing. Fix.
    //
    val function = fmap.keys.toSeq(0)

    runtime.as[String] match {
      case "csharp" => (function, "")
      case _        => splitEntrypoint(function)
    }
  }
  
  def splitEntrypoint(ep: String): (String,String) = { 
    val ps = (for {
      m <- "^(.*)\\.(.*)$".r.findAllIn(ep.trim).matchData
      e <- m.subgroups
    } yield e).toList
    
    ps(0) -> ps(1)
  }
}

