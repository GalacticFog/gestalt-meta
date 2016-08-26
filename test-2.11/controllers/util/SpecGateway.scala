package controllers
package util

import controllers.ApiGateway._
import org.specs2.matcher.JsonMatchers
import play.api.test.PlaySpecification

class SpecGateway extends PlaySpecification with JsonMatchers {

  def buildGatewayInfo(serviceUrl: String, publicUrl: Option[String]): String = ApiGateway.buildGatewayInfo(GatewayInput(
    name = "blah",
    description = None,
    resource_type = "blah",
    properties = GatewayInputProperties(
      config = GatewayInputConfig(
        auth = GatewayInputAuth("BASIC", "admin", "letmein"), url = serviceUrl, extra = publicUrl
      ),
      locations = Seq(GatewayInputLocation("blah", true))
    )
  )).toString

  "GatewayInput" should {

    "not specify port if not included in the URL" in {
      buildGatewayInfo("http://service", Some("https://public")) must
        /("protocol" -> "http") and /("host" -> "service") and not /("port") and
        /("gatewayProtocol" -> "https") and /("gatewayHost" -> "public") and not /("gatewayPort")
    }

    "specify port if included in the URL" in {
      buildGatewayInfo("https://service:1234", Some("http://public:4567")) must
        /("protocol" -> "https") and /("host" -> "service") and /("port" -> 1234) and
        /("gatewayProtocol" -> "http") and /("gatewayHost" -> "public") and /("gatewayPort" -> 4567)
    }

    "pass user credentials" in {
      buildGatewayInfo("http://localhost", Some("http://localhost")) must
        /("username" -> "admin") and /("password" -> "letmein")
    }

    "use the service protocol for the public access if absent" in {
      buildGatewayInfo("https://both:1234", None) must
        /("protocol" -> "https") and /("host" -> "both") and /("port" -> 1234) and
        /("gatewayProtocol" -> "https") and /("gatewayHost" -> "both") and /("gatewayPort" -> 1234)
    }

  }

}