package services.kubernetes.istio.json.networking.v1alpha3.virtual_service

import skuber._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import skuber.json.format._
import services.kubernetes.istio.networking.v1alpha3.virtual_service._

package object format {

  private def formatMaybeEmptyDouble(path: JsPath, omitEmpty: Boolean=true) : OFormat[Double] =
    path.formatNullable[Double].inmap[Double](_.getOrElse(0.0), i => if (omitEmpty && i==0.0) None else Some(i))

  implicit val formatPortSelector: Format[PortSelector] = (
    (JsPath \ "number").formatNullable[Int] and
    (JsPath \ "name").formatNullable[String]
  )(PortSelector.apply _, unlift(PortSelector.unapply))

  implicit lazy val formatDestination: Format[Destination] = (
    (JsPath \ "host").formatMaybeEmptyString() and
    (JsPath \ "subset").formatMaybeEmptyString() and
    (JsPath \ "port").formatNullable[PortSelector]
  )(Destination.apply _, unlift(Destination.unapply))

  implicit lazy val formatStringMatch: Format[StringMatch] = (
    (JsPath \ "exact").formatNullable[String] and
    (JsPath \ "prefix").formatNullable[String] and
    (JsPath \ "regex").formatNullable[String]
  )(StringMatch.apply _, unlift(StringMatch.unapply))

  implicit lazy val formatHTTPMatchRequest: Format[HTTPMatchRequest] = (
    (JsPath \ "uri").formatNullable[StringMatch] and
    (JsPath \ "scheme").formatNullable[StringMatch] and
    (JsPath \ "method").formatNullable[StringMatch] and
    (JsPath \ "authority").formatNullable[StringMatch] and
    (JsPath \ "headers").formatMaybeEmptyMap[StringMatch] and
    (JsPath \ "port").formatMaybeEmptyInt() and
    (JsPath \ "sourceLabels").formatMaybeEmptyMap[String] and
    (JsPath \ "gateways").formatMaybeEmptyList[String]
  )(HTTPMatchRequest.apply _, unlift(HTTPMatchRequest.unapply))

  implicit lazy val formatHeadersHeaderOperations: Format[Headers.HeaderOperations] = (
    (JsPath \ "set").formatMaybeEmptyMap[String] and
    (JsPath \ "add").formatMaybeEmptyMap[String] and
    (JsPath \ "remove").formatMaybeEmptyList[String]
  )(Headers.HeaderOperations.apply _, unlift(Headers.HeaderOperations.unapply))

  implicit lazy val formatHeaders: Format[Headers] = (
    (JsPath \ "request").formatNullable[Headers.HeaderOperations] and
    (JsPath \ "response").formatNullable[Headers.HeaderOperations]
  )(Headers.apply _, unlift(Headers.unapply))

  implicit lazy val formatHTTPRouteDestination: Format[HTTPRouteDestination] = (
    (JsPath \ "destination").formatNullable[Destination] and
    (JsPath \ "weight").formatMaybeEmptyInt() and
    (JsPath \ "removeResponseHeaders").formatMaybeEmptyList[String] and
    (JsPath \ "appendResponseHeaders").formatMaybeEmptyMap[String] and
    (JsPath \ "removeRequestHeaders").formatMaybeEmptyList[String] and
    (JsPath \ "appendRequestHeaders").formatMaybeEmptyMap[String] and
    (JsPath \ "headers").formatNullable[Headers]
  )(HTTPRouteDestination.apply _, unlift(HTTPRouteDestination.unapply))

  implicit lazy val formatHTTPRedirect: Format[HTTPRedirect] = (
    (JsPath \ "uri").formatMaybeEmptyString() and
    (JsPath \ "authority").formatMaybeEmptyString()
  )(HTTPRedirect.apply _, unlift(HTTPRedirect.unapply))

  implicit lazy val formatHTTPRewrite: Format[HTTPRewrite] = (
    (JsPath \ "uri").formatMaybeEmptyString() and
    (JsPath \ "authority").formatMaybeEmptyString()
  )(HTTPRewrite.apply _, unlift(HTTPRewrite.unapply))

  implicit lazy val formatHTTPRetry: Format[HTTPRetry] = (
    (JsPath \ "attempts").formatMaybeEmptyInt() and
    (JsPath \ "perTryTimeout").formatNullable[services.kubernetes.istio.Duration.Duration] and
    (JsPath \ "retryOn").formatMaybeEmptyString()
  )(HTTPRetry.apply _, unlift(HTTPRetry.unapply))

  implicit lazy val formatPercent: Format[Percent] = 
    formatMaybeEmptyDouble((JsPath \ "value")).inmap(u => Percent(u), p => p.value)

  implicit lazy val formatHTTPFaultInjectionDelay: Format[HTTPFaultInjection.Delay] = (
    (JsPath \ "percent").formatMaybeEmptyInt() and
    (JsPath \ "percentage").formatNullable[Percent] and
    (JsPath \ "fixedDelay").formatNullable[services.kubernetes.istio.Duration.Duration] and
    (JsPath \ "exponentialDelay").formatNullable[services.kubernetes.istio.Duration.Duration]
  )(HTTPFaultInjection.Delay.apply _, unlift(HTTPFaultInjection.Delay.unapply))

  implicit lazy val formatHTTPFaultInjectionAbort: Format[HTTPFaultInjection.Abort] = (
    (JsPath \ "percent").formatMaybeEmptyInt() and
    (JsPath \ "percentage").formatNullable[Percent] and
    (JsPath \ "httpStatus").formatNullable[Int] and
    (JsPath \ "grpcStatus").formatNullable[String] and
    (JsPath \ "http2Error").formatNullable[String]
  )(HTTPFaultInjection.Abort.apply _, unlift(HTTPFaultInjection.Abort.unapply))

  implicit lazy val formatHTTPFaultInjection: Format[HTTPFaultInjection] = (
    (JsPath \ "delay").formatNullable[HTTPFaultInjection.Delay] and
    (JsPath \ "abort").formatNullable[HTTPFaultInjection.Abort]
  )(HTTPFaultInjection.apply _, unlift(HTTPFaultInjection.unapply))

  implicit lazy val formatCorsPolicy: Format[CorsPolicy] = (
    (JsPath \ "allowOrigin").formatMaybeEmptyList[String] and
    (JsPath \ "allowMethods").formatMaybeEmptyList[String] and
    (JsPath \ "allowHeaders").formatMaybeEmptyList[String] and
    (JsPath \ "exposeHeaders").formatMaybeEmptyList[String] and
    (JsPath \ "maxAge").formatNullable[services.kubernetes.istio.Duration.Duration] and
    (JsPath \ "allowCredentials").formatNullable[Boolean]
  )(CorsPolicy.apply _, unlift(CorsPolicy.unapply))

  implicit lazy val formatHTTPRoute: Format[HTTPRoute] = (
    (JsPath \ "match").formatMaybeEmptyList[HTTPMatchRequest] and
    (JsPath \ "route").formatMaybeEmptyList[HTTPRouteDestination] and
    (JsPath \ "redirect").formatNullable[HTTPRedirect] and
    (JsPath \ "rewrite").formatNullable[HTTPRewrite] and
    (JsPath \ "websocketUpgrade").formatMaybeEmptyBoolean(false) and
    (JsPath \ "timeout").formatNullable[services.kubernetes.istio.Duration.Duration] and
    (JsPath \ "retries").formatNullable[HTTPRetry] and
    (JsPath \ "fault").formatNullable[HTTPFaultInjection] and
    (JsPath \ "mirror").formatNullable[Destination] and
    (JsPath \ "corsPolicy").formatNullable[CorsPolicy] and
    (JsPath \ "appendHeaders").formatMaybeEmptyMap[String] and
    (JsPath \ "removeResponseHeaders").formatMaybeEmptyList[String] and
    (JsPath \ "appendResponseHeaders").formatMaybeEmptyMap[String] and
    (JsPath \ "removeRequestHeaders").formatMaybeEmptyList[String] and
    (JsPath \ "appendRequestHeaders").formatMaybeEmptyMap[String] and
    (JsPath \ "headers").formatNullable[Headers]
  )(HTTPRoute.apply _, unlift(HTTPRoute.unapply))

  implicit lazy val formatRouteDestination: Format[RouteDestination] = (
    (JsPath \ "destination").formatNullable[Destination] and
    (JsPath \ "weight").formatMaybeEmptyInt()
  )(RouteDestination.apply _, unlift(RouteDestination.unapply))

  implicit lazy val formatL4MatchAttributes: Format[L4MatchAttributes] = (
    (JsPath \ "destinationSubnets").formatMaybeEmptyList[String] and
    (JsPath \ "port").formatMaybeEmptyInt() and
    (JsPath \ "sourceSubnet").formatMaybeEmptyString() and
    (JsPath \ "sourceLabels").formatMaybeEmptyMap[String] and
    (JsPath \ "gateways").formatMaybeEmptyList[String]
  )(L4MatchAttributes.apply _, unlift(L4MatchAttributes.unapply))

  implicit lazy val formatTCPRoute: Format[TCPRoute] = (
    (JsPath \ "match").formatMaybeEmptyList[L4MatchAttributes] and
    (JsPath \ "route").formatMaybeEmptyList[RouteDestination]
  )(TCPRoute.apply _, unlift(TCPRoute.unapply))

  implicit lazy val formatTLSMatchAttributes: Format[TLSMatchAttributes] = (
    (JsPath \ "sniHosts").formatMaybeEmptyList[String] and
    (JsPath \ "destinationSubnets").formatMaybeEmptyList[String] and
    (JsPath \ "port").formatMaybeEmptyInt() and
    (JsPath \ "sourceSubnet").formatMaybeEmptyString() and
    (JsPath \ "sourceLabels").formatMaybeEmptyMap[String] and
    (JsPath \ "gateways").formatMaybeEmptyList[String]
  )(TLSMatchAttributes.apply _, unlift(TLSMatchAttributes.unapply))

  implicit lazy val formatTLSRoute: Format[TLSRoute] = (
    (JsPath \ "match").formatMaybeEmptyList[TLSMatchAttributes] and
    (JsPath \ "route").formatMaybeEmptyList[RouteDestination]
  )(TLSRoute.apply _, unlift(TLSRoute.unapply))

  implicit lazy val formatVirtualServiceSpec: Format[VirtualService.Spec] = (
    (JsPath \ "hosts").formatMaybeEmptyList[String] and
    (JsPath \ "gateways").formatMaybeEmptyList[String] and
    (JsPath \ "http").formatMaybeEmptyList[HTTPRoute] and
    (JsPath \ "tls").formatMaybeEmptyList[TLSRoute] and
    (JsPath \ "tcp").formatMaybeEmptyList[TCPRoute] and
    (JsPath \ "exportTo").formatMaybeEmptyList[String]
  )(VirtualService.Spec.apply _, unlift(VirtualService.Spec.unapply))

  implicit lazy val formatVirtualService: Format[VirtualService] = (
    objFormat and
    (JsPath \ "spec").formatNullable[VirtualService.Spec]
  )(VirtualService.apply _, unlift(VirtualService.unapply))

  implicit val formatVirtualServiceList: Format[ListResource[VirtualService]] = ListResourceFormat[VirtualService]

}