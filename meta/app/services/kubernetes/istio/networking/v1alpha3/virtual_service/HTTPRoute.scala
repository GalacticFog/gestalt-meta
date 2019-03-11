package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Describes match conditions and actions for routing HTTP/1.1, HTTP2, and
  * gRPC traffic. See VirtualService for usage examples.
  *
  * @param match
  *   Match conditions to be satisfied for the rule to be
  *   activated. All conditions inside a single match block have AND
  *   semantics, while the list of match blocks have OR semantics. The rule
  *   is matched if any one of the match blocks succeed.
  * @param route
  *   A http rule can either redirect or forward (default) traffic. The
  *   forwarding target can be one of several versions of a service (see
  *   glossary in beginning of document). Weights associated with the
  *   service version determine the proportion of traffic it receives.
  * @param redirect
  *   A http rule can either redirect or forward (default) traffic. If
  *   traffic passthrough option is specified in the rule,
  *   route/redirect will be ignored. The redirect primitive can be used to
  *   send a HTTP 301 redirect to a different URI or Authority.
  * @param rewrite
  *   Rewrite HTTP URIs and Authority headers. Rewrite cannot be used with
  *   Redirect primitive. Rewrite will be performed before forwarding.
  * @param websocketUpgrade
  *   Deprecated. Websocket upgrades are done automatically starting from Istio 1.0.
  * @param timeout
  *   Timeout for HTTP requests.
  * @param retries
  *   Retry policy for HTTP requests.
  * @param fault
  *   Fault injection policy to apply on HTTP traffic at the client side.
  *   Note that timeouts or retries will not be enabled when faults are
  *   enabled on the client side.
  * @param mirror
  *   Mirror HTTP traffic to a another destination in addition to forwarding
  *   the requests to the intended destination. Mirrored traffic is on a
  *   best effort basis where the sidecar/gateway will not wait for the
  *   mirrored cluster to respond before returning the response from the
  *   original destination.  Statistics will be generated for the mirrored
  *   destination.
  * @param corsPolicy
  *   Cross-Origin Resource Sharing policy (CORS). Refer to
  *   https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS
  *   for further details about cross origin resource sharing.
  * @param appendHeaders
  *   Use of `append_headers` is deprecated. Use the `headers`
  *   field instead.
  * @param removeResponseHeaders
  *   Use of `remove_response_header` is deprecated. Use the `headers`
  *   field instead.
  * @param appendResponseHeaders
  *   Use of `append_response_headers` is deprecated. Use the `headers`
  *   field instead.
  * @param removeRequestHeaders
  *   Use of `remove_request_headers` is deprecated. Use the `headers`
  *   field instead.
  * @param appendRequestHeaders
  *   Use of `append_request_headers` is deprecated. Use the `headers`
  *   field instead.
  * @param headers
  *   Header manipulation rules
  */
case class HTTPRoute(
  `match`: List[HTTPMatchRequest] = Nil,
  route: List[HTTPRouteDestination] = Nil,
  redirect: Option[HTTPRedirect] = None,
  rewrite: Option[HTTPRewrite] = None,
  websocketUpgrade: Boolean = false,
  timeout: Option[services.kubernetes.istio.Duration.Duration] = None,
  retries: Option[HTTPRetry] = None,
  fault: Option[HTTPFaultInjection] = None,
  mirror: Option[Destination] = None,
  corsPolicy: Option[CorsPolicy] = None,
  appendHeaders: Map[String, String] = Map.empty,     // deprecated
  removeResponseHeaders: List[String] = Nil,     // deprecated
  appendResponseHeaders: Map[String, String] = Map.empty,     // deprecated
  removeRequestHeaders: List[String] = Nil,     // deprecated
  appendRequestHeaders: Map[String, String] = Map.empty,     // deprecated
  headers: Option[Headers] = None
)
