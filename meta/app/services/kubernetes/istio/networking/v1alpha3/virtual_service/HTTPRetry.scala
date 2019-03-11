package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Describes the retry policy to use when a HTTP request fails. For
  * example, the following rule sets the maximum number of retries to 3 when
  * calling ratings:v1 service, with a 2s timeout per retry attempt.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: ratings-route
  * spec:
  *   hosts:
  *   - ratings.prod.svc.cluster.local
  *   http:
  *   - route:
  *     - destination:
  *         host: ratings.prod.svc.cluster.local
  *         subset: v1
  *     retries:
  *       attempts: 3
  *       perTryTimeout: 2s
  *       retryOn: gateway-error,connect-failure,refused-stream
  * ```
  *
  * @param attempts
  *   REQUIRED. Number of retries for a given request. The interval
  *   between retries will be determined automatically (25ms+). Actual
  *   number of retries attempted depends on the httpReqTimeout.
  * @param perTryTimeout
  *   Timeout per retry attempt for a given request. format: 1h/1m/1s/1ms. MUST BE &gt;=1ms.
  * @param retryOn
  *   Specifies the conditions under which retry takes place.
  *   One or more policies can be specified using a ‘,’ delimited list.
  *   The supported policies can be found in
  *   &lt;https://www.envoyproxy.io/docs/envoy/latest/configuration/http_filters/router_filter#x-envoy-retry-on&gt;
  *   and &lt;https://www.envoyproxy.io/docs/envoy/latest/configuration/http_filters/router_filter#x-envoy-retry-grpc-on&gt;
  */
case class HTTPRetry(
  attempts: Int = 0,
  perTryTimeout: Option[services.kubernetes.istio.Duration.Duration] = None,
  retryOn: String = ""
)