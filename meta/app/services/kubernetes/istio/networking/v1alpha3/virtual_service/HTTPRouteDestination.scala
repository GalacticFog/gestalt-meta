package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Each routing rule is associated with one or more service versions (see
  * glossary in beginning of document). Weights associated with the version
  * determine the proportion of traffic it receives. For example, the
  * following rule will route 25% of traffic for the "reviews" service to
  * instances with the "v2" tag and the remaining traffic (i.e., 75%) to
  * "v1".
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: reviews-route
  * spec:
  *   hosts:
  *   - reviews.prod.svc.cluster.local
  *   http:
  *   - route:
  *     - destination:
  *         host: reviews.prod.svc.cluster.local
  *         subset: v2
  *       weight: 25
  *     - destination:
  *         host: reviews.prod.svc.cluster.local
  *         subset: v1
  *       weight: 75
  * ```
  *
  * And the associated DestinationRule
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: reviews-destination
  * spec:
  *   host: reviews.prod.svc.cluster.local
  *   subsets:
  *   - name: v1
  *     labels:
  *       version: v1
  *   - name: v2
  *     labels:
  *       version: v2
  * ```
  *
  * Traffic can also be split across two entirely different services without
  * having to define new subsets. For example, the following rule forwards 25% of
  * traffic to reviews.com to dev.reviews.com
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: reviews-route-two-domains
  * spec:
  *   hosts:
  *   - reviews.com
  *   http:
  *   - route:
  *     - destination:
  *         host: dev.reviews.com
  *       weight: 25
  *     - destination:
  *         host: reviews.com
  *       weight: 75
  * ```
  *
  * @param destination
  *   REQUIRED. Destination uniquely identifies the instances of a service
  *   to which the request/connection should be forwarded to.
  * @param weight
  *   REQUIRED. The proportion of traffic to be forwarded to the service
  *   version. (0-100). Sum of weights across destinations SHOULD BE == 100.
  *   If there is only one destination in a rule, the weight value is assumed to
  *   be 100.
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
case class HTTPRouteDestination(
  destination: Option[Destination] = None,
  weight: Int = 0,
  removeResponseHeaders: List[String] = Nil,     // deprecated
  appendResponseHeaders: Map[String, String] = Map.empty,     // deprecated
  removeRequestHeaders: List[String] = Nil,     // deprecated
  appendRequestHeaders: Map[String, String] = Map.empty,     // deprecated
  headers: Option[Headers] = None
)
