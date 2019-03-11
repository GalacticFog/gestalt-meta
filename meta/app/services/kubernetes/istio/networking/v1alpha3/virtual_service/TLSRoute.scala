package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Describes match conditions and actions for routing unterminated TLS
  * traffic (TLS/HTTPS) The following routing rule forwards unterminated TLS
  * traffic arriving at port 443 of gateway called "mygateway" to internal
  * services in the mesh based on the SNI value.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: bookinfo-sni
  * spec:
  *   hosts:
  *   - "*.bookinfo.com"
  *   gateways:
  *   - mygateway
  *   tls:
  *   - match:
  *     - port: 443
  *       sniHosts:
  *       - login.bookinfo.com
  *     route:
  *     - destination:
  *         host: login.prod.svc.cluster.local
  *   - match:
  *     - port: 443
  *       sniHosts:
  *       - reviews.bookinfo.com
  *     route:
  *     - destination:
  *         host: reviews.prod.svc.cluster.local
  * ```
  *
  * @param match
  *   REQUIRED. Match conditions to be satisfied for the rule to be
  *   activated. All conditions inside a single match block have AND
  *   semantics, while the list of match blocks have OR semantics. The rule
  *   is matched if any one of the match blocks succeed.
  * @param route
  *   The destination to which the connection should be forwarded to.
  */
case class TLSRoute(
  `match`: List[TLSMatchAttributes] = Nil,
  route: List[RouteDestination] = Nil
)