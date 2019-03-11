package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Describes match conditions and actions for routing TCP traffic. The
  * following routing rule forwards traffic arriving at port 27017 for
  * mongo.prod.svc.cluster.local to another Mongo server on port 5555.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: bookinfo-Mongo
  * spec:
  *   hosts:
  *   - mongo.prod.svc.cluster.local
  *   tcp:
  *   - match:
  *     - port: 27017
  *     route:
  *     - destination:
  *         host: mongo.backup.svc.cluster.local
  *         port:
  *           number: 5555
  * ```
  *
  * @param match
  *   Match conditions to be satisfied for the rule to be
  *   activated. All conditions inside a single match block have AND
  *   semantics, while the list of match blocks have OR semantics. The rule
  *   is matched if any one of the match blocks succeed.
  * @param route
  *   The destination to which the connection should be forwarded to.
  */
case class TCPRoute(
  `match`: List[L4MatchAttributes] = Nil,
  route: List[RouteDestination] = Nil
)