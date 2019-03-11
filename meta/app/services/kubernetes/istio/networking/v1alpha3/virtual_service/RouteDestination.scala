package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** L4 routing rule weighted destination.
  *
  * @param destination
  *   REQUIRED. Destination uniquely identifies the instances of a service
  *   to which the request/connection should be forwarded to.
  * @param weight
  *   REQUIRED. The proportion of traffic to be forwarded to the service
  *   version. If there is only one destination in a rule, all traffic will be
  *   routed to it irrespective of the weight.
  */
case class RouteDestination(
  destination: Option[Destination] = None,
  weight: Int = 0
)
