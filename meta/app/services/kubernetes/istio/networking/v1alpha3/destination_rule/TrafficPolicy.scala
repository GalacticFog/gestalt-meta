package services.kubernetes.istio.networking.v1alpha3.destination_rule

import services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Traffic policies to apply for a specific destination, across all
  * destination ports. See DestinationRule for examples.
  *
  * @param loadBalancer
  *   Settings controlling the load balancer algorithms.
  * @param connectionPool
  *   Settings controlling the volume of connections to an upstream service
  * @param outlierDetection
  *   Settings controlling eviction of unhealthy hosts from the load balancing pool
  * @param tls
  *   TLS related settings for connections to the upstream service.
  * @param portLevelSettings
  *   Traffic policies specific to individual ports. Note that port level
  *   settings will override the destination-level settings. Traffic
  *   settings specified at the destination-level will not be inherited when
  *   overridden by port-level settings, i.e. default values will be applied
  *   to fields omitted in port-level traffic policies.
  */
final case class TrafficPolicy(
  loadBalancer: Option[LoadBalancerSettings] = None,
  connectionPool: Option[ConnectionPoolSettings] = None,
  outlierDetection: Option[OutlierDetection] = None,
  tls: Option[TLSSettings] = None,
  portLevelSettings: List[TrafficPolicy.PortTrafficPolicy] = Nil
)

object TrafficPolicy {
  /** Traffic policies that apply to specific ports of the service
    *
    * @param port
    *   Specifies the port name or number of a port on the destination service
    *   on which this policy is being applied.
    *  
    *   Names must comply with DNS label syntax (rfc1035) and therefore cannot
    *   collide with numbers. If there are multiple ports on a service with
    *   the same protocol the names should be of the form &lt;protocol-name&gt;-&lt;DNS
    *   label&gt;.
    * @param loadBalancer
    *   Settings controlling the load balancer algorithms.
    * @param connectionPool
    *   Settings controlling the volume of connections to an upstream service
    * @param outlierDetection
    *   Settings controlling eviction of unhealthy hosts from the load balancing pool
    * @param tls
    *   TLS related settings for connections to the upstream service.
    */
  case class PortTrafficPolicy(
    port: Option[virtual_service.PortSelector] = None,
    loadBalancer: Option[LoadBalancerSettings] = None,
    connectionPool: Option[ConnectionPoolSettings] = None,
    outlierDetection: Option[OutlierDetection] = None,
    tls: Option[TLSSettings] = None
  )
}
