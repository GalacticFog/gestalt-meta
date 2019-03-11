package services.kubernetes.istio.networking.v1alpha3.destination_rule

/** Load balancing policies to apply for a specific destination. See Envoy's
  * load balancing
  * [documentation](https://www.envoyproxy.io/docs/envoy/latest/intro/arch_overview/load_balancing/load_balancing)
  * for more details.
  *
  * For example, the following rule uses a round robin load balancing policy
  * for all traffic going to the ratings service.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: bookinfo-ratings
  * spec:
  *   host: ratings.prod.svc.cluster.local
  *   trafficPolicy:
  *     loadBalancer:
  *       simple: ROUND_ROBIN
  * ```
  *
  * The following example sets up sticky sessions for the ratings service
  * hashing-based load balancer for the same ratings service using the
  * the User cookie as the hash key.
  *
  * ```yaml
  *  apiVersion: networking.istio.io/v1alpha3
  *  kind: DestinationRule
  *  metadata:
  *    name: bookinfo-ratings
  *  spec:
  *    host: ratings.prod.svc.cluster.local
  *    trafficPolicy:
  *      loadBalancer:
  *        consistentHash:
  *          httpCookie:
  *            name: user
  *            ttl: 0s
  * ```
  */
case class LoadBalancerSettings(
  simple: Option[LoadBalancerSettings.SimpleLB.SimpleLB] = None,    // one of these two fields must be present
  consistentHash: Option[LoadBalancerSettings.ConsistentHashLB] = None
)

object LoadBalancerSettings {

  object SimpleLB extends Enumeration {
    type SimpleLB = Value
    val ROUND_ROBIN, LEAST_CONN, RANDOM, PASSTHROUGH = Value
  }

  /** Consistent Hash-based load balancing can be used to provide soft
    * session affinity based on HTTP headers, cookies or other
    * properties. This load balancing policy is applicable only for HTTP
    * connections. The affinity to a particular destination host will be
    * lost when one or more hosts are added/removed from the destination
    * service.
    *
    * @param minimumRingSize
    *   The minimum number of virtual nodes to use for the hash
    *   ring. Defaults to 1024. Larger ring sizes result in more granular
    *   load distributions. If the number of hosts in the load balancing
    *   pool is larger than the ring size, each host will be assigned a
    *   single virtual node.
    */
  case class ConsistentHashLB(
    minimumRingSize: Long = 0L,
    httpHeaderName: Option[String],    // one of these three fields must be present
    httpCookie: Option[ConsistentHashLB.HTTPCookie],
    useSourceIp: Option[Boolean]
  )
  
  object ConsistentHashLB {

    /** Describes a HTTP cookie that will be used as the hash key for the
      * Consistent Hash load balancer. If the cookie is not present, it will
      * be generated.
      *
      * @param name
      *   REQUIRED. Name of the cookie.
      * @param path
      *   Path to set for the cookie.
      * @param ttl
      *   REQUIRED. Lifetime of the cookie.
      */
    case class HTTPCookie(
      name: String = "",
      path: String = "",
      ttl: Option[services.kubernetes.istio.Duration.Duration] = None
    )
  }
}
