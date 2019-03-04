package services.kubernetes.istio.networking.v1alpha3.destination_rule

/** A Circuit breaker implementation that tracks the status of each
  * individual host in the upstream service.  Applicable to both HTTP and
  * TCP services.  For HTTP services, hosts that continually return 5xx
  * errors for API calls are ejected from the pool for a pre-defined period
  * of time. For TCP services, connection timeouts or connection
  * failures to a given host counts as an error when measuring the
  * consecutive errors metric. See Envoy's [outlier
  * detection](https://www.envoyproxy.io/docs/envoy/latest/intro/arch_overview/outlier)
  * for more details.
  *
  * The following rule sets a connection pool size of 100 connections and
  * 1000 concurrent HTTP2 requests, with no more than 10 req/connection to
  * "reviews" service. In addition, it configures upstream hosts to be
  * scanned every 5 mins, such that any host that fails 7 consecutive times
  * with 5XX error code will be ejected for 15 minutes.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: reviews-cb-policy
  * spec:
  *   host: reviews.prod.svc.cluster.local
  *   trafficPolicy:
  *     connectionPool:
  *       tcp:
  *         maxConnections: 100
  *       http:
  *         http2MaxRequests: 1000
  *         maxRequestsPerConnection: 10
  *     outlierDetection:
  *       consecutiveErrors: 7
  *       interval: 5m
  *       baseEjectionTime: 15m
  * ```
  *
  * @param consecutiveErrors
  *   Number of errors before a host is ejected from the connection
  *   pool. Defaults to 5. When the upstream host is accessed over HTTP, a
  *   502, 503 or 504 return code qualifies as an error. When the upstream host
  *   is accessed over an opaque TCP connection, connect timeouts and
  *   connection error/failure events qualify as an error.
  * @param interval
  *   Time interval between ejection sweep analysis. format:
  *   1h/1m/1s/1ms. MUST BE &gt;=1ms. Default is 10s.
  * @param baseEjectionTime
  *   Minimum ejection duration. A host will remain ejected for a period
  *   equal to the product of minimum ejection duration and the number of
  *   times the host has been ejected. This technique allows the system to
  *   automatically increase the ejection period for unhealthy upstream
  *   servers. format: 1h/1m/1s/1ms. MUST BE &gt;=1ms. Default is 30s.
  * @param maxEjectionPercent
  *   Maximum % of hosts in the load balancing pool for the upstream
  *   service that can be ejected. Defaults to 10%.
  * @param minHealthPercent
  *   Outlier detection will be enabled as long as the associated load balancing
  *   pool has at least min_health_percent hosts in healthy mode. When the
  *   percentage of healthy hosts in the load balancing pool drops below this
  *   threshold, outlier detection will be disabled and the proxy will load balance
  *   across all hosts in the pool (healthy and unhealthy).  The default is 50%.
  */
case class OutlierDetection(
  consecutiveErrors: Int = 0,
  interval: Option[services.kubernetes.istio.Duration.Duration] = None,
  baseEjectionTime: Option[services.kubernetes.istio.Duration.Duration] = None,
  maxEjectionPercent: Int = 0,
  minHealthPercent: Int = 0
)