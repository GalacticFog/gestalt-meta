package services.kubernetes.istio.networking.v1alpha3.destination_rule

/** Connection pool settings for an upstream host. The settings apply to
  * each individual host in the upstream service.  See Envoy's [circuit
  * breaker](https://www.envoyproxy.io/docs/envoy/latest/intro/arch_overview/circuit_breaking)
  * for more details. Connection pool settings can be applied at the TCP
  * level as well as at HTTP level.
  *
  * For example, the following rule sets a limit of 100 connections to redis
  * service called myredissrv with a connect timeout of 30ms
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: bookinfo-redis
  * spec:
  *   host: myredissrv.prod.svc.cluster.local
  *   trafficPolicy:
  *     connectionPool:
  *       tcp:
  *         maxConnections: 100
  *         connectTimeout: 30ms
  *         tcpKeepalive:
  *           time: 7200s
  *           interval: 75s
  * ```
  *
  * @param tcp
  *   Settings common to both HTTP and TCP upstream connections.
  * @param http
  *   HTTP connection pool settings.
  */
case class ConnectionPoolSettings(
  tcp: Option[ConnectionPoolSettings.TCPSettings] = None,
  http: Option[ConnectionPoolSettings.HTTPSettings] = None
)

object ConnectionPoolSettings {
  /** Settings common to both HTTP and TCP upstream connections.
    *
    * @param maxConnections
    *   Maximum number of HTTP1 /TCP connections to a destination host.
    * @param connectTimeout
    *   TCP connection timeout.
    * @param tcpKeepalive
    *   If set then set SO_KEEPALIVE on the socket to enable TCP Keepalives.
    */
  case class TCPSettings(
    maxConnections: Int = 0,
    connectTimeout: Option[services.kubernetes.istio.Duration.Duration] = None,
    tcpKeepalive: Option[ConnectionPoolSettings.TCPSettings.TcpKeepalive] = None
  )
  
  object TCPSettings {
    /** TCP keepalive.
      *
      * @param probes
      *   Maximum number of keepalive probes to send without response before
      *   deciding the connection is dead. Default is to use the OS level configuration
      *   (unless overridden, Linux defaults to 9.)
      * @param time
      *   The time duration a connection needs to be idle before keep-alive
      *   probes start being sent. Default is to use the OS level configuration
      *   (unless overridden, Linux defaults to 7200s (ie 2 hours.)
      * @param interval
      *   The time duration between keep-alive probes.
      *   Default is to use the OS level configuration
      *   (unless overridden, Linux defaults to 75s.)
      */
    case class TcpKeepalive(
      probes: Int = 0,
      time: Option[services.kubernetes.istio.Duration.Duration] = None,
      interval: Option[services.kubernetes.istio.Duration.Duration] = None
    )
  }
  
  /** Settings applicable to HTTP1.1/HTTP2/GRPC connections.
    *
    * @param http1MaxPendingRequests
    *   Maximum number of pending HTTP requests to a destination. Default 1024.
    * @param http2MaxRequests
    *   Maximum number of requests to a backend. Default 1024.
    * @param maxRequestsPerConnection
    *   Maximum number of requests per connection to a backend. Setting this
    *   parameter to 1 disables keep alive.
    * @param maxRetries
    *   Maximum number of retries that can be outstanding to all hosts in a
    *   cluster at a given time. Defaults to 3.
    */
  case class HTTPSettings(
    http1MaxPendingRequests: Int = 0,
    http2MaxRequests: Int = 0,
    maxRequestsPerConnection: Int = 0,
    maxRetries: Int = 0
  )
}
