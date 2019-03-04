package services.kubernetes.istio.networking.v1alpha3.gateway

/** `Server` describes the properties of the proxy on a given load balancer
  * port. For example,
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: Gateway
  * metadata:
  *   name: my-ingress
  * spec:
  *   selector:
  *     app: my-ingress-gateway
  *   servers:
  *   - port:
  *       number: 80
  *       name: http2
  *       protocol: HTTP2
  *     hosts:
  *     - "*"
  * ```
  *
  * Another example
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: Gateway
  * metadata:
  *   name: my-tcp-ingress
  * spec:
  *   selector:
  *     app: my-tcp-ingress-gateway
  *   servers:
  *   - port:
  *       number: 27018
  *       name: mongo
  *       protocol: MONGO
  *     hosts:
  *     - "*"
  * ```
  *
  * The following is an example of TLS configuration for port 443
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: Gateway
  * metadata:
  *   name: my-tls-ingress
  * spec:
  *   selector:
  *     app: my-tls-ingress-gateway
  *   servers:
  *   - port:
  *       number: 443
  *       name: https
  *       protocol: HTTPS
  *     hosts:
  *     - "*"
  *     tls:
  *       mode: SIMPLE
  *       serverCertificate: /etc/certs/server.pem
  *       privateKey: /etc/certs/privatekey.pem
  * ```
  *
  * @param port
  *   REQUIRED: The Port on which the proxy should listen for incoming
  *   connections. If using unix domain socket, use 0 as the port number,
  *   with a valid protocol and port name, along with the bind parameter.
  * @param bind
  *   The ip or the unix domain socket to which the listener should be bound
  *   to. Format: x.x.x.x or unix:///path/to/uds or unix://&#64;foobar (Linux
  *   abstract namespace).
  * @param hosts
  *   REQUIRED. A list of hosts exposed by this gateway. At least one
  *   host is required. While typically applicable to
  *   HTTP services, it can also be used for TCP services using TLS with
  *   SNI. May contain a wildcard prefix for the bottom-level component of
  *   a domain name. For example `*.foo.com` matches `bar.foo.com`
  *   and `*.com` matches `bar.foo.com`, `example.com`, and so on.
  *  
  *   **Note**: A `VirtualService` that is bound to a gateway must have one
  *   or more hosts that match the hosts specified in a server. The match
  *   could be an exact match or a suffix match with the server's hosts. For
  *   example, if the server's hosts specifies "*.example.com",
  *   VirtualServices with hosts dev.example.com, prod.example.com will
  *   match. However, VirtualServices with hosts example.com or
  *   newexample.com will not match.
  * @param tls
  *   Set of TLS related options that govern the server's behavior. Use
  *   these options to control if all http requests should be redirected to
  *   https, and the TLS modes to use.
  * @param defaultEndpoint
  *   The loopback IP endpoint or unix domain socket to which traffic should
  *   be forwarded to by default. Format should be 127.0.0.1:PORT or
  *   unix:///path/to/socket or unix://&#64;foobar (Linux abstract namespace).
  */
case class Server(
  port: Option[Port] = None,
  bind: String = "",
  hosts: List[String] = Nil,
  tls: Option[Server.TLSOptions] = None,
  defaultEndpoint: String = ""
)

object Server {
  /** @param httpsRedirect
    *   If set to true, the load balancer will send a 301 redirect for all
    *   http connections, asking the clients to use HTTPS.
    * @param mode
    *   Optional: Indicates whether connections to this port should be
    *   secured using TLS. The value of this field determines how TLS is
    *   enforced.
    * @param serverCertificate
    *   REQUIRED if mode is `SIMPLE` or `MUTUAL`. The path to the file
    *   holding the server-side TLS certificate to use.
    * @param privateKey
    *   REQUIRED if mode is `SIMPLE` or `MUTUAL`. The path to the file
    *   holding the server's private key.
    * @param caCertificates
    *   REQUIRED if mode is `MUTUAL`. The path to a file containing
    *   certificate authority certificates to use in verifying a presented
    *   client side certificate.
    * @param credentialName
    *   The credentialName stands for a unique identifier that can be used
    *   to identify the serverCertificate and the privateKey. The credentialName
    *   appended with suffix "-cacert" is used to identify the CaCertificates
    *   associated with this server. Gateway workloads capable of fetching
    *   credentials from a remote credential store will be configured to retrieve
    *   the serverCertificate and the privateKey using credentialName, instead of
    *   using the file system paths specified above. If using mutual TLS,
    *   gateway workloads will retrieve the CaCertificates using
    *   credentialName-cacert. The semantics of the name are platform dependent.
    *   In Kubernetes, the default Istio supplied credential server expects the
    *   credentialName to match the name of the Kubernetes secret that holds the
    *   server certificate, the private key, and the CA certificate
    *   (if using mutual TLS).
    * @param subjectAltNames
    *   A list of alternate names to verify the subject identity in the
    *   certificate presented by the client.
    * @param minProtocolVersion
    *   Optional: Minimum TLS protocol version.
    * @param maxProtocolVersion
    *   Optional: Maximum TLS protocol version.
    * @param cipherSuites
    *   Optional: If specified, only support the specified cipher list.
    *   Otherwise default to the default cipher list supported by Envoy.
    */
  case class TLSOptions(
    httpsRedirect: Boolean = false,
    mode: Server.TLSOptions.TLSmode.TLSmode = Server.TLSOptions.TLSmode.PASSTHROUGH,
    serverCertificate: String = "",
    privateKey: String = "",
    caCertificates: String = "",
    credentialName: String = "",
    subjectAltNames: List[String] = Nil,
    minProtocolVersion: Server.TLSOptions.TLSProtocol.TLSProtocol = Server.TLSOptions.TLSProtocol.TLS_AUTO,
    maxProtocolVersion: Server.TLSOptions.TLSProtocol.TLSProtocol = Server.TLSOptions.TLSProtocol.TLS_AUTO,
    cipherSuites: List[String] = Nil
  )
  
  object TLSOptions {
    object TLSmode extends Enumeration {
      type TLSmode = Value
      val PASSTHROUGH, SIMPLE, MUTUAL, AUTO_PASSTHROUGH = Value
    }
    object TLSProtocol extends Enumeration {
      type TLSProtocol = Value
      val TLS_AUTO, TLSV1_0, TLSV1_1, TLSV1_2, TLSV1_3 = Value
    }
  }
}
