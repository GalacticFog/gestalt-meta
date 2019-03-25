package services.kubernetes.istio.networking.v1alpha3.service_entry

import skuber.ResourceSpecification.{Names, Scope}
import skuber._
import services.kubernetes.istio.networking.v1alpha3.gateway

/** `ServiceEntry` enables adding additional entries into Istio's internal
  * service registry, so that auto-discovered services in the mesh can
  * access/route to these manually specified services. A service entry
  * describes the properties of a service (DNS name, VIPs, ports, protocols,
  * endpoints). These services could be external to the mesh (e.g., web
  * APIs) or mesh-internal services that are not part of the platform's
  * service registry (e.g., a set of VMs talking to services in Kubernetes).
  *
  * The following configuration adds a set of MongoDB instances running on
  * unmanaged VMs to Istio's registry, so that these services can be treated
  * as any other service in the mesh. The associated DestinationRule is used
  * to initiate mTLS connections to the database instances.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: ServiceEntry
  * metadata:
  *   name: external-svc-mongocluster
  * spec:
  *   hosts:
  *   - mymongodb.somedomain # not used
  *   addresses:
  *   - 192.192.192.192/24 # VIPs
  *   ports:
  *   - number: 27018
  *     name: mongodb
  *     protocol: MONGO
  *   location: MESH_INTERNAL
  *   resolution: STATIC
  *   endpoints:
  *   - address: 2.2.2.2
  *   - address: 3.3.3.3
  * ```
  *
  * and the associated DestinationRule
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: mtls-mongocluster
  * spec:
  *   host: mymongodb.somedomain
  *   trafficPolicy:
  *     tls:
  *       mode: MUTUAL
  *       clientCertificate: /etc/certs/myclientcert.pem
  *       privateKey: /etc/certs/client_private_key.pem
  *       caCertificates: /etc/certs/rootcacerts.pem
  * ```
  *
  * The following example uses a combination of service entry and TLS
  * routing in virtual service to demonstrate the use of SNI routing to
  * forward unterminated TLS traffic from the application to external
  * services via the sidecar. The sidecar inspects the SNI value in the
  * ClientHello message to route to the appropriate external service.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: ServiceEntry
  * metadata:
  *   name: external-svc-https
  * spec:
  *   hosts:
  *   - api.dropboxapi.com
  *   - www.googleapis.com
  *   - api.facebook.com
  *   location: MESH_EXTERNAL
  *   ports:
  *   - number: 443
  *     name: https
  *     protocol: TLS
  *   resolution: DNS
  * ```
  *
  * And the associated VirtualService to route based on the SNI value.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: tls-routing
  * spec:
  *   hosts:
  *   - api.dropboxapi.com
  *   - www.googleapis.com
  *   - api.facebook.com
  *   tls:
  *   - match:
  *     - port: 443
  *       sniHosts:
  *       - api.dropboxapi.com
  *     route:
  *     - destination:
  *         host: api.dropboxapi.com
  *   - match:
  *     - port: 443
  *       sniHosts:
  *       - www.googleapis.com
  *     route:
  *     - destination:
  *         host: www.googleapis.com
  *   - match:
  *     - port: 443
  *       sniHosts:
  *       - api.facebook.com
  *     route:
  *     - destination:
  *         host: api.facebook.com
  *
  * ```
  *
  * The following example demonstrates the use of a dedicated egress gateway
  * through which all external service traffic is forwarded.
  * The 'exportTo' field allows for control over the visibility of a service
  * declaration to other namespaces in the mesh. By default a service is exported
  * to all namespaces. The following example restricts the visibility to the
  * current namespace, represented by ".", so that it cannot be used by other
  * namespaces.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: ServiceEntry
  * metadata:
  *   name: external-svc-httpbin
  *   namespace : egress
  * spec:
  *   hosts:
  *   - httpbin.com
  *   exportTo:
  *   - "."
  *   location: MESH_EXTERNAL
  *   ports:
  *   - number: 80
  *     name: http
  *     protocol: HTTP
  *   resolution: DNS
  * ```
  *
  * Define a gateway to handle all egress traffic.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: Gateway
  * metadata:
  *  name: istio-egressgateway
  *  namespace: egress
  * spec:
  *  selector:
  *    istio: egressgateway
  *  servers:
  *  - port:
  *      number: 80
  *      name: http
  *      protocol: HTTP
  *    hosts:
  *    - "*"
  * ```
  *
  * And the associated VirtualService to route from the sidecar to the
  * gateway service (istio-egressgateway.istio-system.svc.cluster.local), as
  * well as route from the gateway to the external service. Note that the
  * virtual service is exported to all namespaces enabling them to route traffic
  * through the gateway to the external service. Forcing traffic to go through
  * a managed middle proxy like this is a common practice.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: gateway-routing
  *   namespace: egress
  * spec:
  *   hosts:
  *   - httpbin.com
  *   exportTo:
  *   - *
  *   gateways:
  *   - mesh
  *   - istio-egressgateway
  *   http:
  *   - match:
  *     - port: 80
  *       gateways:
  *       - mesh
  *     route:
  *     - destination:
  *         host: istio-egressgateway.istio-system.svc.cluster.local
  *   - match:
  *     - port: 80
  *       gateway:
  *       - istio-egressgateway
  *     route:
  *     - destination:
  *         host: httpbin.com
  * ```
  *
  * The following example demonstrates the use of wildcards in the hosts for
  * external services. If the connection has to be routed to the IP address
  * requested by the application (i.e. application resolves DNS and attempts
  * to connect to a specific IP), the discovery mode must be set to `NONE`.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: ServiceEntry
  * metadata:
  *   name: external-svc-wildcard-example
  * spec:
  *   hosts:
  *   - "*.bar.com"
  *   location: MESH_EXTERNAL
  *   ports:
  *   - number: 80
  *     name: http
  *     protocol: HTTP
  *   resolution: NONE
  * ```
  *
  * The following example demonstrates a service that is available via a
  * Unix Domain Socket on the host of the client. The resolution must be
  * set to STATIC to use unix address endpoints.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: ServiceEntry
  * metadata:
  *   name: unix-domain-socket-example
  * spec:
  *   hosts:
  *   - "example.unix.local"
  *   location: MESH_EXTERNAL
  *   ports:
  *   - number: 80
  *     name: http
  *     protocol: HTTP
  *   resolution: STATIC
  *   endpoints:
  *   - address: unix:///var/run/example/socket
  * ```
  *
  * For HTTP-based services, it is possible to create a VirtualService
  * backed by multiple DNS addressable endpoints. In such a scenario, the
  * application can use the HTTP_PROXY environment variable to transparently
  * reroute API calls for the VirtualService to a chosen backend. For
  * example, the following configuration creates a non-existent external
  * service called foo.bar.com backed by three domains: us.foo.bar.com:8080,
  * uk.foo.bar.com:9080, and in.foo.bar.com:7080
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: ServiceEntry
  * metadata:
  *   name: external-svc-dns
  * spec:
  *   hosts:
  *   - foo.bar.com
  *   location: MESH_EXTERNAL
  *   ports:
  *   - number: 80
  *     name: http
  *     protocol: HTTP
  *   resolution: DNS
  *   endpoints:
  *   - address: us.foo.bar.com
  *     ports:
  *       https: 8080
  *   - address: uk.foo.bar.com
  *     ports:
  *       https: 9080
  *   - address: in.foo.bar.com
  *     ports:
  *       https: 7080
  * ```
  *
  * With `HTTP_PROXY=http://localhost/`, calls from the application to
  * `http://foo.bar.com` will be load balanced across the three domains
  * specified above. In other words, a call to `http://foo.bar.com/baz` would
  * be translated to `http://uk.foo.bar.com/baz`.
  *
  * The following example illustrates the usage of a ServiceEntry
  * containing a subject alternate name
  * whose format conforms to the SPIFEE standard
  * &lt;https://github.com/spiffe/spiffe/blob/master/standards/SPIFFE-ID.md&gt;:
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: ServiceEntry
  * metadata:
  *   name: httpbin
  *   namespace : httpbin-ns
  * spec:
  *   hosts:
  *   - httpbin.com
  *   location: MESH_INTERNAL
  *   ports:
  *   - number: 80
  *     name: http
  *     protocol: HTTP
  *   resolution: STATIC
  *   endpoints:
  *   - address: 2.2.2.2
  *   - address: 3.3.3.3
  *   subjectAltNames:
  *   - "spiffe://cluster.local/ns/httpbin-ns/sa/httpbin-service-account"
  * ```
  *
  * @param hosts
  *   REQUIRED. The hosts associated with the ServiceEntry. Could be a DNS
  *   name with wildcard prefix (external services only). DNS names in hosts
  *   will be ignored if the application accesses the service over non-HTTP
  *   protocols such as mongo/opaque TCP/HTTPS. In such scenarios, the
  *   IP addresses specified in the Addresses field or the port will be used
  *   to uniquely identify the destination.
  * @param addresses
  *   The virtual IP addresses associated with the service. Could be CIDR
  *   prefix. For HTTP services, the addresses field will be ignored and
  *   the destination will be identified based on the HTTP Host/Authority
  *   header. For non-HTTP protocols such as mongo/opaque TCP/HTTPS,
  *   the hosts will be ignored. If one or more IP addresses are specified,
  *   the incoming traffic will be identified as belonging to this service
  *   if the destination IP matches the IP/CIDRs specified in the addresses
  *   field. If the Addresses field is empty, traffic will be identified
  *   solely based on the destination port. In such scenarios, the port on
  *   which the service is being accessed must not be shared by any other
  *   service in the mesh. In other words, the sidecar will behave as a
  *   simple TCP proxy, forwarding incoming traffic on a specified port to
  *   the specified destination endpoint IP/host. Unix domain socket
  *   addresses are not supported in this field.
  * @param ports
  *   REQUIRED. The ports associated with the external service. If the
  *   Endpoints are unix domain socket addresses, there must be exactly one
  *   port.
  * @param location
  *   Specify whether the service should be considered external to the mesh
  *   or part of the mesh.
  * @param resolution
  *   REQUIRED: Service discovery mode for the hosts. Care must be taken
  *   when setting the resolution mode to NONE for a TCP port without
  *   accompanying IP addresses. In such cases, traffic to any IP on
  *   said port will be allowed (i.e. 0.0.0.0:&lt;port&gt;).
  * @param endpoints
  *   One or more endpoints associated with the service.
  * @param exportTo
  *   A list of namespaces to which this service is exported. Exporting a service
  *   allows it to used by sidecars, gateways and virtual services defined in
  *   other namespaces. This feature provides a mechanism for service owners
  *   and mesh administrators to control the visibility of services across
  *   namespace boundaries.
  *  
  *   If no namespaces are specified then the service is exported to all
  *   namespaces by default.
  *  
  *   The value "." is reserved and defines an export to the same namespace that
  *   the service is declared in, similarly the value "*" is reserved and
  *   defines an export to all namespaces.
  *  
  *   For a Kubernetes Service the equivalent effect can be achieved by setting
  *   the annotation "networking.istio.io/exportTo" to a comma-separated list
  *   of namespace names.
  * @param subjectAltNames
  *   The list of subject alternate names allowed for workloads that
  *   implement this service. This information is used to enforce
  *   secure-naming &lt;https://istio.io/docs/concepts/security/#secure-naming&gt;.
  *   If specified, the proxy will verify that the server
  *   certificate's subject alternate name matches one of the specified values.
  */
case class ServiceEntry(
  val kind: String = "ServiceEntry",
  override val apiVersion: String = "networking.istio.io/v1alpha3",
  val metadata: ObjectMeta = ObjectMeta(),
  spec: Option[ServiceEntry.Spec] = None
) extends ObjectResource

object ServiceEntry {
  case class Spec(
    hosts: List[String] = Nil,
    addresses: List[String] = Nil,
    ports: List[gateway.Port] = Nil,
    location: ServiceEntry.Location.Location = ServiceEntry.Location.MESH_EXTERNAL,
    resolution: ServiceEntry.Resolution.Resolution = ServiceEntry.Resolution.NONE,
    endpoints: List[ServiceEntry.Endpoint] = Nil,
    exportTo: List[String] = Nil,
    subjectAltNames: List[String] = Nil
  )

  val specification=NonCoreResourceSpecification(
    apiGroup = "networking.istio.io",
    version = "v1alpha3",
    scope = Scope.Namespaced,
    names = Names(
      plural = "serviceentries",
      singular = "serviceentry",
      kind = "ServiceEntry",
      shortNames = List()
    )
  )
  implicit val serviceEntriesDef = new ResourceDefinition[ServiceEntry] { def spec=specification }
  implicit val serviceEntriesListDef = new ResourceDefinition[ListResource[ServiceEntry]] { def spec=specification }

  object Location extends Enumeration {
    type Location = Value
    val MESH_EXTERNAL, MESH_INTERNAL = Value
  }

  object Resolution extends Enumeration {
    type Resolution = Value
    val NONE, STATIC, DNS = Value
  }

  /** Endpoint defines a network address (IP or hostname) associated with
    * the mesh service.
    *
    * @param address
    *   REQUIRED: Address associated with the network endpoint without the
    *   port.  Domain names can be used if and only if the resolution is set
    *   to DNS, and must be fully-qualified without wildcards. Use the form
    *   unix:///absolute/path/to/socket for unix domain socket endpoints.
    * @param ports
    *   Set of ports associated with the endpoint. The ports must be
    *   associated with a port name that was declared as part of the
    *   service. Do not use for unix:// addresses.
    * @param labels
    *   One or more labels associated with the endpoint.
    * @param network
    *   Network enables Istio to group endpoints resident in the same L3
    *   domain/network. All endpoints in the same network are assumed to be
    *   directly reachable from one another. When endpoints in different
    *   networks cannot reach each other directly, an Istio Gateway can be
    *   used to establish connectivity (usually using the
    *   AUTO_PASSTHROUGH mode in a Gateway Server). This is
    *   an advanced configuration used typically for spanning an Istio mesh
    *   over multiple clusters.
    * @param locality
    *   The locality associated with the endpoint. A locality corresponds
    *   to a failure domain (e.g., country/region/zone). Arbitrary failure
    *   domain hierarchies can be represented by separating each
    *   encapsulating failure domain by /. For example, the locality of an
    *   an endpoint in US, in US-East-1 region, within availability zone
    *   az-1, in data center rack r11 can be represented as
    *   us/us-east-1/az-1/r11. Istio will configure the sidecar to route to
    *   endpoints within the same locality as the sidecar. If none of the
    *   endpoints in the locality are available, endpoints parent locality
    *   (but within the same network ID) will be chosen. For example, if
    *   there are two endpoints in same network (networkID "n1"), say e1
    *   with locality us/us-east-1/az-1/r11 and e2 with locality
    *   us/us-east-1/az-2/r12, a sidecar from us/us-east-1/az-1/r11 locality
    *   will prefer e1 from the same locality over e2 from a different
    *   locality. Endpoint e2 could be the IP associated with a gateway
    *   (that bridges networks n1 and n2), or the IP associated with a
    *   standard service endpoint.
    * @param weight
    *   The load balancing weight associated with the endpoint. Endpoints
    *   with higher weights will receive proportionally higher traffic.
    */
  case class Endpoint(
    address: String = "",
    ports: Map[String, Int] = Map.empty,
    labels: Map[String, String] = Map.empty,
    network: String = "",
    locality: String = "",
    weight: Int = 0
  )
}