package services.kubernetes.istio.networking.v1alpha3.virtual_service

import skuber.ResourceSpecification.{Names, Scope}
import skuber._

/** A `VirtualService` defines a set of traffic routing rules to apply when a host is
  * addressed. Each routing rule defines matching criteria for traffic of a specific
  * protocol. If the traffic is matched, then it is sent to a named destination service
  * (or subset/version of it) defined in the registry.
  *
  * The source of traffic can also be matched in a routing rule. This allows routing
  * to be customized for specific client contexts.
  *
  * The following example on Kubernetes, routes all HTTP traffic by default to
  * pods of the reviews service with label "version: v1". In addition,
  * HTTP requests with path starting with /wpcatalog/ or /consumercatalog/ will
  * be rewritten to /newcatalog and sent to pods with label "version: v2".
  *
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
  *   - match:
  *     - uri:
  *         prefix: "/wpcatalog"
  *     - uri:
  *         prefix: "/consumercatalog"
  *     rewrite:
  *       uri: "/newcatalog"
  *     route:
  *     - destination:
  *         host: reviews.prod.svc.cluster.local
  *         subset: v2
  *   - route:
  *     - destination:
  *         host: reviews.prod.svc.cluster.local
  *         subset: v1
  * ```
  *
  * A subset/version of a route destination is identified with a reference
  * to a named service subset which must be declared in a corresponding
  * `DestinationRule`.
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
  * @param hosts
  *   REQUIRED. The destination hosts to which traffic is being sent. Could
  *   be a DNS name with wildcard prefix or an IP address.  Depending on the
  *   platform, short-names can also be used instead of a FQDN (i.e. has no
  *   dots in the name). In such a scenario, the FQDN of the host would be
  *   derived based on the underlying platform.
  *  
  *   **A host name can be defined by only one VirtualService**. A single
  *   VirtualService can be used to describe traffic properties for multiple
  *   HTTP and TCP ports.
  *  
  *   *Note for Kubernetes users*: When short names are used (e.g. "reviews"
  *   instead of "reviews.default.svc.cluster.local"), Istio will interpret
  *   the short name based on the namespace of the rule, not the service. A
  *   rule in the "default" namespace containing a host "reviews will be
  *   interpreted as "reviews.default.svc.cluster.local", irrespective of
  *   the actual namespace associated with the reviews service. _To avoid
  *   potential misconfigurations, it is recommended to always use fully
  *   qualified domain names over short names._
  *  
  *   The hosts field applies to both HTTP and TCP services. Service inside
  *   the mesh, i.e., those found in the service registry, must always be
  *   referred to using their alphanumeric names. IP addresses are allowed
  *   only for services defined via the Gateway.
  * @param gateways
  *   The names of gateways and sidecars that should apply these routes. A
  *   single VirtualService is used for sidecars inside the mesh as well as
  *   for one or more gateways. The selection condition imposed by this
  *   field can be overridden using the source field in the match conditions
  *   of protocol-specific routes. The reserved word `mesh` is used to imply
  *   all the sidecars in the mesh. When this field is omitted, the default
  *   gateway (`mesh`) will be used, which would apply the rule to all
  *   sidecars in the mesh. If a list of gateway names is provided, the
  *   rules will apply only to the gateways. To apply the rules to both
  *   gateways and sidecars, specify `mesh` as one of the gateway names.
  * @param http
  *   An ordered list of route rules for HTTP traffic. HTTP routes will be
  *   applied to platform service ports named 'http-*'/'http2-*'/'grpc-*', gateway
  *   ports with protocol HTTP/HTTP2/GRPC/ TLS-terminated-HTTPS and service
  *   entry ports using HTTP/HTTP2/GRPC protocols.  The first rule matching
  *   an incoming request is used.
  * @param tls
  *   An ordered list of route rule for non-terminated TLS &amp; HTTPS
  *   traffic. Routing is typically performed using the SNI value presented
  *   by the ClientHello message. TLS routes will be applied to platform
  *   service ports named 'https-*', 'tls-*', unterminated gateway ports using
  *   HTTPS/TLS protocols (i.e. with "passthrough" TLS mode) and service
  *   entry ports using HTTPS/TLS protocols.  The first rule matching an
  *   incoming request is used.  NOTE: Traffic 'https-*' or 'tls-*' ports
  *   without associated virtual service will be treated as opaque TCP
  *   traffic.
  * @param tcp
  *   An ordered list of route rules for opaque TCP traffic. TCP routes will
  *   be applied to any port that is not a HTTP or TLS port. The first rule
  *   matching an incoming request is used.
  * @param exportTo
  *   A list of namespaces to which this virtual service is exported. Exporting a
  *   virtual service allows it to used by sidecars and gateways defined in
  *   other namespaces. This feature provides a mechanism for service owners
  *   and mesh administrators to control the visibility of virtual services
  *   across namespace boundaries.
  *  
  *   If no namespaces are specified then the virtual service is exported to all
  *   namespaces by default.
  *  
  *   The value "." is reserved and defines an export to the same namespace that
  *   the virtual service is declared in, similarly the value "*" is reserved and
  *   defines an export to all namespaces.
  */
case class VirtualService(
  val kind: String = "VirtualService",
  override val apiVersion: String = "networking.istio.io/v1alpha3",
  val metadata: ObjectMeta = ObjectMeta(),
  spec: Option[VirtualService.Spec] = None
) extends ObjectResource

object VirtualService {
  case class Spec(
    hosts: List[String] = Nil,
    gateways: List[String] = Nil,
    http: List[HTTPRoute] = Nil,
    tls: List[TLSRoute] = Nil,
    tcp: List[TCPRoute] = Nil,
    exportTo: List[String] = Nil
  )

  val specification=NonCoreResourceSpecification(
    apiGroup = "networking.istio.io",
    version = "v1alpha3",
    scope = Scope.Namespaced,
    names = Names(
      plural = "virtualservices",
      singular = "virtualservice",
      kind = "VirtualService",
      shortNames = List()
    )
  )
  implicit val virtualServiceDef = new ResourceDefinition[VirtualService] { def spec=specification }
  implicit val virtualServiceListDef = new ResourceDefinition[ListResource[VirtualService]] { def spec=specification }
}