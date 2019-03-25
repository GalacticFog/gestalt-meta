package services.kubernetes.istio.networking.v1alpha3.gateway

import skuber.ResourceSpecification.{Names, Scope}
import skuber._

/** `Gateway` describes a load balancer operating at the edge of the mesh
  * receiving incoming or outgoing HTTP/TCP connections. The specification
  * describes a set of ports that should be exposed, the type of protocol to
  * use, SNI configuration for the load balancer, etc.
  *
  * For example, the following Gateway configuration sets up a proxy to act
  * as a load balancer exposing port 80 and 9080 (http), 443 (https), and
  * port 2379 (TCP) for ingress.  The gateway will be applied to the proxy
  * running on a pod with labels `app: my-gateway-controller`. While Istio
  * will configure the proxy to listen on these ports, it is the
  * responsibility of the user to ensure that external traffic to these
  * ports are allowed into the mesh.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: Gateway
  * metadata:
  *   name: my-gateway
  *   namespace: some-config-namespace
  * spec:
  *   selector:
  *     app: my-gateway-controller
  *   servers:
  *   - port:
  *       number: 80
  *       name: http
  *       protocol: HTTP
  *     hosts:
  *     - uk.bookinfo.com
  *     - eu.bookinfo.com
  *     tls:
  *       httpsRedirect: true # sends 301 redirect for http requests
  *   - port:
  *       number: 443
  *       name: https
  *       protocol: HTTPS
  *     hosts:
  *     - uk.bookinfo.com
  *     - eu.bookinfo.com
  *     tls:
  *       mode: SIMPLE #enables HTTPS on this port
  *       serverCertificate: /etc/certs/servercert.pem
  *       privateKey: /etc/certs/privatekey.pem
  *   - port:
  *       number: 9080
  *       name: http-wildcard
  *       protocol: HTTP
  *     hosts:
  *     - "*"
  *   - port:
  *       number: 2379 # to expose internal service via external port 2379
  *       name: mongo
  *       protocol: MONGO
  *     hosts:
  *     - "*"
  * ```
  * The Gateway specification above describes the L4-L6 properties of a load
  * balancer. A `VirtualService` can then be bound to a gateway to control
  * the forwarding of traffic arriving at a particular host or gateway port.
  *
  * For example, the following VirtualService splits traffic for
  * "https://uk.bookinfo.com/reviews", "https://eu.bookinfo.com/reviews",
  * "http://uk.bookinfo.com:9080/reviews",
  * "http://eu.bookinfo.com:9080/reviews" into two versions (prod and qa) of
  * an internal reviews service on port 9080. In addition, requests
  * containing the cookie "user: dev-123" will be sent to special port 7777
  * in the qa version. The same rule is also applicable inside the mesh for
  * requests to the "reviews.prod.svc.cluster.local" service. This rule is
  * applicable across ports 443, 9080. Note that "http://uk.bookinfo.com"
  * gets redirected to "https://uk.bookinfo.com" (i.e. 80 redirects to 443).
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: bookinfo-rule
  *   namespace: bookinfo-namespace
  * spec:
  *   hosts:
  *   - reviews.prod.svc.cluster.local
  *   - uk.bookinfo.com
  *   - eu.bookinfo.com
  *   gateways:
  *   - some-config-namespace/my-gateway
  *   - mesh # applies to all the sidecars in the mesh
  *   http:
  *   - match:
  *     - headers:
  *         cookie:
  *           user: dev-123
  *     route:
  *     - destination:
  *         port:
  *           number: 7777
  *         host: reviews.qa.svc.cluster.local
  *   - match:
  *       uri:
  *         prefix: /reviews/
  *     route:
  *     - destination:
  *         port:
  *           number: 9080 # can be omitted if its the only port for reviews
  *         host: reviews.prod.svc.cluster.local
  *       weight: 80
  *     - destination:
  *         host: reviews.qa.svc.cluster.local
  *       weight: 20
  * ```
  *
  * The following VirtualService forwards traffic arriving at (external)
  * port 27017 to internal Mongo server on port 5555. This rule is not
  * applicable internally in the mesh as the gateway list omits the
  * reserved name `mesh`.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: bookinfo-Mongo
  *   namespace: bookinfo-namespace
  * spec:
  *   hosts:
  *   - mongosvr.prod.svc.cluster.local #name of internal Mongo service
  *   gateways:
  *   - some-config-namespace/my-gateway # can omit the namespace if gateway is in same
  *                                        namespace as virtual service.
  *   tcp:
  *   - match:
  *     - port: 27017
  *     route:
  *     - destination:
  *         host: mongo.prod.svc.cluster.local
  *         port:
  *           number: 5555
  * ```
  *
  * @param servers
  *   REQUIRED: A list of server specifications.
  * @param selector
  *   REQUIRED: One or more labels that indicate a specific set of pods/VMs
  *   on which this gateway configuration should be applied. The scope of
  *   label search is restricted to the configuration namespace in which the
  *   the resource is present. In other words, the Gateway resource must
  *   reside in the same namespace as the gateway workload.
  */
case class Gateway(
  val kind: String = "Gateway",
  override val apiVersion: String = "networking.istio.io/v1alpha3",
  val metadata: ObjectMeta = ObjectMeta(),
  spec: Option[Gateway.Spec] = None
) extends ObjectResource

object Gateway {
  case class Spec(
    servers: List[Server] = Nil,
    selector: Map[String, String] = Map.empty
  )

  val specification=NonCoreResourceSpecification(
    apiGroup = "networking.istio.io",
    version = "v1alpha3",
    scope = Scope.Namespaced,
    names = Names(
      plural = "gateways",
      singular = "gateway",
      kind = "Gateway",
      shortNames = List()
    )
  )
  implicit val gatewayDef = new ResourceDefinition[Gateway] { def spec=specification }
  implicit val gatewayListDef = new ResourceDefinition[ListResource[Gateway]] { def spec=specification }
}