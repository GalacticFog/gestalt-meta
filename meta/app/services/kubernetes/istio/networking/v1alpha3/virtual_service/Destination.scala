package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Destination indicates the network addressable service to which the
  * request/connection will be sent after processing a routing rule. The
  * destination.host should unambiguously refer to a service in the service
  * registry. Istio's service registry is composed of all the services found
  * in the platform's service registry (e.g., Kubernetes services, Consul
  * services), as well as services declared through the
  * [ServiceEntry](#ServiceEntry) resource.
  *
  * *Note for Kubernetes users*: When short names are used (e.g. "reviews"
  * instead of "reviews.default.svc.cluster.local"), Istio will interpret
  * the short name based on the namespace of the rule, not the service. A
  * rule in the "default" namespace containing a host "reviews will be
  * interpreted as "reviews.default.svc.cluster.local", irrespective of the
  * actual namespace associated with the reviews service. _To avoid potential
  * misconfigurations, it is recommended to always use fully qualified
  * domain names over short names._
  *
  * The following Kubernetes example routes all traffic by default to pods
  * of the reviews service with label "version: v1" (i.e., subset v1), and
  * some to subset v2, in a kubernetes environment.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: reviews-route
  *   namespace: foo
  * spec:
  *   hosts:
  *   - reviews # interpreted as reviews.foo.svc.cluster.local
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
  *         host: reviews # interpreted as reviews.foo.svc.cluster.local
  *         subset: v2
  *   - route:
  *     - destination:
  *         host: reviews # interpreted as reviews.foo.svc.cluster.local
  *         subset: v1
  * ```
  *
  * And the associated DestinationRule
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: reviews-destination
  *   namespace: foo
  * spec:
  *   host: reviews # interpreted as reviews.foo.svc.cluster.local
  *   subsets:
  *   - name: v1
  *     labels:
  *       version: v1
  *   - name: v2
  *     labels:
  *       version: v2
  * ```
  *
  * The following VirtualService sets a timeout of 5s for all calls to
  * productpage.prod.svc.cluster.local service in Kubernetes. Notice that
  * there are no subsets defined in this rule. Istio will fetch all
  * instances of productpage.prod.svc.cluster.local service from the service
  * registry and populate the sidecar's load balancing pool. Also, notice
  * that this rule is set in the istio-system namespace but uses the fully
  * qualified domain name of the productpage service,
  * productpage.prod.svc.cluster.local. Therefore the rule's namespace does
  * not have an impact in resolving the name of the productpage service.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: my-productpage-rule
  *   namespace: istio-system
  * spec:
  *   hosts:
  *   - productpage.prod.svc.cluster.local # ignores rule namespace
  *   http:
  *   - timeout: 5s
  *     route:
  *     - destination:
  *         host: productpage.prod.svc.cluster.local
  * ```
  *
  * To control routing for traffic bound to services outside the mesh, external
  * services must first be added to Istio's internal service registry using the
  * ServiceEntry resource. VirtualServices can then be defined to control traffic
  * bound to these external services. For example, the following rules define a
  * Service for wikipedia.org and set a timeout of 5s for http requests.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: ServiceEntry
  * metadata:
  *   name: external-svc-wikipedia
  * spec:
  *   hosts:
  *   - wikipedia.org
  *   location: MESH_EXTERNAL
  *   ports:
  *   - number: 80
  *     name: example-http
  *     protocol: HTTP
  *   resolution: DNS
  *
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: my-wiki-rule
  * spec:
  *   hosts:
  *   - wikipedia.org
  *   http:
  *   - timeout: 5s
  *     route:
  *     - destination:
  *         host: wikipedia.org
  * ```
  *
  * @param host
  *   REQUIRED. The name of a service from the service registry. Service
  *   names are looked up from the platform's service registry (e.g.,
  *   Kubernetes services, Consul services, etc.) and from the hosts
  *   declared by [ServiceEntry](#ServiceEntry). Traffic forwarded to
  *   destinations that are not found in either of the two, will be dropped.
  *  
  *   *Note for Kubernetes users*: When short names are used (e.g. "reviews"
  *   instead of "reviews.default.svc.cluster.local"), Istio will interpret
  *   the short name based on the namespace of the rule, not the service. A
  *   rule in the "default" namespace containing a host "reviews will be
  *   interpreted as "reviews.default.svc.cluster.local", irrespective of
  *   the actual namespace associated with the reviews service. _To avoid
  *   potential misconfigurations, it is recommended to always use fully
  *   qualified domain names over short names._
  * @param subset
  *   The name of a subset within the service. Applicable only to services
  *   within the mesh. The subset must be defined in a corresponding
  *   DestinationRule.
  * @param port
  *   Specifies the port on the host that is being addressed. If a service
  *   exposes only a single port it is not required to explicitly select the
  *   port.
  */
case class Destination(
  host: String = "",
  subset: String = "",
  port: Option[PortSelector] = None
)