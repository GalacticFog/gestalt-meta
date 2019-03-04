package services.kubernetes.istio.networking.v1alpha3.destination_rule

import skuber.ResourceSpecification.{Names, Scope}
import skuber._

/** `DestinationRule` defines policies that apply to traffic intended for a
  * service after routing has occurred. These rules specify configuration
  * for load balancing, connection pool size from the sidecar, and outlier
  * detection settings to detect and evict unhealthy hosts from the load
  * balancing pool. For example, a simple load balancing policy for the
  * ratings service would look as follows:
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
  *       simple: LEAST_CONN
  * ```
  *
  * Version specific policies can be specified by defining a named
  * `subset` and overriding the settings specified at the service level. The
  * following rule uses a round robin load balancing policy for all traffic
  * going to a subset named testversion that is composed of endpoints (e.g.,
  * pods) with labels (version:v3).
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
  *       simple: LEAST_CONN
  *   subsets:
  *   - name: testversion
  *     labels:
  *       version: v3
  *     trafficPolicy:
  *       loadBalancer:
  *         simple: ROUND_ROBIN
  * ```
  *
  * **Note:** Policies specified for subsets will not take effect until
  * a route rule explicitly sends traffic to this subset.
  *
  * Traffic policies can be customized to specific ports as well. The
  * following rule uses the least connection load balancing policy for all
  * traffic to port 80, while uses a round robin load balancing setting for
  * traffic to the port 9080.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: bookinfo-ratings-port
  * spec:
  *   host: ratings.prod.svc.cluster.local
  *   trafficPolicy: # Apply to all ports
  *     portLevelSettings:
  *     - port:
  *         number: 80
  *       loadBalancer:
  *         simple: LEAST_CONN
  *     - port:
  *         number: 9080
  *       loadBalancer:
  *         simple: ROUND_ROBIN
  * ```
  *
  * @param host
  *   REQUIRED. The name of a service from the service registry. Service
  *   names are looked up from the platform's service registry (e.g.,
  *   Kubernetes services, Consul services, etc.) and from the hosts
  *   declared by [ServiceEntries](#ServiceEntry). Rules defined for
  *   services that do not exist in the service registry will be ignored.
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
  *   Note that the host field applies to both HTTP and TCP services.
  * @param trafficPolicy
  *   Traffic policies to apply (load balancing policy, connection pool
  *   sizes, outlier detection).
  * @param subsets
  *   One or more named sets that represent individual versions of a
  *   service. Traffic policies can be overridden at subset level.
  * @param exportTo
  *   The resolution of a DestinationRule to apply to a service occurs in the
  *   context of a hierarchy of namespaces. This rule controls whether those
  *   namespaces are allowed to select this rule.
  */
case class DestinationRule(
  val kind: String = "DestinationRule",
  override val apiVersion: String = "networking.istio.io/v1alpha3",
  val metadata: ObjectMeta = ObjectMeta(),
  spec: Option[DestinationRule.Spec] = None
) extends ObjectResource

object DestinationRule {
  case class Spec(
    host: String = "",
    trafficPolicy: Option[TrafficPolicy] = None,
    subsets: List[Subset] = Nil,
    exportTo: List[String] = Nil
  )

  val specification=NonCoreResourceSpecification(
    apiGroup = "networking.istio.io",
    version = "v1alpha3",
    scope = Scope.Namespaced,
    names = Names(
      plural = "destinationrules",
      singular = "destinationrule",
      kind = "DestinationRule",
      shortNames = List()
    )
  )
  implicit val destinationRuleFilterDef = new ResourceDefinition[DestinationRule] { def spec=specification }
  implicit val destinationRuleFilterListDef = new ResourceDefinition[ListResource[DestinationRule]] { def spec=specification }
}