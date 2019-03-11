package services.kubernetes.istio.networking.v1alpha3.destination_rule

/** A subset of endpoints of a service. Subsets can be used for scenarios
  * like A/B testing, or routing to a specific version of a service. Refer
  * to [VirtualService](#VirtualService) documentation for examples of using
  * subsets in these scenarios. In addition, traffic policies defined at the
  * service-level can be overridden at a subset-level. The following rule
  * uses a round robin load balancing policy for all traffic going to a
  * subset named testversion that is composed of endpoints (e.g., pods) with
  * labels (version:v3).
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
  * One or more labels are typically required to identify the subset destination,
  * however, when the corresponding DestinationRule represents a host that
  * supports multiple SNI hosts (e.g., an egress gateway), a subset without labels
  * may be meaningful. In this case a traffic policy with [TLSSettings](#TLSSettings)
  * can be used to identify a specific SNI host corresponding to the named subset.
  *
  * @param name
  *   REQUIRED. Name of the subset. The service name and the subset name can
  *   be used for traffic splitting in a route rule.
  * @param labels
  *   Labels apply a filter over the endpoints of a service in the
  *   service registry. See route rules for examples of usage.
  * @param trafficPolicy
  *   Traffic policies that apply to this subset. Subsets inherit the
  *   traffic policies specified at the DestinationRule level. Settings
  *   specified at the subset level will override the corresponding settings
  *   specified at the DestinationRule level.
  */
case class Subset(
  name: String = "",
  labels: Map[String, String] = Map.empty,
  trafficPolicy: Option[TrafficPolicy] = None
)

object Subset {
  case class LabelsEntry(
    key: String = "",
    value: String = ""
  )
}
