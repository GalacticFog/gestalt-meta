package services.kubernetes.istio.networking.v1alpha3.sidecar

import skuber.ResourceSpecification.{Names, Scope}
import skuber._

/** `Sidecar` describes the configuration of the sidecar proxy that mediates
  * inbound and outbound communication to the workload it is attached to. By
  * default, Istio will program all sidecar proxies in the mesh with the
  * necessary configuration required to reach every workload in the mesh, as
  * well as accept traffic on all the ports associated with the
  * workload. The Sidecar resource provides a way to fine tune the set of
  * ports, protocols that the proxy will accept when forwarding traffic to
  * and from the workload. In addition, it is possible to restrict the set
  * of services that the proxy can reach when forwarding outbound traffic
  * from the workload.
  *
  * Services and configuration in a mesh are organized into one or more
  * namespaces (e.g., a Kubernetes namespace or a CF org/space). A Sidecar
  * resource in a namespace will apply to one or more workloads in the same
  * namespace, selected using the workloadSelector. In the absence of a
  * workloadSelector, it will apply to all workloads in the same
  * namespace. When determining the Sidecar resource to be applied to a
  * workload, preference will be given to the resource with a
  * workloadSelector that selects this workload, over a Sidecar resource
  * without any workloadSelector.
  *
  * NOTE: *_Each namespace can have only one Sidecar resource without any
  * workload selector_*. The behavior of the system is undefined if more
  * than one selector-less Sidecar resources exist in a given namespace. The
  * behavior of the system is undefined if two or more Sidecar resources
  * with a workload selector select the same workload.
  *
  * The example below declares a Sidecar resource in the prod-us1 namespace
  * that configures the sidecars in the namespace to allow egress traffic to
  * public services in the prod-us1, prod-apis, and the istio-system
  * namespaces.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: Sidecar
  * metadata:
  *   name: default
  *   namespace: prod-us1
  * spec:
  *   egress:
  *   - hosts:
  *     - "prod-us1/&#42;"
  *     - "prod-apis/&#42;"
  *     - "istio-system/&#42;"
  * ```
  *
  * The example below declares a Sidecar resource in the prod-us1 namespace
  * that accepts inbound HTTP traffic on port 9080 and forwards
  * it to the attached workload listening on a unix domain socket. In the
  * egress direction, in addition to the istio-system namespace, the sidecar
  * proxies only HTTP traffic bound for port 9080 for services in the
  * prod-us1 namespace.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: Sidecar
  * metadata:
  *   name: default
  *   namespace: prod-us1
  * spec:
  *   ingress:
  *   - port:
  *       number: 9080
  *       protocol: HTTP
  *       name: somename
  *     defaultEndpoint: unix:///var/run/someuds.sock
  *   egress:
  *   - hosts:
  *     - "istio-system/&#42;"
  *   - port:
  *       number: 9080
  *       protocol: HTTP
  *       name: egresshttp
  *     hosts:
  *     - "prod-us1/&#42;"
  * ```
  *
  * @param workloadSelector
  *   Criteria used to select the specific set of pods/VMs on which this
  *   sidecar configuration should be applied. If omitted, the sidecar
  *   configuration will be applied to all workloads in the same config
  *   namespace.
  * @param ingress
  *   Ingress specifies the configuration of the sidecar for processing
  *   inbound traffic to the attached workload. If omitted, Istio will
  *   autoconfigure the sidecar based on the information about the workload
  *   obtained from the orchestration platform (e.g., exposed ports, services,
  *   etc.).
  * @param egress
  *   Egress specifies the configuration of the sidecar for processing
  *   outbound traffic from the attached workload to other services in the
  *   mesh. If omitted, Istio will autoconfigure the sidecar to be able to
  *   reach every service in the mesh that is visible to this namespace.
  */
case class Sidecar(
  val kind: String = "Sidecar",
  override val apiVersion: String = "networking.istio.io/v1alpha3",
  val metadata: ObjectMeta = ObjectMeta(),
  spec: Option[Sidecar.Spec] = None
) extends ObjectResource

object Sidecar {
  case class Spec(
    workloadSelector: Option[WorkloadSelector] = None,
    ingress: List[IstioIngressListener] = Nil,
    egress: List[IstioEgressListener] = Nil
  )

  val specification=NonCoreResourceSpecification(
    apiGroup = "networking.istio.io",
    version = "v1alpha3",
    scope = Scope.Namespaced,
    names = Names(
      plural = "sidecars",
      singular = "sidecar",
      kind = "Sidecar",
      shortNames = List()
    )
  )
  implicit val sidecarDef = new ResourceDefinition[Sidecar] { def spec=specification }
  implicit val sidecarListDef = new ResourceDefinition[ListResource[Sidecar]] { def spec=specification }
}