package services.kubernetes.istio.networking.v1alpha3.sidecar

/** WorkloadSelector specifies the criteria used to determine if the Gateway
  * or Sidecar resource can be applied to a proxy. The matching criteria
  * includes the metadata associated with a proxy, workload info such as
  * labels attached to the pod/VM, or any other info that the proxy provides
  * to Istio during the initial handshake. If multiple conditions are
  * specified, all conditions need to match in order for the workload to be
  * selected. Currently, only label based selection mechanism is supported.
  *
  * @param labels
  *   REQUIRED: One or more labels that indicate a specific set of pods/VMs
  *   on which this sidecar configuration should be applied. The scope of
  *   label search is restricted to the configuration namespace in which the
  *   the resource is present.
  */
case class WorkloadSelector(
    labels: Map[String, String] = Map.empty
)
