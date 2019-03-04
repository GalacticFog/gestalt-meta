package services.kubernetes.istio.networking.v1alpha3.sidecar

object CaptureMode extends Enumeration {
  type CaptureMode = Value
  val DEFAULT, IPTABLES, NONE = Value
}