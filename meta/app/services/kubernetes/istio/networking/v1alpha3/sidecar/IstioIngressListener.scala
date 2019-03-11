package services.kubernetes.istio.networking.v1alpha3.sidecar

import services.kubernetes.istio.networking.v1alpha3.gateway

/** IstioIngressListener specifies the properties of an inbound
  * traffic listener on the sidecar proxy attached to a workload.
  *
  * @param port
  *   REQUIRED. The port associated with the listener. If using
  *   unix domain socket, use 0 as the port number, with a valid
  *   protocol.
  * @param bind
  *   The ip or the unix domain socket to which the listener should be bound
  *   to. Format: x.x.x.x or unix:///path/to/uds or unix://&#64;foobar (Linux
  *   abstract namespace). If omitted, Istio will autoconfigure the defaults
  *   based on imported services and the workload to which this
  *   configuration is applied to.
  * @param captureMode
  *   When the bind address is an IP, the captureMode option dictates
  *   how traffic to the listener is expected to be captured (or not).
  *   captureMode must be DEFAULT or NONE for unix domain socket binds.
  * @param defaultEndpoint
  *   REQUIRED: The loopback IP endpoint or unix domain socket to which
  *   traffic should be forwarded to. This configuration can be used to
  *   redirect traffic arriving at the bind point on the sidecar to a port
  *   or unix domain socket where the application workload is listening for
  *   connections. Format should be 127.0.0.1:PORT or unix:///path/to/socket
  */
case class IstioIngressListener(
  port: Option[gateway.Port] = None,
  bind: String = "",
  captureMode: CaptureMode.CaptureMode = CaptureMode.DEFAULT,
  defaultEndpoint: String = ""
)