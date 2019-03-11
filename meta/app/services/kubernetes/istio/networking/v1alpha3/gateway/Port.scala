package services.kubernetes.istio.networking.v1alpha3.gateway

/** Port describes the properties of a specific port of a service.
  *
  * @param number
  *   REQUIRED: A valid non-negative integer port number.
  * @param protocol
  *   REQUIRED: The protocol exposed on the port.
  *   MUST BE one of HTTP|HTTPS|GRPC|HTTP2|MONGO|TCP|TLS.
  *   TLS implies the connection will be routed based on the SNI header to
  *   the destination without terminating the TLS connection.
  * @param name
  *   Label assigned to the port.
  */
case class Port(
  number: Int = 0,
  protocol: String = "",
  name: String = ""
)