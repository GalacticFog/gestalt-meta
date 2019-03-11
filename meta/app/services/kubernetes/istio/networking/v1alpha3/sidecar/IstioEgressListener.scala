package services.kubernetes.istio.networking.v1alpha3.sidecar

import services.kubernetes.istio.networking.v1alpha3.gateway

/** IstioEgressListener specifies the properties of an outbound traffic
  * listener on the sidecar proxy attached to a workload.
  *
  * @param port
  *   The port associated with the listener. If using unix domain socket,
  *   use 0 as the port number, with a valid protocol. The port if
  *   specified, will be used as the default destination port associated
  *   with the imported hosts. If the port is omitted, Istio will infer the
  *   listener ports based on the imported hosts. Note that when multiple
  *   egress listeners are specified, where one or more listeners have
  *   specific ports while others have no port, the hosts exposed on a
  *   listener port will be based on the listener with the most specific
  *   port.
  * @param bind
  *   The ip or the unix domain socket to which the listener should be bound
  *   to. Port MUST be specified if bind is not empty. Format: x.x.x.x or
  *   unix:///path/to/uds or unix://&#64;foobar (Linux abstract namespace). If
  *   omitted, Istio will autoconfigure the defaults based on imported
  *   services, the workload to which this configuration is applied to and
  *   the captureMode. If captureMode is NONE, bind will default to
  *   127.0.0.1.
  * @param captureMode
  *   When the bind address is an IP, the captureMode option dictates
  *   how traffic to the listener is expected to be captured (or not).
  *   captureMode must be DEFAULT or NONE for unix domain socket binds.
  * @param hosts
  *   REQUIRED: One or more services/virtualServices exposed by the listener
  *   in namespace/dnsName format.  Publicly scoped services and
  *   VirtualServices from remote namespaces corresponding to the specified
  *   hosts will be imported. The service in a namespace can be a service in
  *   the service registry (e.g., a kubernetes or cloud foundry service) or
  *   a service specified via ServiceEntry configuration. In addition, any
  *   publicly scoped DestinationRule associated with the imported services
  *   will also be imported.
  *  
  *   Set the namespace to * to import a particular service from any
  *   available namespace (e.g., "*&#47;foo.example.com"). Set the dnsName field
  *   to * to import all services from the specified namespace (e.g.,
  *   "prod/&#42;"). The services should be specified using FQDN format.
  *  
  *   NOTE: Only exported services and configuration artifacts from a
  *   namespace can be imported. Private services/configuration will not be
  *   imported. Refer to the scope setting associated with VirtualService,
  *   DestinationRule, ServiceEntry, etc. for details.
  */
case class IstioEgressListener(
  port: Option[gateway.Port] = None,
  bind: String = "",
  captureMode: CaptureMode.CaptureMode = CaptureMode.DEFAULT,
  hosts: List[String] = Nil
)