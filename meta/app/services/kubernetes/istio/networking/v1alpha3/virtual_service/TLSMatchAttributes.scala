package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** TLS connection match attributes.
  *
  * @param sniHosts
  *   REQUIRED. SNI (server name indicator) to match on. Wildcard prefixes
  *   can be used in the SNI value, e.g., *.com will match foo.example.com
  *   as well as example.com. An SNI value must be a subset (i.e., fall
  *   within the domain) of the corresponding virtual serivce's hosts.
  * @param destinationSubnets
  *   IPv4 or IPv6 ip addresses of destination with optional subnet.  E.g.,
  *   a.b.c.d/xx form or just a.b.c.d.
  * @param port
  *   Specifies the port on the host that is being addressed. Many services
  *   only expose a single port or label ports with the protocols they
  *   support, in these cases it is not required to explicitly select the
  *   port.
  * @param sourceSubnet
  *   IPv4 or IPv6 ip address of source with optional subnet. E.g., a.b.c.d/xx
  *   form or just a.b.c.d
  * @param sourceLabels
  *   One or more labels that constrain the applicability of a rule to
  *   workloads with the given labels. If the VirtualService has a list of
  *   gateways specified at the top, it should include the reserved gateway
  *   `mesh` in order for this field to be applicable.
  * @param gateways
  *   Names of gateways where the rule should be applied to. Gateway names
  *   at the top of the VirtualService (if any) are overridden. The gateway
  *   match is independent of sourceLabels.
  */
case class TLSMatchAttributes(
  sniHosts: List[String] = Nil,
  destinationSubnets: List[String] = Nil,
  port: Int = 0,
  sourceSubnet: String = "",
  sourceLabels: Map[String, String] = Map.empty,
  gateways: List[String] = Nil
)