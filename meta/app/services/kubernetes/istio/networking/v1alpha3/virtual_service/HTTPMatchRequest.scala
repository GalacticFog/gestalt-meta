package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** HttpMatchRequest specifies a set of criterion to be met in order for the
  * rule to be applied to the HTTP request. For example, the following
  * restricts the rule to match only requests where the URL path
  * starts with /ratings/v2/ and the request contains a custom `end-user` header
  * with value `jason`.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: VirtualService
  * metadata:
  *   name: ratings-route
  * spec:
  *   hosts:
  *   - ratings.prod.svc.cluster.local
  *   http:
  *   - match:
  *     - headers:
  *         end-user:
  *           exact: jason
  *       uri:
  *         prefix: "/ratings/v2/"
  *     route:
  *     - destination:
  *         host: ratings.prod.svc.cluster.local
  * ```
  *
  * HTTPMatchRequest CANNOT be empty.
  *
  * @param uri
  *   URI to match
  *   values are case-sensitive and formatted as follows:
  *  
  *   - `exact: "value"` for exact string match
  *  
  *   - `prefix: "value"` for prefix-based match
  *  
  *   - `regex: "value"` for ECMAscript style regex-based match
  * @param scheme
  *   URI Scheme
  *   values are case-sensitive and formatted as follows:
  *  
  *   - `exact: "value"` for exact string match
  *  
  *   - `prefix: "value"` for prefix-based match
  *  
  *   - `regex: "value"` for ECMAscript style regex-based match
  * @param method
  *   HTTP Method
  *   values are case-sensitive and formatted as follows:
  *  
  *   - `exact: "value"` for exact string match
  *  
  *   - `prefix: "value"` for prefix-based match
  *  
  *   - `regex: "value"` for ECMAscript style regex-based match
  * @param authority
  *   HTTP Authority
  *   values are case-sensitive and formatted as follows:
  *  
  *   - `exact: "value"` for exact string match
  *  
  *   - `prefix: "value"` for prefix-based match
  *  
  *   - `regex: "value"` for ECMAscript style regex-based match
  * @param headers
  *   The header keys must be lowercase and use hyphen as the separator,
  *   e.g. _x-request-id_.
  *  
  *   Header values are case-sensitive and formatted as follows:
  *  
  *   - `exact: "value"` for exact string match
  *  
  *   - `prefix: "value"` for prefix-based match
  *  
  *   - `regex: "value"` for ECMAscript style regex-based match
  *  
  *   **Note:** The keys `uri`, `scheme`, `method`, and `authority` will be ignored.
  * @param port
  *   Specifies the ports on the host that is being addressed. Many services
  *   only expose a single port or label ports with the protocols they support,
  *   in these cases it is not required to explicitly select the port.
  * @param sourceLabels
  *   One or more labels that constrain the applicability of a rule to
  *   workloads with the given labels. If the VirtualService has a list of
  *   gateways specified at the top, it should include the reserved gateway
  *   `mesh` in order for this field to be applicable.
  * @param gateways
  *   Names of gateways where the rule should be applied to. Gateway names
  *   at the top of the VirtualService (if any) are overridden. The gateway match is
  *   independent of sourceLabels.
  */
case class HTTPMatchRequest(
  uri: Option[StringMatch] = None,
  scheme: Option[StringMatch] = None,
  method: Option[StringMatch] = None,
  authority: Option[StringMatch] = None,
  headers: Map[String, StringMatch] = Map.empty,
  port: Int = 0,
  sourceLabels: Map[String, String] = Map.empty,
  gateways: List[String] = Nil
)