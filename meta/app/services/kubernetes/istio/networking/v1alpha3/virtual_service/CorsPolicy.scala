package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Describes the Cross-Origin Resource Sharing (CORS) policy, for a given
  * service. Refer to
  * &lt;https://developer.mozilla.org/en-US/docs/Web/HTTP/Access_control_CORS&gt;
  * for further details about cross origin resource sharing. For example,
  * the following rule restricts cross origin requests to those originating
  * from example.com domain using HTTP POST/GET, and sets the
  * `Access-Control-Allow-Credentials` header to false. In addition, it only
  * exposes `X-Foo-bar` header and sets an expiry period of 1 day.
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
  *   - route:
  *     - destination:
  *         host: ratings.prod.svc.cluster.local
  *         subset: v1
  *     corsPolicy:
  *       allowOrigin:
  *       - example.com
  *       allowMethods:
  *       - POST
  *       - GET
  *       allowCredentials: false
  *       allowHeaders:
  *       - X-Foo-Bar
  *       maxAge: "1d"
  * ```
  *
  * @param allowOrigin
  *   The list of origins that are allowed to perform CORS requests. The
  *   content will be serialized into the Access-Control-Allow-Origin
  *   header. Wildcard * will allow all origins.
  * @param allowMethods
  *   List of HTTP methods allowed to access the resource. The content will
  *   be serialized into the Access-Control-Allow-Methods header.
  * @param allowHeaders
  *   List of HTTP headers that can be used when requesting the
  *   resource. Serialized to Access-Control-Allow-Headers header.
  * @param exposeHeaders
  *   A white list of HTTP headers that the browsers are allowed to
  *   access. Serialized into Access-Control-Expose-Headers header.
  * @param maxAge
  *   Specifies how long the results of a preflight request can be
  *   cached. Translates to the `Access-Control-Max-Age` header.
  * @param allowCredentials
  *   Indicates whether the caller is allowed to send the actual request
  *   (not the preflight) using credentials. Translates to
  *   `Access-Control-Allow-Credentials` header.
  */
case class CorsPolicy(
  allowOrigin: List[String] = Nil,
  allowMethods: List[String] = Nil,
  allowHeaders: List[String] = Nil,
  exposeHeaders: List[String] = Nil,
  maxAge: Option[services.kubernetes.istio.Duration.Duration] = None,
  allowCredentials: Option[Boolean] = None
)