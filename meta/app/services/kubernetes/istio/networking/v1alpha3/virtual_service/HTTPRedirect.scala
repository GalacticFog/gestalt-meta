package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** HTTPRedirect can be used to send a 301 redirect response to the caller,
  * where the Authority/Host and the URI in the response can be swapped with
  * the specified values. For example, the following rule redirects
  * requests for /v1/getProductRatings API on the ratings service to
  * /v1/bookRatings provided by the bookratings service.
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
  *     - uri:
  *         exact: /v1/getProductRatings
  *   redirect:
  *     uri: /v1/bookRatings
  *     authority: newratings.default.svc.cluster.local
  *   ...
  * ```
  *
  * @param uri
  *   On a redirect, overwrite the Path portion of the URL with this
  *   value. Note that the entire path will be replaced, irrespective of the
  *   request URI being matched as an exact path or prefix.
  * @param authority
  *   On a redirect, overwrite the Authority/Host portion of the URL with
  *   this value.
  */
case class HTTPRedirect(
  uri: String = "",
  authority: String = ""
)
