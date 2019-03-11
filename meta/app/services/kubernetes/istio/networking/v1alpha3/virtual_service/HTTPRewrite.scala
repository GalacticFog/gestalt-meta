package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** HTTPRewrite can be used to rewrite specific parts of a HTTP request
  * before forwarding the request to the destination. Rewrite primitive can
  * be used only with HTTPRouteDestination. The following example
  * demonstrates how to rewrite the URL prefix for api call (/ratings) to
  * ratings service before making the actual API call.
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
  *         prefix: /ratings
  *     rewrite:
  *       uri: /v1/bookRatings
  *     route:
  *     - destination:
  *         host: ratings.prod.svc.cluster.local
  *         subset: v1
  * ```
  *
  * @param uri
  *   rewrite the path (or the prefix) portion of the URI with this
  *   value. If the original URI was matched based on prefix, the value
  *   provided in this field will replace the corresponding matched prefix.
  * @param authority
  *   rewrite the Authority/Host header with this value.
  */
case class HTTPRewrite(
  uri: String = "",
  authority: String = ""
)
