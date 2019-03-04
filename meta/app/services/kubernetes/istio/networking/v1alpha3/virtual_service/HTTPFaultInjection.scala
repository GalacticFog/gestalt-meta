package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** HTTPFaultInjection can be used to specify one or more faults to inject
  * while forwarding http requests to the destination specified in a route.
  * Fault specification is part of a VirtualService rule. Faults include
  * aborting the Http request from downstream service, and/or delaying
  * proxying of requests. A fault rule MUST HAVE delay or abort or both.
  *
  * *Note:* Delay and abort faults are independent of one another, even if
  * both are specified simultaneously.
  *
  * @param delay
  *   Delay requests before forwarding, emulating various failures such as
  *   network issues, overloaded upstream service, etc.
  * @param abort
  *   Abort Http request attempts and return error codes back to downstream
  *   service, giving the impression that the upstream service is faulty.
  */
case class HTTPFaultInjection(
  delay: Option[HTTPFaultInjection.Delay] = None,
  abort: Option[HTTPFaultInjection.Abort] = None
)

object HTTPFaultInjection {
  /** Delay specification is used to inject latency into the request
    * forwarding path. The following example will introduce a 5 second delay
    * in 1 out of every 1000 requests to the "v1" version of the "reviews"
    * service from all pods with label env: prod
    *
    * ```yaml
    * apiVersion: networking.istio.io/v1alpha3
    * kind: VirtualService
    * metadata:
    *   name: reviews-route
    * spec:
    *   hosts:
    *   - reviews.prod.svc.cluster.local
    *   http:
    *   - match:
    *     - sourceLabels:
    *         env: prod
    *     route:
    *     - destination:
    *         host: reviews.prod.svc.cluster.local
    *         subset: v1
    *     fault:
    *       delay:
    *         percentage:
    *           value: 0.001
    *         fixedDelay: 5s
    * ```
    *
    * The _fixedDelay_ field is used to indicate the amount of delay in seconds.
    * The optional _percentage_ field can be used to only delay a certain
    * percentage of requests. If left unspecified, all request will be delayed.
    *
    * @param percent
    *   Percentage of requests on which the delay will be injected (0-100).
    *   Use of integer `percent` value is deprecated. Use the double `percentage`
    *   field instead.
    * @param percentage
    *   Percentage of requests on which the delay will be injected.
    */
  case class Delay(
    percent: Int = 0,    // deprecated
    percentage: Option[Percent] = None,
    fixedDelay: Option[services.kubernetes.istio.Duration.Duration] = None,     // one of these two fields must be set
    exponentialDelay: Option[services.kubernetes.istio.Duration.Duration] = None
  )
  
  /** Abort specification is used to prematurely abort a request with a
    * pre-specified error code. The following example will return an HTTP 400
    * error code for 1 out of every 1000 requests to the "ratings" service "v1".
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
    *     fault:
    *       abort:
    *         percentage:
    *           value: 0.001
    *         httpStatus: 400
    * ```
    *
    * The _httpStatus_ field is used to indicate the HTTP status code to
    * return to the caller. The optional _percentage_ field can be used to only
    * abort a certain percentage of requests. If not specified, all requests are
    * aborted.
    *
    * @param percent
    *   Percentage of requests to be aborted with the error code provided (0-100).
    *   Use of integer `percent` value is deprecated. Use the double `percentage`
    *   field instead.
    * @param percentage
    *   Percentage of requests to be aborted with the error code provided.
    */
  case class Abort(
    percent: Int = 0,   // deprecated
    percentage: Option[Percent] = None,
    httpStatus: Option[Int] = None,     // one of these three fields must be set
    grpcStatus: Option[String] = None,
    http2Error: Option[String] = None
  )
}
