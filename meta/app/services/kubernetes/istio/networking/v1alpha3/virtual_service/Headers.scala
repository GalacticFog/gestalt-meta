package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Header manipulation rules
  *
  * @param request
  *   Header manipulation rules to apply before forwarding a request
  *   to the destination service
  * @param response
  *   Header manipulation rules to apply before returning a response
  *   to the caller
  */
case class Headers(
  request: Option[Headers.HeaderOperations] = None,
  response: Option[Headers.HeaderOperations] = None
)

object Headers {
  /** HeaderOperations Describes the header manipulations to apply
    *
    * @param set
    *   Overwrite the headers specified by key with the given values
    * @param add
    *   Append the given values to the headers specified by keys
    *   (will create a comma-separated list of values)
    * @param remove
    *   Remove a the specified headers
    */
  case class HeaderOperations(
    set: Map[String, String] = Map.empty,
    add: Map[String, String] = Map.empty,
    remove: List[String] = Nil
  )
}
