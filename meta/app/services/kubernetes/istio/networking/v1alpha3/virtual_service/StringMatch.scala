package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Describes how to match a given string in HTTP headers. Match is
  * case-sensitive.
  */
case class StringMatch(
  exact: Option[String],    // one of these three fields must be set
  prefix: Option[String],
  regex: Option[String]
)