package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** Percent specifies a percentage in the range of [0.0, 100.0].
  */
case class Percent(
  value: Double = 0.0
)