package services.kubernetes.istio.networking.v1alpha3.virtual_service

/** PortSelector specifies the number of a port to be used for
  * matching or selection for final routing.
  */
case class PortSelector(
  number: Option[Int] = None,    // one of these two fields must be set
  name: Option[String] = None
)

object PortSelector {

  object Port extends Enumeration {
    type Port = Value
    val Empty, Number, Name = Value
  }
}
