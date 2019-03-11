package services.kubernetes.istio.json.networking.v1alpha3.service_entry

import skuber._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import skuber.json.format._
import services.kubernetes.istio.networking.v1alpha3.service_entry._
import services.kubernetes.istio.networking.v1alpha3.gateway

package object format {
  import services.kubernetes.istio.json.networking.v1alpha3.gateway.format._

  implicit val formatServiceEntryEndpoint: Format[ServiceEntry.Endpoint] = (
    (JsPath \ "address").formatMaybeEmptyString() and
    (JsPath \ "ports").formatMaybeEmptyMap[Int] and
    (JsPath \ "labels").formatMaybeEmptyMap[String] and
    (JsPath \ "network").formatMaybeEmptyString() and
    (JsPath \ "locality").formatMaybeEmptyString() and
    (JsPath \ "weight").formatMaybeEmptyInt()
  )(ServiceEntry.Endpoint.apply _, unlift(ServiceEntry.Endpoint.unapply))
  
  implicit val formatServiceEntrySpec: Format[ServiceEntry.Spec] = (
    (JsPath \ "hosts").formatMaybeEmptyList[String] and
    (JsPath \ "addresses").formatMaybeEmptyList[String] and
    (JsPath \ "ports").formatMaybeEmptyList[gateway.Port] and
    (JsPath \ "location").formatEnum(ServiceEntry.Location, Some(ServiceEntry.Location.MESH_EXTERNAL)) and
    (JsPath \ "resolution").formatEnum(ServiceEntry.Resolution, Some(ServiceEntry.Resolution.NONE)) and
    (JsPath \ "endpoints").formatMaybeEmptyList[ServiceEntry.Endpoint] and
    (JsPath \ "exportTo").formatMaybeEmptyList[String] and
    (JsPath \ "subjectAltNames").formatMaybeEmptyList[String]
  )(ServiceEntry.Spec.apply _, unlift(ServiceEntry.Spec.unapply))

  implicit lazy val formatServiceEntry: Format[ServiceEntry] = (
    objFormat and
    (JsPath \ "spec").formatNullable[ServiceEntry.Spec]
  )(ServiceEntry.apply _, unlift(ServiceEntry.unapply))

  implicit val formatServiceEntryList: Format[ListResource[ServiceEntry]] = ListResourceFormat[ServiceEntry]
}