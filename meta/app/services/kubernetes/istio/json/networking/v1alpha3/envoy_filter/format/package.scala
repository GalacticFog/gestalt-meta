package services.kubernetes.istio.json.networking.v1alpha3.envoy_filter

import skuber._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import skuber.json.format._
import services.kubernetes.istio.networking.v1alpha3.envoy_filter._

package object format {
  implicit val formatEnvoyFilterInsertPosition: Format[EnvoyFilter.InsertPosition] = (
    (JsPath \ "index").formatEnum(EnvoyFilter.InsertPosition.Index, Some(EnvoyFilter.InsertPosition.Index.FIRST)) and
    (JsPath \ "relativeTo").formatMaybeEmptyString()
  )(EnvoyFilter.InsertPosition.apply _, unlift(EnvoyFilter.InsertPosition.unapply))
  
  implicit val formatEnvoyFilterListenerMatch: Format[EnvoyFilter.ListenerMatch] = (
    (JsPath \ "portNumber").formatMaybeEmptyInt() and
    (JsPath \ "portNamePrefix").formatMaybeEmptyString() and
    (JsPath \ "listenerType").formatEnum(EnvoyFilter.ListenerMatch.ListenerType, Some(EnvoyFilter.ListenerMatch.ListenerType.ANY)) and
    (JsPath \ "listenerProtocol").formatEnum(EnvoyFilter.ListenerMatch.ListenerProtocol, Some(EnvoyFilter.ListenerMatch.ListenerProtocol.ALL)) and
    (JsPath \ "address").formatMaybeEmptyList[String]
  )(EnvoyFilter.ListenerMatch.apply _, unlift(EnvoyFilter.ListenerMatch.unapply))

  implicit val formatEnvoyFilterFilter: Format[EnvoyFilter.Filter] = (
    (JsPath \ "listenerMatch").formatNullable[EnvoyFilter.ListenerMatch] and
    (JsPath \ "insertPosition").formatNullableEnum(EnvoyFilter.InsertPosition.Index) and
    (JsPath \ "filterType").formatEnum(EnvoyFilter.Filter.FilterType, Some(EnvoyFilter.Filter.FilterType.INVALID)) and
    (JsPath \ "filterName").formatMaybeEmptyString() and
    (JsPath \ "filterConfig").formatNullable[JsValue]
  )(EnvoyFilter.Filter.apply _, unlift(EnvoyFilter.Filter.unapply))
  
  implicit val formatEnvoyFilterSpec: Format[EnvoyFilter.Spec] = (
    (JsPath \ "workloadLabels").formatMaybeEmptyMap[String] and
    (JsPath \ "filters").formatMaybeEmptyList[EnvoyFilter.Filter]
  )(EnvoyFilter.Spec.apply _, unlift(EnvoyFilter.Spec.unapply))

  implicit lazy val formatEnvoyFilter: Format[EnvoyFilter] = (
    objFormat and
    (JsPath \ "spec").formatNullable[EnvoyFilter.Spec]
  )(EnvoyFilter.apply _, unlift(EnvoyFilter.unapply))

  implicit val formatEnvoyFilterList: Format[ListResource[EnvoyFilter]] = ListResourceFormat[EnvoyFilter]
}