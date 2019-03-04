package services.kubernetes.istio.json.networking.v1alpha3.sidecar

import skuber._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import skuber.json.format._
import services.kubernetes.istio.networking.v1alpha3.gateway
import services.kubernetes.istio.networking.v1alpha3.sidecar._

package object format {
  import services.kubernetes.istio.json.networking.v1alpha3.gateway.format._

  implicit val formatWorkloadSelector: Format[WorkloadSelector] = (
    (JsPath \ "labels").formatMaybeEmptyMap[String].inmap(u => WorkloadSelector(u), p => p.labels)
  )

  implicit val formatIstioEgressListener: Format[IstioEgressListener] = (
    (JsPath \ "port").formatNullable[gateway.Port] and
    (JsPath \ "bind").formatMaybeEmptyString() and
    (JsPath \ "captureMode").formatEnum(CaptureMode, Some(CaptureMode.DEFAULT)) and
    (JsPath \ "hosts").formatMaybeEmptyList[String]
  )(IstioEgressListener.apply _, unlift(IstioEgressListener.unapply))

  implicit val formatIstioIngressListener: Format[IstioIngressListener] = (
    (JsPath \ "port").formatNullable[gateway.Port] and
    (JsPath \ "bind").formatMaybeEmptyString() and
    (JsPath \ "captureMode").formatEnum(CaptureMode, Some(CaptureMode.DEFAULT)) and
    (JsPath \ "defaultEndpoint").formatMaybeEmptyString()
  )(IstioIngressListener.apply _, unlift(IstioIngressListener.unapply))

  implicit val formatSidecarSpec: Format[Sidecar.Spec] = (
    (JsPath \ "workloadSelector").formatNullable[WorkloadSelector] and
    (JsPath \ "ingress").formatMaybeEmptyList[IstioIngressListener] and
    (JsPath \ "egress").formatMaybeEmptyList[IstioEgressListener]
  )(Sidecar.Spec.apply _, unlift(Sidecar.Spec.unapply))

  implicit lazy val formatSidecar: Format[Sidecar] = (
    objFormat and
    (JsPath \ "spec").formatNullable[Sidecar.Spec]
  )(Sidecar.apply _, unlift(Sidecar.unapply))

  implicit val formatServiceEntryList: Format[ListResource[Sidecar]] = ListResourceFormat[Sidecar]
}