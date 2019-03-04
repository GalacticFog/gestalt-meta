package services.kubernetes.istio.json.networking.v1alpha3.destination_rule

import skuber._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import skuber.json.format._
import services.kubernetes.istio.networking.v1alpha3.destination_rule._

package object format {
  import services.kubernetes.istio.json.networking.v1alpha3.virtual_service.format._

  implicit val formatConnectionPoolSettingsHTTPSettings = Json.format[ConnectionPoolSettings.HTTPSettings]
  implicit val formatConnectionPoolSettingsTCPSettingsTcpKeepalive = Json.format[ConnectionPoolSettings.TCPSettings.TcpKeepalive]
  implicit val formatConnectionPoolSettingsTCPSettings = Json.format[ConnectionPoolSettings.TCPSettings]
  implicit val formatConnectionPoolSettings = Json.format[ConnectionPoolSettings]
  implicit val formatLoadBalancerSettingsConsistentHashLBHTTPCookie = Json.format[LoadBalancerSettings.ConsistentHashLB.HTTPCookie]
  implicit val formatLoadBalancerSettingsConsistentHashLB = Json.format[LoadBalancerSettings.ConsistentHashLB]
  
  implicit val formatLoadBalancerSettings: Format[LoadBalancerSettings] = (
    (JsPath \ "simple").formatNullableEnum(LoadBalancerSettings.SimpleLB) and
    (JsPath \ "consistentHash").formatNullable[LoadBalancerSettings.ConsistentHashLB]
  )(LoadBalancerSettings.apply _, unlift(LoadBalancerSettings.unapply))
  
  implicit val formatOutlierDetection = Json.format[OutlierDetection]

  implicit val deleteOptionsFmt: Format[TLSSettings] = (
    (JsPath \ "mode").formatEnum(TLSSettings.TLSmode, Some(TLSSettings.TLSmode.DISABLE)) and
    (JsPath \ "clientCertificate").formatMaybeEmptyString() and
    (JsPath \ "privateKey").formatMaybeEmptyString() and
    (JsPath \ "caCertificates").formatMaybeEmptyString() and
    (JsPath \ "subjectAltNames").formatMaybeEmptyList[String] and
    (JsPath \ "sni").formatMaybeEmptyString()
  )(TLSSettings.apply _, unlift(TLSSettings.unapply))

  implicit val formatTrafficPolicyPortTrafficPolicy = Json.format[TrafficPolicy.PortTrafficPolicy]
  
  implicit val formatTrafficPolicy: Format[TrafficPolicy] = (
    (JsPath \ "loadBalancer").formatNullable[LoadBalancerSettings] and
    (JsPath \ "connectionPool").formatNullable[ConnectionPoolSettings] and
    (JsPath \ "outlierDetection").formatNullable[OutlierDetection] and
    (JsPath \ "tls").formatNullable[TLSSettings] and
    (JsPath \ "portLevelSettings").formatMaybeEmptyList[TrafficPolicy.PortTrafficPolicy]
  )(TrafficPolicy.apply _, unlift(TrafficPolicy.unapply))
  
  implicit val formatSubset: Format[Subset] = (
    (JsPath \ "name").format[String] and
    (JsPath \ "labels").formatMaybeEmptyMap[String] and
    (JsPath \ "trafficPolicy").formatNullable[TrafficPolicy]
  )(Subset.apply _, unlift(Subset.unapply))
  
  implicit val formatDestinationRuleSpec: Format[DestinationRule.Spec] = (
    (JsPath \ "host").format[String] and
    (JsPath \ "trafficPolicy").formatNullable[TrafficPolicy] and
    (JsPath \ "subsets").formatMaybeEmptyList[Subset] and
    (JsPath \ "exportTo").formatMaybeEmptyList[String]
  )(DestinationRule.Spec.apply _, unlift(DestinationRule.Spec.unapply))

  implicit lazy val destinationRuleFormat: Format[DestinationRule] = (
    objFormat and
    (JsPath \ "spec").formatNullable[DestinationRule.Spec]
  )(DestinationRule.apply _, unlift(DestinationRule.unapply))

  implicit val destinationRuleListFormat: Format[ListResource[DestinationRule]] = ListResourceFormat[DestinationRule]

}
