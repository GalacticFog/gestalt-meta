package services.kubernetes.istio.json.networking.v1alpha3.gateway

import skuber._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import skuber.json.format._
import services.kubernetes.istio.networking.v1alpha3.gateway._

package object format {
  implicit val formatPort: Format[Port] = (
    (JsPath \ "number").formatMaybeEmptyInt() and
    (JsPath \ "protocol").formatMaybeEmptyString() and
    (JsPath \ "name").formatMaybeEmptyString()
  )(Port.apply _, unlift(Port.unapply))

  implicit val formatServerTLSOptions: Format[Server.TLSOptions] = (
    (JsPath \ "httpsRedirect").formatMaybeEmptyBoolean(false) and
    (JsPath \ "mode").formatEnum(Server.TLSOptions.TLSmode, Some(Server.TLSOptions.TLSmode.PASSTHROUGH)) and
    (JsPath \ "serverCertificate").formatMaybeEmptyString() and
    (JsPath \ "privateKey").formatMaybeEmptyString() and
    (JsPath \ "caCertificates").formatMaybeEmptyString() and
    (JsPath \ "credentialName").formatMaybeEmptyString() and
    (JsPath \ "subjectAltNames").formatMaybeEmptyList[String] and
    (JsPath \ "minProtocolVersion").formatEnum(Server.TLSOptions.TLSProtocol, Some(Server.TLSOptions.TLSProtocol.TLS_AUTO)) and
    (JsPath \ "maxProtocolVersion").formatEnum(Server.TLSOptions.TLSProtocol, Some(Server.TLSOptions.TLSProtocol.TLS_AUTO)) and
    (JsPath \ "cipherSuites").formatMaybeEmptyList[String]
  )(Server.TLSOptions.apply _, unlift(Server.TLSOptions.unapply))

  implicit val formatServer: Format[Server] = (
    (JsPath \ "port").formatNullable[Port] and
    (JsPath \ "bind").formatMaybeEmptyString() and
    (JsPath \ "hosts").formatMaybeEmptyList[String] and
    (JsPath \ "tls").formatNullable[Server.TLSOptions] and
    (JsPath \ "defaultEndpoint").formatMaybeEmptyString()
  )(Server.apply _, unlift(Server.unapply))

  implicit val formatGatewaySpec: Format[Gateway.Spec] = (
    (JsPath \ "servers").formatMaybeEmptyList[Server] and
    (JsPath \ "selector").formatMaybeEmptyMap[String]
  )(Gateway.Spec.apply _, unlift(Gateway.Spec.unapply))

  implicit lazy val formatGateway: Format[Gateway] = (
    objFormat and
    (JsPath \ "spec").formatNullable[Gateway.Spec]
  )(Gateway.apply _, unlift(Gateway.unapply))

  implicit val formatGatewayList: Format[ListResource[Gateway]] = ListResourceFormat[Gateway]

}