package services.kubernetes.istio.networking.v1alpha3.destination_rule

/** SSL/TLS related settings for upstream connections. See Envoy's [TLS
  * context](https://www.envoyproxy.io/docs/envoy/latest/api-v2/api/v2/auth/cert.proto.html)
  * for more details. These settings are common to both HTTP and TCP upstreams.
  *
  * For example, the following rule configures a client to use mutual TLS
  * for connections to upstream database cluster.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: db-mtls
  * spec:
  *   host: mydbserver.prod.svc.cluster.local
  *   trafficPolicy:
  *     tls:
  *       mode: MUTUAL
  *       clientCertificate: /etc/certs/myclientcert.pem
  *       privateKey: /etc/certs/client_private_key.pem
  *       caCertificates: /etc/certs/rootcacerts.pem
  * ```
  *
  * The following rule configures a client to use TLS when talking to a
  * foreign service whose domain matches *.foo.com.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: tls-foo
  * spec:
  *   host: "*.foo.com"
  *   trafficPolicy:
  *     tls:
  *       mode: SIMPLE
  * ```
  *
  * The following rule configures a client to use Istio mutual TLS when talking
  * to rating services.
  *
  * ```yaml
  * apiVersion: networking.istio.io/v1alpha3
  * kind: DestinationRule
  * metadata:
  *   name: ratings-istio-mtls
  * spec:
  *   host: ratings.prod.svc.cluster.local
  *   trafficPolicy:
  *     tls:
  *       mode: ISTIO_MUTUAL
  * ```
  *
  * @param mode
  *   REQUIRED: Indicates whether connections to this port should be secured
  *   using TLS. The value of this field determines how TLS is enforced.
  * @param clientCertificate
  *   REQUIRED if mode is `MUTUAL`. The path to the file holding the
  *   client-side TLS certificate to use.
  *   Should be empty if mode is `ISTIO_MUTUAL`.
  * @param privateKey
  *   REQUIRED if mode is `MUTUAL`. The path to the file holding the
  *   client's private key.
  *   Should be empty if mode is `ISTIO_MUTUAL`.
  * @param caCertificates
  *   OPTIONAL: The path to the file containing certificate authority
  *   certificates to use in verifying a presented server certificate. If
  *   omitted, the proxy will not verify the server's certificate.
  *   Should be empty if mode is `ISTIO_MUTUAL`.
  * @param subjectAltNames
  *   A list of alternate names to verify the subject identity in the
  *   certificate. If specified, the proxy will verify that the server
  *   certificate's subject alt name matches one of the specified values.
  *   If specified, this list overrides the value of subject_alt_names
  *   from the ServiceEntry.
  * @param sni
  *   SNI string to present to the server during TLS handshake.
  */
case class TLSSettings(
  mode: TLSSettings.TLSmode.TLSmode = TLSSettings.TLSmode.DISABLE,
  clientCertificate: String = "",
  privateKey: String = "",
  caCertificates: String = "",
  subjectAltNames: List[String] = Nil,
  sni: String = ""
)

object TLSSettings {
  object TLSmode extends Enumeration {
    type TLSmode = Value
    val DISABLE, SIMPLE, MUTUAL, ISTIO_MUTUAL = Value
  }
}
