package services.kubernetes

import play.api.test.PlaySpecification
import play.api.libs.json._
import org.specs2.specification.{BeforeAfterEach, BeforeAll}
import com.galacticfog.gestalt.meta.test.ResourceScope

import services.kubernetes.istio.networking.v1alpha3.destination_rule.DestinationRule
import services.kubernetes.istio.networking.v1alpha3.envoy_filter.EnvoyFilter
import services.kubernetes.istio.networking.v1alpha3.gateway.Gateway
import services.kubernetes.istio.networking.v1alpha3.service_entry.ServiceEntry
import services.kubernetes.istio.networking.v1alpha3.sidecar.Sidecar
import services.kubernetes.istio.networking.v1alpha3.virtual_service.VirtualService

class IstioFormatSpec extends PlaySpecification with ResourceScope with BeforeAll with BeforeAfterEach {
  override def beforeAll(): Unit = () //pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential

  "IstioFormatSpec" should {
    "DestinationRule" in {
      import services.kubernetes.istio.json.networking.v1alpha3.destination_rule.format._

      val s = Json.parse("""{
         "apiVersion": "networking.istio.io/v1alpha3",
         "kind": "DestinationRule",
         "metadata": {
            "name": "bookinfo-ratings"
         },
         "spec": {
            "host": "ratings.prod.svc.cluster.local",
            "trafficPolicy": {
               "loadBalancer": {
                  "simple": "LEAST_CONN"
               }
            },
            "subsets": [
               {
                  "name": "testversion",
                  "labels": {
                     "version": "v3"
                  },
                  "trafficPolicy": {
                     "loadBalancer": {
                        "simple": "ROUND_ROBIN"
                     }
                  }
               }
            ]
         }
      }""")
      val r = Json.fromJson[DestinationRule](s).get
      Json.toJson(r) must_== s
    }

    "EnvoyFilter" in {
      import services.kubernetes.istio.json.networking.v1alpha3.envoy_filter.format._
      
      val s = Json.parse("""{
         "apiVersion": "networking.istio.io/v1alpha3",
         "kind": "EnvoyFilter",
         "metadata": {
            "name": "reviews-lua"
         },
         "spec": {
            "workloadLabels": {
               "app": "reviews"
            },
            "filters": [
               {
                  "listenerMatch": {
                     "portNumber": 8080,
                     "listenerType": "SIDECAR_INBOUND",
                     "listenerProtocol": "ALL"
                  },
                  "filterName": "envoy.lua",
                  "filterType": "HTTP",
                  "filterConfig": {
                     "inlineCode": "... lua code\n"
                  }
               }
            ]
         }
      }""")
      val r = Json.fromJson[EnvoyFilter](s).get
      Json.toJson(r) must_== s
    }

    "Gateway" in {
      import services.kubernetes.istio.json.networking.v1alpha3.gateway.format._
      
      val s = Json.parse("""{
         "apiVersion": "networking.istio.io/v1alpha3",
         "kind": "Gateway",
         "metadata": {
            "name": "my-gateway",
            "namespace": "some-config-namespace"
         },
         "spec": {
            "selector": {
               "app": "my-gateway-controller"
            },
            "servers": [
               {
                  "port": {
                     "number": 80,
                     "name": "http",
                     "protocol": "HTTP"
                  },
                  "hosts": [
                     "uk.bookinfo.com",
                     "eu.bookinfo.com"
                  ],
                  "tls": {
                     "httpsRedirect": true,
                     "mode": "PASSTHROUGH",
                     "minProtocolVersion": "TLS_AUTO",
                     "maxProtocolVersion": "TLS_AUTO"
                  }
               },
               {
                  "port": {
                     "number": 443,
                     "name": "https",
                     "protocol": "HTTPS"
                  },
                  "hosts": [
                     "uk.bookinfo.com",
                     "eu.bookinfo.com"
                  ],
                  "tls": {
                    "httpsRedirect": false,
                     "mode": "SIMPLE",
                     "serverCertificate": "/etc/certs/servercert.pem",
                     "privateKey": "/etc/certs/privatekey.pem",
                     "minProtocolVersion": "TLS_AUTO",
                     "maxProtocolVersion": "TLS_AUTO"
                  }
               },
               {
                  "port": {
                     "number": 9080,
                     "name": "http-wildcard",
                     "protocol": "HTTP"
                  },
                  "hosts": [
                     "*"
                  ]
               },
               {
                  "port": {
                     "number": 2379,
                     "name": "mongo",
                     "protocol": "MONGO"
                  },
                  "hosts": [
                     "*"
                  ]
               }
            ]
         }
      }""")
      val r = Json.fromJson[Gateway](s).get
      Json.toJson(r) must_== s
    }

    "ServiceEntry" in {
      import services.kubernetes.istio.json.networking.v1alpha3.service_entry.format._
      
      val s = Json.parse("""{
         "apiVersion": "networking.istio.io/v1alpha3",
         "kind": "ServiceEntry",
         "metadata": {
            "name": "external-svc-httpbin",
            "namespace": "egress"
         },
         "spec": {
            "hosts": [
               "httpbin.com"
            ],
            "exportTo": [
               "."
            ],
            "location": "MESH_EXTERNAL",
            "ports": [
               {
                  "number": 80,
                  "name": "http",
                  "protocol": "HTTP"
               }
            ],
            "resolution": "DNS"
         }
      }""")
      val r = Json.fromJson[ServiceEntry](s).get
      Json.toJson(r) must_== s
    }

    "Sidecar" in {
      import services.kubernetes.istio.json.networking.v1alpha3.sidecar.format._
      
      val s = Json.parse("""{
         "apiVersion": "networking.istio.io/v1alpha3",
         "kind": "Sidecar",
         "metadata": {
            "name": "default",
            "namespace": "prod-us1"
         },
         "spec": {
            "ingress": [
               {
                  "captureMode":"DEFAULT",
                  "port": {
                     "number": 9080,
                     "protocol": "HTTP",
                     "name": "somename"
                  },
                  "defaultEndpoint": "unix:///var/run/someuds.sock"
               }
            ],
            "egress": [
               {
                  "captureMode":"DEFAULT",
                  "hosts": [
                     "istio-system/&#42;"
                  ]
               },
               {
                  "captureMode":"DEFAULT",
                  "port": {
                     "number": 9080,
                     "protocol": "HTTP",
                     "name": "egresshttp"
                  },
                  "hosts": [
                     "prod-us1/&#42;"
                  ]
               }
            ]
         }
      }""")
      val r = Json.fromJson[Sidecar](s).get
      Json.toJson(r) must_== s
    }

    "VirtualService" in {
      import services.kubernetes.istio.json.networking.v1alpha3.virtual_service.format._
      
      val s = Json.parse("""{
         "apiVersion": "networking.istio.io/v1alpha3",
         "kind": "VirtualService",
         "metadata": {
            "name": "reviews-route"
         },
         "spec": {
            "hosts": [
               "reviews.prod.svc.cluster.local"
            ],
            "http": [
               {
                  "match": [
                     {
                        "uri": {
                           "prefix": "/wpcatalog"
                        }
                     },
                     {
                        "uri": {
                           "prefix": "/consumercatalog"
                        }
                     }
                  ],
                  "rewrite": {
                     "uri": "/newcatalog"
                  },
                  "route": [
                     {
                        "destination": {
                           "host": "reviews.prod.svc.cluster.local",
                           "subset": "v2"
                        }
                     }
                  ],
                  "websocketUpgrade": false
               },
               {
                  "route": [
                     {
                        "destination": {
                           "host": "reviews.prod.svc.cluster.local",
                           "subset": "v1"
                        }
                     }
                  ],
                  "websocketUpgrade": false
               }
            ]
         }
      }""")
      val r = Json.fromJson[VirtualService](s).get
      Json.toJson(r) must_== s
    }
  }
}