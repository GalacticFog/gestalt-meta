package services.kubernetes

import play.api.libs.json._
import ai.x.play.json.Jsonx
import skuber._
import skuber.json.format._

object KubernetesProviderProperties {
  sealed trait RequestLimit
  case object Request extends RequestLimit
  case object Limit extends RequestLimit

  val readsRequestLimit = new Reads[RequestLimit] {
    def reads(json: JsValue): JsResult[RequestLimit] = {
      json.validate[String] flatMap { rl =>
        if(rl == "request") {
          JsSuccess(Request)
        }else if(rl == "limit") {
          JsSuccess(Limit)
        }else {
          JsError(s"`${json}` must be one of 'request', 'limit'")
        }
      }
    }
  }
  val writesRequestLimit = new Writes[RequestLimit] {
    def writes(o: RequestLimit): JsValue = {
      o match {
        case Request => JsString("request")
        case Limit => JsString("limit")
      }
    }
  }

  case class Config(
    host_volume_whitelist: Seq[String] = Seq.empty,
    storage_classes: Seq[String] = Seq.empty,
    cpu_requirement_type: Set[RequestLimit] = Set(Request),
    memory_requirement_type: Set[RequestLimit] = Set(Limit, Request),
    affinity: Option[Pod.Affinity] = None,
    // https://kubernetes.io/docs/tasks/manage-gpus/scheduling-gpus/#clusters-containing-different-types-of-gpus
    gpu_default_node_selector: Map[String,String] = Map.empty
    // formerly:
    // `cpu-requirement-type`: String = "request",
    // `memory-requirement-type`: String = "limit,request"
  )
  case class Properties(
    config: Config = Config()
  )

  object Implicits {
    implicit val formatRequestLimit = Format(readsRequestLimit, writesRequestLimit)
    implicit val formatKubernetesProviderConfig = Jsonx.formatCaseClassUseDefaults[Config]
    implicit val formatKubernetesProviderProperties = Jsonx.formatCaseClassUseDefaults[Properties]
  }
}