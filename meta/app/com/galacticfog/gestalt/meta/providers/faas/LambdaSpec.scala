package com.galacticfog.gestalt.meta.providers.faas

import java.util.UUID
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.providers.LinkedProvider

case class InlineSecretProvider(
  id: UUID
)
case class SecretProperties(
  provider: InlineSecretProvider
)

case class LambdaProviderPropertiesConfigEnv(
  public: Map[String,String]
)
case class LambdaProviderPropertiesConfig(
  env: LambdaProviderPropertiesConfigEnv
)
case class LambdaProviderProperties(
  schema: Option[String],
  config: Option[LambdaProviderPropertiesConfig],
  linked_providers: Option[Seq[LinkedProvider]],
  services: Option[Seq[JsValue]],
  environments: Option[Seq[String]],
  config_schema: Option[JsValue],
  provider_actions: Option[Seq[JsValue]],
  environment_types: Option[Seq[String]]
)

case class InlineLambdaProvider(
  id: UUID,
  locations: Option[Seq[JsValue]]
)
case class GPUSupport(enabled: Boolean, count: Int, `type`: String)
case class LaserLambdaProperties(
  public: Boolean,
  runtime: String,
  code_type: String,
  memory: Int,
  cpus: Double,
  timeout: Int,
  provider: InlineLambdaProvider,
  handler: String,
  messaging: Option[JsValue],
  package_url: Option[String],
  code: Option[String],
  parent: Option[JsValue],
  headers: Option[Map[String,String]],
  compressed: Option[Boolean],
  periodic_info: Option[JsValue],
  apiendpoints: Option[JsValue],
  pre_warm: Option[Int],
  secrets: Option[Seq[ContainerSpec.SecretMount]],
  linked_providers: Option[Seq[LinkedProvider]],
  isolate: Option[Boolean],
  gpu_support: Option[GPUSupport]
)
case class AWSLambdaProperties(
  public: Boolean,
  runtime: String,
  code_type: String,
  memory: Int,
  timeout: Int,
  provider: InlineLambdaProvider,
  handler: String,
  package_url: Option[String],
  code: Option[String],
  parent: Option[JsValue],
  env: Option[Map[String,String]],
  aws_role_id: Option[String],
  aws_function_id: Option[String]
)

case class StreamSpecProcessor(
  lambdaId: UUID
)

case class StreamSpecProperties(
  processor: StreamSpecProcessor
)

object LambdaSpec {
  object Implicits {

    implicit val inlineSecretProviderFormat = Json.format[InlineSecretProvider]
    implicit val secretPropertiesFormat = Json.format[SecretProperties]

    implicit val lambdaProviderPropertiesConfigEnvFormat = Json.format[LambdaProviderPropertiesConfigEnv]
    implicit val lambdaProviderPropertiesConfigFormat = Json.format[LambdaProviderPropertiesConfig]
    implicit val linkedProviderFormat = Json.format[LinkedProvider]
    implicit val lambdaProviderPropertiesFormat = Json.format[LambdaProviderProperties]

    implicit val gpuSupportFormat = Json.format[GPUSupport]
    implicit val inlineLambdaProviderFormat = Json.format[InlineLambdaProvider]
    implicit val laserLambdaPropertiesFormat = Json.format[LaserLambdaProperties]
    implicit val awsLambdaPropertiesFormat = Json.format[AWSLambdaProperties]

    implicit val streamSpecProcessorFormat = Json.format[StreamSpecProcessor]
    implicit val streamSpecPropertiesFormat = Json.format[StreamSpecProperties]
  }
}