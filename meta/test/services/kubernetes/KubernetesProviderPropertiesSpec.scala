package services.kubernetes

// import java.util.UUID
import play.api.libs.json._
import play.api.test.PlaySpecification
import org.specs2.specification.{BeforeAfterEach, BeforeAll}
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.test.ResourceScope

class KubernetesProviderPropertiesSpec extends PlaySpecification with ResourceScope with BeforeAll with BeforeAfterEach {
  override def beforeAll(): Unit = pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential

  "KubernetesProviderProperties#KubernetesProviderConfig" should {
    import KubernetesProviderProperties.Implicits._

    def validateProvider(provider: GestaltResourceInstance): JsResult[KubernetesProviderProperties.Config] = {
      val rawConfig = for(
        props <- provider.properties;
        config <- props.get("config")
      ) yield config
      Json.parse(rawConfig.get).validate[KubernetesProviderProperties.Config]
    }

    "validate empty provider config" in {
      val testK8SProvider = newInstance(
        typeId = ResourceIds.KubeProvider,
        name = "test-provider",
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> "{}"
        ))
      )
      validateProvider(testK8SProvider) === JsSuccess(KubernetesProviderProperties.Config())
    }
    "validate full provider config" in {
      val testK8SProvider = newInstance(
        typeId = ResourceIds.KubeProvider,
        name = "test-provider",
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> Json.obj(
            "host_volume_whitelist" -> Json.arr("/supported-path/sub-path"),
            "storage_classes" -> Json.arr("storage-class-1"),
            "cpu_requirement_type" -> Json.arr("request", "limit"),
            "memory_requirement_type" -> Json.arr("request", "limit")
          ).toString
        ))
      )
      validateProvider(testK8SProvider) === JsSuccess(KubernetesProviderProperties.Config(
        host_volume_whitelist = Seq("/supported-path/sub-path"),
        storage_classes = Seq("storage-class-1"),
        cpu_requirement_type = Set(KubernetesProviderProperties.Request, KubernetesProviderProperties.Limit),
        memory_requirement_type = Set(KubernetesProviderProperties.Request, KubernetesProviderProperties.Limit)
      ))
    }
    "validate provider config with empty requirements" in {
      val testK8SProvider = newInstance(
        typeId = ResourceIds.KubeProvider,
        name = "test-provider",
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> Json.obj(
            "cpu_requirement_type" -> Json.arr(),
            "memory_requirement_type" -> Json.arr()
          ).toString
        ))
      )
      validateProvider(testK8SProvider) === JsSuccess(KubernetesProviderProperties.Config(
        cpu_requirement_type = Set(),
        memory_requirement_type = Set()
      ))
    }
    "fail to validate invalid requirement type" in {
      val testK8SProvider = newInstance(
        typeId = ResourceIds.KubeProvider,
        name = "test-provider",
        properties = Option(Map(
          "parent" -> "{}",
          "config" -> Json.obj(
            "cpu_requirement_type" -> Json.arr("request", "limit", "sdfsd")
          ).toString
        ))
      )
      validateProvider(testK8SProvider).toString === """JsError(List((/cpu_requirement_type(2),List(JsonValidationError(List(`"sdfsd"` must be one of 'request', 'limit'),WrappedArray())))))"""
    }
  }
}