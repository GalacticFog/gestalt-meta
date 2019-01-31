package controllers

import java.util.UUID
import javax.inject.Singleton
import com.google.inject.Inject
import cats.syntax.either._
import play.api.i18n.MessagesApi
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecurity
import com.galacticfog.gestalt.util.Error
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.ResourceSerde
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.meta.auth.Authorization
import controllers.util._

object Environment {

  case class Namespace(name: String, default: Boolean)
  case class ProviderMapping(namespaces: Seq[Namespace])

  case class EnvironmentProperties(
    environment_type: UUID,
    workspace: Option[UUID],
    provider_mapping: Option[Map[UUID,ProviderMapping]]
  )

  implicit def mapUuidReads[V: Reads] = new Reads[Map[UUID,V]] {
    def reads(json: JsValue): JsResult[Map[UUID,V]] = {
      val r = implicitly[Reads[Map[String,V]]]
      r.reads(json) map { mapping =>
        mapping map { case(key, value) =>
          (UUID.fromString(key), value)
        }
      }
    }
  }
  implicit def mapUuidWrites[V: Writes] = new Writes[Map[UUID,V]] {
    def writes(o: Map[UUID,V]): JsValue = {
      val w = implicitly[Writes[Map[String,V]]]
      val mapping = o map { case(key, value) =>
        (key.toString, value)
      }
      w.writes(mapping)
    }
  }

  implicit val formatNamespace = Json.format[Namespace]
  implicit val formatProviderMapping = Json.format[ProviderMapping]
  implicit val formatEnvironmentProperties = Json.format[EnvironmentProperties]

  def getDefaultNamespace(envId: UUID, providerId: UUID): EitherError[String] = {
    for(
      env <- eitherFrom[Error.NotFound].option(ResourceFactory.findById(ResourceIds.Environment, envId),
       s"Environment not found with id ${envId}");
      // will not check if provider exists for providerId - caller can do this better
      envProperties <- ResourceSerde.deserialize[EnvironmentProperties,Error.Default](env);
      defaultNamespaces = ProviderMapping(Seq(Namespace(s"${envId}", true)));
      providerMapping = envProperties.provider_mapping.getOrElse(Map());
      namespaces = providerMapping.get(providerId).getOrElse(defaultNamespaces);
      namespace <- eitherFromOption(namespaces.namespaces.find(_.default), s"Default namespace not set for environment=${envId}")
    ) yield namespace.name
  }
}

@Singleton
class EnvironmentProviderMappingController @Inject()(messagesApi: MessagesApi,
 sec: GestaltFrameworkSecurity) extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {

  import Environment._

  def getAll(fqon: String, envId: UUID) = Audited() { request =>
    (for(
      org <- eitherFromOption(Resource.findFqon(fqon), s"Organization not found with fqon ${fqon}");
      env <- eitherFrom[Error.NotFound].option(ResourceFactory.findById(ResourceIds.Environment, envId),
       s"Environment not found with id ${envId}");
      envProperties <- ResourceSerde.deserialize[EnvironmentProperties,Error.Default](env)
    ) yield {
      Ok(Json.toJson(envProperties.provider_mapping.getOrElse(Map.empty[UUID,ProviderMapping])))
    }).valueOr(errorToResult(_))
  }
  def get(fqon: String, envId: UUID, providerId: UUID) = Audited() { request =>
    (for(
      org <- eitherFromOption(Resource.findFqon(fqon), s"Organization not found with fqon ${fqon}");
      env <- eitherFrom[Error.NotFound].option(ResourceFactory.findById(ResourceIds.Environment, envId),
       s"Environment not found with id ${envId}");
      envProperties <- ResourceSerde.deserialize[EnvironmentProperties,Error.Default](env)
    ) yield {
      val namespaces = for(
        providerMapping <- envProperties.provider_mapping;
        namespaces <- providerMapping.get(providerId)
      ) yield namespaces
      val defaultNamespaces = ProviderMapping(Seq(Namespace(s"${envId}", true)))
      Ok(Json.toJson(namespaces.getOrElse(defaultNamespaces)))
    }).valueOr(errorToResult(_))
  }
  def post(fqon: String, envId: UUID, providerId: UUID) = Audited() { request =>
    (for(
      body <- eitherFrom[Error.BadRequest].option(request.body.asJson, "Malformed request payload");
      org <- eitherFromOption(Resource.findFqon(fqon), s"Organization not found with fqon ${fqon}");
      env <- eitherFrom[Error.NotFound].option(ResourceFactory.findById(ResourceIds.Environment, envId),
       s"Environment not found with id ${envId}");
      envProperties <- ResourceSerde.deserialize[EnvironmentProperties,Error.Default](env);
      newNamespaces <- eitherFromJsResult(body.validate[ProviderMapping]);
      providerMapping = envProperties.provider_mapping.getOrElse(Map());
      newProviderMapping = providerMapping ++ Map(providerId -> newNamespaces); 
      newEnvProperties = envProperties.copy(provider_mapping=Some(newProviderMapping));
      newEnv <- ResourceSerde.serialize[EnvironmentProperties](env, newEnvProperties)
    ) yield {
      ResourceFactory.update(newEnv, request.identity.account.id).get
      Accepted(Json.toJson(newNamespaces))
    }).valueOr(errorToResult(_))
  }
  def delete(fqon: String, envId: UUID, providerId: UUID) = Audited() { request =>
    (for(
      org <- eitherFromOption(Resource.findFqon(fqon), s"Organization not found with fqon ${fqon}");
      env <- eitherFrom[Error.NotFound].option(ResourceFactory.findById(ResourceIds.Environment, envId),
       s"Environment not found with id ${envId}");
      envProperties <- ResourceSerde.deserialize[EnvironmentProperties,Error.Default](env);
      newProviderMapping = envProperties.provider_mapping.getOrElse(Map()).-(providerId);
      newEnvProperties = envProperties.copy(provider_mapping=Some(newProviderMapping));
      newEnv <- ResourceSerde.serialize[EnvironmentProperties](env, newEnvProperties)
    ) yield {
      ResourceFactory.update(newEnv, request.identity.account.id).get
      Accepted(Json.obj())
    }).valueOr(errorToResult(_))
  }
}


