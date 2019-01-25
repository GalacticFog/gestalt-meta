package com.galacticfog.gestalt.meta.providers.faas

import java.util.UUID
import scala.concurrent.Future
import javax.inject.Inject
import cats.syntax.either._
import cats.syntax.traverse._
import cats.instances.vector._
// import cats.instances.either._
import play.api.Logger
import play.api.libs.json._
import play.api.libs.ws.WSClient
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.errors.ApiError
import com.galacticfog.gestalt.meta.api.output.OrgCache
import com.galacticfog.gestalt.meta.api.patch.PatchInstance
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.util.Error
import com.galacticfog.gestalt.util.EitherWithErrors._
import com.galacticfog.gestalt.util.FutureFromTryST._
import controllers.util.{ProviderMethods,unstringmap}

import play.api.libs.concurrent.Execution.Implicits.defaultContext

class LaserProvider @Inject()(ws: WSClient, providerMethods: ProviderMethods) extends FaasProviderImplementation[Future] {

  private val log = Logger(this.getClass)

  import LambdaSpec.Implicits._
  import LaserProvider._

  def createLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))

    log.debug("Creating lambda in Laser...")
    for(
      laserLambda <- toLaserLambda(resource, provider.id);
      result <- client.post("/lambdas", Option(Json.toJson(laserLambda)))
    ) yield {
      if (Seq(200, 201).contains(result.status)) {
        log.info("Successfully created Lambda in backend system.")
        resource
      } else {
        throw ApiError(result.status, result.body).throwable
      }
    }
  }

  def importLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[GestaltResourceInstance] = {
    Future.failed(???)
  }

  def updateLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance, patch: PatchDocument): Future[GestaltResourceInstance] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    (for(
      patched <- Future.fromTryST(PatchInstance.applyPatch(resource, patch));
      laserLambda <- toLaserLambda(patched, provider.id);
      response <- client.put(s"/lambdas/${resource.id}", Some(Json.toJson(laserLambda)))
    ) yield {
      if(response.status == 200) {
        log.info(s"Successfully PUT Lambda in lambda provider.")
        ()
      }else {
        log.error(s"Error PUTting Lambda in lambda provider: ${response}")
        throw ApiError(response.status, response.body).throwable
      }
    }) flatMap { _ =>
      Future.fromTryST(PatchInstance.applyPatch(resource, patch))
      // we don't actually use the result from laser, though we probably should
    }
  }

  def deleteLambda(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[Unit] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    client.delete(s"/lambdas/${resource.id}") map { result =>
      log.info("Deleting API from Lambda backend...")
      log.debug(s"Response from Lambda backend: ${result.body}")
    } recover {
      case e: Throwable => {
        log.error(s"Error deleting lambda from Lambda backend: ${e.getMessage}")
        throw e
      }
    }
  }

  private def toLaserLambda(lambda: GestaltResourceInstance, providerId: UUID): Future[LaserLambda] = {

    log.debug("toLaserLambda(...)")

    val eitherL: EitherError[LaserLambda] = for(
      rawProperties <- Either.fromOption(lambda.properties, Error.Default(s"Cannot use lambda resource ${lambda.id} with unset properties map"));
      rawJsonProperties = unstringmap(Some(rawProperties)).get;
      properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[LaserLambdaProperties]);

      lambdaProvider <- Either.fromOption(ResourceFactory.findById(ResourceIds.LambdaProvider, properties.provider.id),
       Error.Default(s"Lambda '${properties.provider.id}' provider did not exist"));
      rawLambdaProviderProperties <- Either.fromOption(lambdaProvider.properties, Error.Default(s"Cannot use lambda provider resource ${lambdaProvider.id} with unset properties map"));
      rawJsonLambdaProviderProperties = unstringmap(Some(rawLambdaProviderProperties)).get;
      lambdaProviderProperties <- eitherFromJsResult(JsObject(rawJsonLambdaProviderProperties).validate[LambdaProviderProperties]);

      fqon <- Either.fromOption(OrgCache.getFqon(lambda.orgId), Error.Default(s"Could not determine FQON for lambda ${lambda.id}."));
      lambdaCaasProviderId <- Either.fromOption(lambdaProviderProperties.config.flatMap(_.env.public.get("META_COMPUTE_PROVIDER_ID")).map(UUID.fromString(_)),
       Error.Default(s"Lambda '${lambda.id}' provider '${lambdaProvider.id}' did not have '.properties.config.env.public.META_COMPUTE_PROVIDER_ID'"));
      lambdaEnvironment <- Either.fromOption(ResourceFactory.findParent(parentType=ResourceIds.Environment, childId=lambda.id),
       Error.Default(s"Could not locate parent Environment for Lambda '${lambda.id}'"));

      secrets <- properties.secrets.getOrElse(Seq()).toVector.traverse[EitherError,GestaltResourceInstance] { secretMount => 
        Either.fromOption(ResourceFactory.findById(ResourceIds.Secret, secretMount.secret_id),
          Error.Default(s"Secret '${secretMount.secret_id}' does not exist"))
      };
      secretProperties <- secrets.traverse[EitherError,SecretProperties] { secret =>
        for(
          rawProperties <- Either.fromOption(secret.properties, Error.Default(s"Cannot use secret resource ${secret.id} with unset properties"));
          rawJsonProperties = unstringmap(Some(rawProperties)).get;
          properties <- eitherFromJsResult(JsObject(rawJsonProperties).validate[SecretProperties])
        ) yield properties
      };
      _ <- secretProperties.map(_.provider.id).distinct match {
        case Vector() => Right(())
        case Vector(providerId) if providerId == lambdaCaasProviderId => Right(())
        case Vector(_) => Left(Error.Default(s"Lambda '${lambda.id}' provider '${properties.provider.id}' did not have same CaaS provider as mounted Secrets"))
        case _ => Left(Error.Default("Secrets must have the same CaaS provider"))
      };
      secretEnvironments <- secrets.traverse[EitherError,GestaltResourceInstance] { secret =>
        Either.fromOption(ResourceFactory.findParent(parentType=ResourceIds.Environment, childId=secret.id),
         Error.Default(s"Could not locate parent Environment for Secret '${secret.id}'"))
      };
      _ <- secretEnvironments.map(_.id).distinct match {
        case Vector() => Right(())
        case Vector(environmentId) if environmentId == lambdaEnvironment.id => Right(())
        case Vector(_) => Left(Error.Default(s"Lambda '${lambda.id}' must belong to the same Environment as all mounted Secrets"))
        case _ => Left(Error.Default("All mounted Secrets must belong to the same Environment"))
      }
    ) yield {
      val executorEnvironmentOptId = if(properties.pre_warm.exists(_ > 0)) {
        Some(lambdaEnvironment.id)
      }else {
        secretEnvironments.headOption.map(_.id)
      }
      val computePathOpt = executorEnvironmentOptId.map(controllers.routes.ContainerController.postContainer(fqon, _).url)
      LaserLambda(
        id = Some(lambda.id.toString),
        eventFilter = Some(UUID.randomUUID.toString),
        public = properties.public,
        provider = Some(Json.obj(
          "id" -> properties.provider.id.toString,
          "location" -> "",
          "href" -> "/foo/bar"
        )),
        LaserArtifactDescription(
          artifactUri = properties.package_url,
          description = None,     // no such property
          handler = properties.handler,
          memorySize = properties.memory.toInt,
          cpus = properties.cpus.toDouble,
          publish = false,     // <- currently not used
          role = "none",    // <- currently not used
          preWarm = properties.pre_warm,
          runtime = properties.runtime,
          timeoutSecs = properties.timeout,
          compressed = properties.compressed.getOrElse(false),
          periodicInfo = properties.periodic_info,
          code = properties.code,
          headers = properties.headers.getOrElse(Map()),
          isolate = properties.isolate,
          secrets = Some(properties.secrets.getOrElse(Seq()).map(Json.toJson(_).as[JsObject])),
          computePathOverride = computePathOpt
        )
      )
    }

    eitherL.liftTo[Future]
  }

  def getStreamDefinitions(provider: GestaltResourceInstance, resource: GestaltResourceInstance): Future[JsValue] = {
    val client = providerMethods.configureWebClient(provider, Some(ws))
    val streamUrl = s"/streamDefinitions/${resource.id}/streams"
    log.debug(s"Stream URL : $streamUrl")
    client.get(streamUrl) map { result =>
      Json.parse(result.body)
    } recover {
      case e: Throwable => {
        log.error(s"Error looking up streams: ${e.getMessage}")
        throw e
      }      
    }
  }
}

object LaserProvider {
  case class LaserArtifactDescription(
    artifactUri: Option[String],
    runtime: String,
    handler: String,
    memorySize: Int,
    cpus: Double,
    description: Option[String] = None,
    compressed: Boolean = false,
    publish: Boolean = false,
    role: String = "none",
    timeoutSecs: Int = 180,
    code: Option[String] = None,
    periodicInfo : Option[JsValue] = None,
    headers : Map[String,String] = Map.empty,
    computePathOverride: Option[String] = None,
    secrets: Option[Seq[JsObject]] = None,
    preWarm: Option[Int] = None,
    isolate: Option[Boolean] = None)

  case class LaserLambda(
    id: Option[String], 
    eventFilter: Option[String],
    public: Boolean,
    provider: Option[JsValue],
    artifactDescription: LaserArtifactDescription)
  
  implicit val laserArtifactDescriptionFormat: Format[LaserArtifactDescription] = Json.format[LaserArtifactDescription]
  implicit val laserLambdaFormat: Format[LaserLambda] = Json.format[LaserLambda]
}