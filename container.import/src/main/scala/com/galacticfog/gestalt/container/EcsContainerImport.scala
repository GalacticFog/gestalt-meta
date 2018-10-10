package com.galacticfog.gestalt.container

// import java.io.{StringWriter,PrintWriter}
// import scala.util.{Try,Success,Failure}
import scala.collection.JavaConversions._
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceInput
import com.galacticfog.gestalt.meta.api.sdk.GestaltResourceOutput
import com.galacticfog.gestalt.meta.api.ContainerSpec
import play.api.libs.json._
import cats.syntax.either._
import cats.Semigroup
import cats.syntax.semigroup._
import cats.instances.vector._
import cats.instances.either._
import cats.syntax.traverse._
import com.amazonaws.services.ecs.model._
import com.galacticfog.gestalt.integrations.ecs._

object EcsContainerImport {
  case class Payload(
    action: String,
    provider: GestaltResourceOutput,
    resource: GestaltResourceOutput
  )

  implicit val gestaltResourceOutputReads = Json.reads[GestaltResourceOutput]
  implicit val payloadReads = Json.reads[Payload]

  implicit val doubleSemigroup = new Semigroup[Double] {
    def combine(a: Double, b: Double): Double = if(a == 0.0) { b }else { a }
  }

  implicit val stringSemigroup = new Semigroup[String] {
    def combine(a: String, b: String): String = if(a == "" || a == "n/a") { b }else { a }
  }

  implicit def optionSemigroup[A: Semigroup] = new Semigroup[Option[A]] {
    def combine(a: Option[A], b: Option[A]): Option[A] = {
      (a, b) match {
        case (Some(a), Some(b)) => Some(a |+| b)
        case (_, _) => if(a.isEmpty) { b }else { a }
      }
    }
  }

  implicit def seqSemigroup[A] = new Semigroup[Seq[A]] {
    def combine(a: Seq[A], b: Seq[A]): Seq[A] = if(a.isEmpty) { b }else { a }
  }

  implicit val inputProviderSemigroup = new Semigroup[ContainerSpec.InputProvider] {
    def combine(a: ContainerSpec.InputProvider, b: ContainerSpec.InputProvider): ContainerSpec.InputProvider = {
      if(a.id == b.id) {
        ContainerSpec.InputProvider(
          a.id,
          a.name |+| b.name,
          a.locations |+| b.locations
        )
      }else {
        a
      }
    }
  }

  implicit val containerSpecSemigroup = new Semigroup[ContainerSpec] {
    def combine(a: ContainerSpec, b: ContainerSpec): ContainerSpec = {
      ContainerSpec(
        a.name |+| b.name,
        a.description |+| b.description,
        a.container_type,
        a.image |+| b.image,
        a.provider |+| b.provider,
        a.port_mappings |+| b.port_mappings,
        a.cpus |+| b.cpus,
        a.memory |+| b.memory,
        a.disk |+| b.disk,
        a.num_instances,
        a.network |+| b.network,
        a.cmd |+| b.cmd,
        a.constraints |+| b.constraints,
        a.accepted_resource_roles |+| b.accepted_resource_roles,
        a.args |+| b.args,
        a.force_pull,
        a.health_checks |+| b.health_checks,
        a.volumes |+| b.volumes,
        a.labels,
        a.env,
        a.user |+| b.user,
        a.external_id |+| b.external_id,
        a.created,
        a.secrets |+| b.secrets
      )
    }
  }
}

class EcsContainerImport extends ContainerImport with FromJsResult {
  import EcsContainerImport._

  val clientFactory: EcsClientFactory = new DefaultEcsClientFactory()

  type EitherString[A] = Either[String,A]

  def containerImport(containerSpec: ContainerSpec, client: EcsClient): EitherString[ContainerSpec] = {
    val Some(externalId) = containerSpec.external_id

    def mkInlineVolumeMountSpec(mountPointMapping: Map[String,String])(volume: Volume): EitherString[ContainerSpec.VolumeMountSpec] = {
      for(
        hostVolumeProperties <- Either.fromOption(Option(volume.getHost()), "Only host path volumes (bind mounts) are supported");
        sourcePath = hostVolumeProperties.getSourcePath();
        mountPath <- Either.fromOption(mountPointMapping.get(volume.getName()), s"Container does not define a mount point for ${volume.getName}")
      ) yield {
        val resource = GestaltResourceInput(
          name = volume.getName(),
          id = None,
          resource_type = None,
          resource_state = None,
          properties = Some(Map(
            "type" -> JsString("host_path"),
            "access_mode" -> JsString("ReadWriteMany"),
            "size" -> JsNumber(1),
            "config" -> JsString(s"""{
              "host_path": "${sourcePath}"
            }""")
          ))
        )
        ContainerSpec.InlineVolumeMountSpec(mountPath, resource)
      }
    }

    val dsr = new DescribeServicesRequest()
      .withCluster(client.cluster)
      .withServices(externalId)
    for(
      service <- client.client.describeServices(dsr).getServices().toSeq match {
        case Seq(service) => Right(service)
        case Seq() => Left(s"No service entity found for `${externalId}`")
        case response => throw new RuntimeException(s"Invalid response: $response")
      };
      _ <- if(service.getLaunchType() == client.launchType) { Right(()) }else {
        Left(s"The service `${externalId}` belongs to `${service.getLaunchType()}` launch type; this provider is configured to use `${client.launchType}`")
      };
      dtdr = new DescribeTaskDefinitionRequest().withTaskDefinition(service.getTaskDefinition());
      taskDefn = client.client.describeTaskDefinition(dtdr).getTaskDefinition();
      containerDefn <- taskDefn.getContainerDefinitions().toSeq match {
        case Seq(containerDefn) => Right(containerDefn)
        case Seq() => Left(s"No container definitions found for `${externalId}`")
        case _ => {
          Left(s"More than one container per task definition is defined for `${externalId}`; this is not supported")
          // anything I need to do about it?
        }
      };
      mountPointMapping = (Option(containerDefn.getMountPoints()) map { mps =>
        mps.toSeq map { mp =>
          (mp.getSourceVolume(), mp.getContainerPath())
        }
      }).map(_.toMap).getOrElse(Map());
      inputVolumes = Option(taskDefn.getVolumes()).map(_.toSeq).getOrElse(Seq());
      volumes <- inputVolumes.toVector.traverse(mkInlineVolumeMountSpec(mountPointMapping))
    ) yield {
      val entrypoint = Option(containerDefn.getEntryPoint()).map(_.toSeq.mkString(" "))
      val command = Option(containerDefn.getCommand()).map(_.toSeq)
      val environment = containerDefn.getEnvironment() map { kv =>
        (kv.getName(), kv.getValue())
      }
      val healthChecks = (Option(containerDefn.getHealthCheck()) map { hc =>
        ContainerSpec.HealthCheck(
          "COMMAND",
          command = Some(hc.getCommand().toSeq.mkString(" ")),
          grace_period_seconds = hc.getStartPeriod(),
          interval_seconds = hc.getInterval(),
          timeout_seconds = hc.getTimeout(),
          max_consecutive_failures = hc.getRetries()
        )
      }) match {
        case Some(hc) => Seq(hc)
        case None => Seq()
      }
      val portMappings = containerDefn.getPortMappings().toSeq map { pm =>
        ContainerSpec.PortMapping(
          protocol = pm.getProtocol(),
          container_port = Option(pm.getContainerPort()),
          host_port = Option(pm.getHostPort())
        )
      }
      val network = if(service.getLaunchType() == "FARGATE") {
        val networkConfiguration = Option(service.getNetworkConfiguration()).flatMap { a => Option(a.getAwsvpcConfiguration()) }
        networkConfiguration.map(_.getSubnets().toSeq.mkString(";"))
      }else {
        Option(taskDefn.getNetworkMode())
      }
      val updatedContainerSpec = ContainerSpec(
        name = Option(containerDefn.getName()).getOrElse(""),
        container_type = "DOCKER",
        image = Option(containerDefn.getImage()).getOrElse(""),
        provider = containerSpec.provider,
        port_mappings = portMappings,
        cpus = (Option(taskDefn.getCpu()) |+| Option(containerDefn.getCpu()).map(_.toString)).map(_.toDouble / 1024.0).getOrElse(0.0),
        memory = (Option(taskDefn.getMemory()) |+| Option(containerDefn.getMemory()).map(_.toString)).map(_.toDouble).getOrElse(0.0),
        num_instances = service.getDesiredCount(),
        network = network,
        cmd = if(entrypoint == Some("")) { None }else { entrypoint },
        args = if(command == Some(Seq())) { None }else { command },
        health_checks = healthChecks,
        volumes = volumes,
        labels = Option(containerDefn.getDockerLabels()).map(_.toMap).getOrElse(Map()),
        env = environment.toMap,
        user = Option(containerDefn.getUser())
      )
      updatedContainerSpec |+| containerSpec
    }
  }

  def parsePayload(payloadStr: String): EitherString[(GestaltResourceOutput,ContainerSpec,EcsClient)] = {
    // https://stackoverflow.com/questions/23571677/22-fields-limit-in-scala-2-11-play-framework-2-3-case-classes-and-functions/23588132#23588132
    // It's funny, but play does not seem to provide a way to serialize or deserialize case classes with more than 22 fields
    // ContainerSpec is already a couple of fields too big, so there is no way to update Reads[ContainerSpec] to make it extract external_id
    for(
      payloadJson <- Right(Json.parse(payloadStr));
      payload <- fromJsResult(payloadJson.validate[Payload]);
      resourceProperties <- Either.fromOption(payload.resource.properties, "Resource properties cannot be empty");
      containerSpec0 <- fromJsResult(resourceProperties.validate[ContainerSpec]);
      externalId <- fromJsResult((payloadJson \ "resource" \ "properties" \ "external_id").validate[String]);     // see above
      containerSpec = containerSpec0.copy(external_id=Some(externalId));
      providerProperties <- Either.fromOption(payload.provider.properties, "Provider properties cannot be empty");
      providerSubtype <- fromJsResult((providerProperties \ "provider_subtype").validate[String]);
      providerConfig <- fromJsResult((providerProperties \ "config").validate[JsValue]);
      client <- clientFactory.getEcsClient(providerSubtype, providerConfig.toString);
      _ <- if(payload.action != "container.import") { Left(s"Unsupported action: `${payload.action}`") }else { Right(()) }
    ) yield (payload.resource, containerSpec, client)
  }

  def run(payloadStr: String, ctxStr: String): String = {
    // Try {
    (for(
      resourceContainerSpecEcsClient <- parsePayload(payloadStr);
      updatedContainerSpec <- containerImport(resourceContainerSpecEcsClient._2, resourceContainerSpecEcsClient._3);
      containerSpecSerialized <- Right(Json.toJson(updatedContainerSpec).as[JsObject]);
      resourceSerialized <- Right(Json.toJson(resourceContainerSpecEcsClient._1).as[JsObject])
    ) yield {
      // these three fields are not serialized in reads[ContainerSpec] so I have to add them by hand
      val obligatoryObj = Json.obj(
        "name" -> updatedContainerSpec.name,
        "external_id" -> updatedContainerSpec.external_id
      )
      val optionalObj = updatedContainerSpec.description match {
        case Some(description) => Json.obj("description" -> description)
        case None => Json.obj()
      }
      val resourceProperties = containerSpecSerialized ++ obligatoryObj ++ optionalObj - "name"    // "Unknown properties: name" – ???
      resourceSerialized + ("properties" -> resourceProperties)
    }) match {
      case Left(errorMessage) => Json.obj("actionFailed" -> errorMessage).toString
      case Right(response) => response.toString
    }
    // } match {
    //   case Failure(throwable) => {
    //     val errors = new StringWriter()
    //     throwable.printStackTrace(new PrintWriter(errors))
    //     Json.obj("actionFailed" -> throwable.getMessage(), "traceback" -> errors.toString.replace("\"", "\'")).toString
    //   }
    //   case Success(response) => response
    // }
  }
}