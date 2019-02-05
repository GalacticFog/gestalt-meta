package migrations

import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.sdk.{ResourceOwnerLink, _}
import com.galacticfog.gestalt.meta.auth._
import play.api.libs.json._

import scala.util.{Either, Failure, Left, Right, Success}

/**
 * Add Job ("Gestalt::Resource::Job") Resource Type to Meta.
 */
class V33() extends MetaMigration with AuthorizationMethods {

  import V33._

  private implicit val acc = new MessageAccumulator()

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    val process = for {
      root <- {
        acc push "Looking up 'root' org"
        ResourceFactory.findRootOrg
      }
      volumeTpe <- createResourceType(
        root, JOB_TYPE_ID, JOB_TYPE_NAME,
        SystemType(root.id, ResourceOwnerLink(ResourceIds.Org, root.id),
          typeId = JOB_TYPE_ID,
          typeName = JOB_TYPE_NAME,
          desc = Some("One-off workload (e.g. kubernetes job)"),
          extend = Some(ResourceIds.Resource)
        ).withTypeProperties(
          TypeProperty("container_type", "string"),
          TypeProperty("image", "string"),
          TypeProperty("provider", "json"),
          TypeProperty("port_mappings", "json", require = "optional"),
          TypeProperty("cpus", "float", require = "optional"),
          TypeProperty("memory", "float", require = "optional"),
          TypeProperty("disk", "float", require = "optional"),
          TypeProperty("num_instances", "int", require = "optional"),
          TypeProperty("network", "string", require = "optional"),
          TypeProperty("cmd", "string", require = "optional"),
          TypeProperty("args", "string::list", require = "optional"),
          TypeProperty("force_pull", "boolean", require = "optional"),
          TypeProperty("health_checks", "json::list", require = "optional"),
          TypeProperty("volumes", "json::list", require = "optional"),
          TypeProperty("secrets", "json::list", require = "optional"),
          TypeProperty("labels", "json", require = "optional"),
          TypeProperty("env", "json", require = "optional"),
          TypeProperty("state", "string", require = "optional"),
          TypeProperty("external_id", "string", require = "optional"),
          TypeProperty("age", "datetime", require = "optional"),
          TypeProperty("status", "string", require = "optional"),
          TypeProperty("tasks_running", "int", require = "optional"),
          TypeProperty("tasks_healthy", "int", require = "optional"),
          TypeProperty("tasks_unhealthy", "int", require = "optional"),
          TypeProperty("tasks_staged", "int", require = "optional"),
          TypeProperty("user",   "string", require = "optional"),
          TypeProperty("constraints", "string::list", require = "optional"),
          TypeProperty("accepted_resource_roles", "string::list", require = "optional"),
          TypeProperty("instances", "json::list", require = "optional"),
          TypeProperty("service_addresses", "json::list", require = "optional"),  
          TypeProperty("status_detail", "json", require = "optional"),
          TypeProperty("events", "json::list", require = "optional")  
        )
        // .withActionInfo(ActionInfo(
        //   prefix = "job",
        //   verbs = Seq("import")
        // ))
        .withLineageInfo(LineageInfo(
          parent_types = Seq(ResourceIds.Environment),
          child_types = Option(Seq(ResourceIds.Entitlement))
        )).withApiInfo(TypeApiInfo(
          rest_name = "jobs"
        ))
      )
    } yield volumeTpe

    process match {
      case Success(_) => {
        acc push "Meta update successful."
        Right(MigrationStatus(
            status = "SUCCESS",
            message = s"Upgrade successfully applied",
            succeeded = Some(acc.messages),
            None).toJson)
      }
      case Failure(e) => {
        Left(MigrationStatus(
            status = "FAILURE",
            message = s"There were errors performing this upgrade: ${e.getMessage}",
            succeeded = Some(acc.messages),
            errors = Some(Seq(e.getMessage))).toJson)
      }
    }

  }

}

object V33 {
  val JOB_TYPE_ID = UUID.fromString("c8ce1302-6f66-4300-8429-2574abf28d81")
  val JOB_TYPE_NAME = "Gestalt::Resource::Job"
}
