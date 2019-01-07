package migrations


import java.util.UUID

import com.galacticfog.gestalt.data.bootstrap.{ActionInfo, SystemType, _}
import com.galacticfog.gestalt.data.models.{GestaltResourceInstance, GestaltResourceType, GestaltTypeProperty}
import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory, session, _}
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceLabel}
import com.galacticfog.gestalt.meta.auth.{Entitlement, EntitlementProps}
import controllers.util.TypeMethods
import play.api.libs.json.{JsValue, Json}

import scala.util.{Either, Failure, Success, Try}

sealed trait ProcessState
object ProcessState {
  case object NothingToDo extends ProcessState
  case object Continue extends ProcessState
}

abstract class MetaMigration() {

  type CreateTypeFunction = (UUID, GestaltResourceInstance) => Try[GestaltResourceType]
  
  def migrate(identity: UUID, payload: Option[JsValue]): Either[JsValue,JsValue]

  private[migrations] def addEntitlementsToInstances(identity: UUID, action: String, typeId: UUID)
                                                    (implicit acc: MessageAccumulator): Seq[Try[Unit]] = {
    val allInstances = ResourceFactory.findAll(typeId)
    val tpe = TypeFactory.findById(typeId).map { t =>
      SystemType.fromResourceType(t)
    }.get

    val parentTypes = tpe.lineage.get.parent_types
    val allParents = parentTypes.flatMap(typeId => ResourceFactory.findAll(typeId))

    val allForUpdate = (allInstances ++ allParents)

    val entitlementExists = ResourceFactory.findEntitlementsByAction(action).map {
      ent => ResourceFactory.findParent(ent.id).get.id
    }

    def hasEntitlement(id: UUID): Boolean = {
      entitlementExists.contains(id)
    }

    acc push s"Adding '$action' entitlement to existing instances. (${allForUpdate.size} resources total)"

    if (allForUpdate.isEmpty) {
      acc push "There are no resource instances to update at this time."
      Seq(Success(()))
    } else {
      allForUpdate.map { p =>
        if (hasEntitlement(p.id)) {
          acc push s"${ResourceLabel(p.typeId)} '${p.id}' already has the entitlement. Skipping..."
          Success(())
        } else {
          val owner = p.owner.id
          val ent = Entitlement(
            id = UUID.randomUUID(),
            org = p.orgId,
            name = s"${p.id}.${action}",
            properties = EntitlementProps(action, None, Some(Seq(identity, owner))))

          ResourceFactory.create(ResourceIds.User, identity)(
            Entitlement.toGestalt(identity, ent), parentId = Some(p.id)) match {
            case Failure(e) => throw e
            case Success(_) => {
              acc push s"Entitlement added to ${ResourceLabel(p.typeId)} '${p.id}'"
              Success(())
            }
          }
        }
      }
    }
  }

  private[migrations] def addPropertyTypeToResourceType(tpe: GestaltResourceType,
                                                        prop: GestaltTypeProperty)
                                                       (implicit acc: MessageAccumulator): Try[GestaltTypeProperty] = {
    Try(PropertyFactory.findByName(tpe.id, prop.name)) flatMap {
      case Some(t) => {
        acc push s"Type ${tpe.id} already had '${prop.name}' property"
        Success(t)
      }
      case None => {
        PropertyFactory.create(tpe.owner.id)(prop) map { t =>
          acc push s"Added '${prop.name}' to Type ${tpe.id}"
          t
        } recoverWith {
          case e: Throwable => {
            acc push s"ERROR adding '${prop.name}' to Type ${tpe.id}: ${e.getMessage}"
            Failure(e)
          }
        }
      }
    }
  }

  private[migrations] def createResourceType(creator: GestaltResourceInstance,
                                             typeId: UUID, typeName: String,
                                             systemType: => SystemType)
                                            (implicit acc: MessageAccumulator): Try[GestaltResourceType] = {

    TypeFactory.findById(typeId) match {
      case Some(tpe) if tpe.name == typeName =>
        acc push s"ResourceType '${tpe.name}' (${tpe.id}) already exists"
        Success(tpe)
      case Some(tpe) =>
        acc push s"ResourceType '${tpe.name}' (${tpe.id}) exists but name is not '${typeName}"
        Failure(new ConflictException("type already exists"))
      case None =>
        Try { systemType.save() } flatMap { setTypeLineage(_, creator) }
    }
  }

  private[migrations] def setTypeLineage( newType: GestaltResourceType, creator: GestaltResourceInstance)
                                        ( implicit acc: MessageAccumulator) = {
    val newParentTypes = TypeMethods.getParentTypes(newType)
    for {
      _ <- {
        acc push s"Updating parent lineage for type ${newType.name}"
        updateParentLineage(creator.id, newType, newParentTypes)
      }
      _ <- {
        acc push s"Updating parent entitlements for type ${newType.name}"
        updateParentEntitlements(creator, creator.id, newType.id, newParentTypes)
      }
    } yield newType
  }

  /**
    * Add a new child type to a parent at the type level. This enables child-type to be a 'childOf'
    * the parent type in the schema (child-type is now a child of each given parent-type).
    */
  private[migrations] def updateParentLineage( caller: UUID,
                                               childType: GestaltResourceType,
                                               parents: Seq[UUID]): Try[Unit] = Try {
    // Update the parent-types with this new child type
    val results = TypeMethods.makeNewParents(caller, childType.id, parents)
    val errors = results.collect { case Failure(e) => e.getMessage }
    if (errors.nonEmpty) {
      val msg = errors.flatten.mkString(",")
      throw new RuntimeException(s"There were errors updating parent-type schemas: " + msg )
    }
  }

  private[migrations] def updateParentEntitlements( rootUser: GestaltResourceInstance,
                                                    callerId: UUID,
                                                    childType: UUID,
                                                    parents: Seq[UUID]): Try[Unit] = Try {

    val t = parents.foldLeft(Try(())) { (_, parent) =>
      TypeMethods.updateInstanceEntitlementsUserId(parent, childType, rootUser, callerId, None) match {
        case Left(errs) =>
          throw new RuntimeException("There were errors setting instance entitlements: " + errs.mkString(","))
        case Right(_) => Success(())
      }
    }
  }

  private[migrations] def addVerbToResourceType(identity: UUID, payload: Option[JsValue], verb: String, typeId: UUID)
                                               (implicit acc: MessageAccumulator) = Try {

    acc push s"Looking up Resource Type '$typeId'"
    val tpe = TypeFactory.findById(typeId).getOrElse {
      throw new RuntimeException(s"Could not find resource-type '$typeId'. This is a bug.")
    }

    val actions = tpe.properties.get.get("actions").getOrElse {
      throw new RuntimeException(s"Could not find '.properties.actions' on ${tpe.name}. This is a bug.")
    }

    acc push "Parsing .properties.actions to ActionInfo object."
    val ai = Js.parse[ActionInfo](Json.parse(actions)) match {
      case Failure(e) => {
        println(s"Failed parsing '.properties.actions' on ${tpe.name}")
        throw e
      }
      case Success(a) => a
    }

    acc push s"Adding new verb '$verb' to verbs list '${ai.verbs.mkString(",")}'"
    val newVerbs = (ai.verbs.toSet + verb).toSeq

    val jsonActions = Json.toJson(ai.copy(verbs = newVerbs))
    val newProps = tpe.properties.get ++ Map("actions" -> jsonActions.toString)
    val updated = tpe.copy(properties = Some(newProps))

    acc push "Performing update in Meta"
    TypeFactory.update(updated, identity).get
  }
  
  /**
   * Test if a ResourceType exists with the same name and ID as given.
   * 
   * @return ProcessState indicating how to proceed. If a type is found by either name or ID both values must match
   * the arguments given. If they do not match an error is thrown. If they match, return NothingToDo. If neither value
   * matches, return Continue.
   */
  def testExistingType(typeId: UUID, typeName: String, acc: MessageAccumulator): Try[ProcessState] = Try {
    val ad1 = TypeFactory.findByName(typeName)
    (if (ad1.isEmpty) ProcessState.Continue 
    else {
      // Type name exists - check that it has the expected ID
      if (ad1.get.id == typeId) {
        acc push s"Type ${typeName} with ID ${typeId} already exists. Nothing to do."
        ProcessState.NothingToDo
      } else {
        // Type exists but ID does not match expected - this is an error.
        throw new ConflictException(s"Type ${typeName} already exists, but its ID is unexpected. expected: ${typeId}, found: ${ad1.get.id}")
      }
    }) match {
      case ProcessState.NothingToDo => ProcessState.NothingToDo
      case ProcessState.Continue => {
        val ad2 = TypeFactory.findById(typeId)
        if (ad2.isEmpty) ProcessState.Continue
        else {
          // Type with ID already exists - check it has expected name
          if (ad2.get.name == typeName) {
            acc push s"Type ${typeName} with ID ${typeId} already exists. Nothing to do."
            ProcessState.NothingToDo
          } else {
            // Type with ID exists, buy name does not match expected - error.
            throw new ConflictException(s"Type with ID ${typeId} found, but name is unexpected. expected: ${typeName}. found: ${ad2.get.name}")
          }
        }
      }
    } 
  }  

  /**
   * Idempotent method to add a ResourceType to an Org. Tests first for a type with the same
   * Name and/or ID before creating. If type already exists, the function completes successfully
   * with a message explaining that the type already exists and there's "nothing to do".
   * 
   * @param org UUID of the Org where the new type will be created
   * @param newTypeId UUID of the new type
   * @param newTypeName name of the new type
   * @param identity of the User creating this type
   * @param payload Optional JSON for the creation method
   * @param acc A MessageAccumulator
   * @param typeFunction a function to create the new type
   */
  def addTypeToOrg(
      org: UUID,
      newTypeId: UUID,
      newTypeName: String,
      identity: UUID, 
      payload: Option[JsValue] = None,
      acc: MessageAccumulator)(
          typeFunction : CreateTypeFunction): Either[JsValue, JsValue] = {
    
    val process: Try[_] =
      testExistingType(newTypeId, newTypeName, acc).map { state =>
        state match {
          case ProcessState.NothingToDo => Success(())
          case ProcessState.Continue => for {
            creator <- Try {
              acc push s"Looking up creator '${identity}'"
              ResourceFactory.findById(ResourceIds.User, identity).getOrElse {
                throw new RuntimeException(s"Could not locate creator '${identity}'")
              }
            }
            newType <- {
              acc push s"Creating Type ${newTypeName}"
              typeFunction(org, creator)
            }
          } yield newType
        }
      }
    handleResultStatus(process, acc)
  }
  
  /**
   * General method to handle the final result and message stack of a migration process.
   */
  def handleResultStatus[A](result: Try[A], acc: MessageAccumulator): Either[JsValue,JsValue] = {
    result match {
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
