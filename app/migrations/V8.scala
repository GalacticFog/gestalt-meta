package migrations

import java.util.UUID

import scala.util.{Either, Left, Right}
import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.data.CoVariant
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.data.session
import play.api.libs.json._
import com.galacticfog.gestalt.meta.auth._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.meta.auth._
import com.galacticfog.gestalt.meta.providers.ProviderManager
import controllers.Meta
import controllers.util.JsonInput
import controllers.util.TypeMethods
import javax.inject.Inject

/*
 * Add DataFeed Resource Type
 * Add StreamProvider Resource Type
 * Add StreamSpec Resource Type
 */

class V8 @Inject()( meta: Meta, providerManager: ProviderManager ) extends MetaMigration with AuthorizationMethods {

  private val acc = new MessageAccumulator()
  
  private val START_PROVIDER_TYPE_ID = UUID.fromString("2167fbfc-60ab-42ac-bdd3-1ee5cf7b3437")
  private val START_PROVIDER_TYPE_NAME = "Gestalt::Action::StartStream"
  
  private val STOP_PROVIDER_TYPE_ID = UUID.fromString("12598413-a8b3-4ba9-89c6-fe375a5239da")
  private val STOP_PROVIDER_TYPE_NAME = "Gestalt::Action::StopStream"
  
  private val ENTITLEMENTS = Option(Seq(ResourceIds.Entitlement))

  def newActionProviderInstance(rootId: UUID, typeId: UUID, creator: GestaltResourceInstance, payload: JsObject) = {
    for {
      actionProvider <- CreateNewResource(
          org = rootId,
          creator = creator,
          json = payload,
          typeId = Option(typeId),
          parent = Option(rootId))
      providerEnv = providerManager.getOrCreateProviderEnvironment(actionProvider, creator)
      _ = meta.createProviderActions(actionProvider, creator, providerEnv)
      _ = setNewResourceEntitlements(rootId, actionProvider.id, creator, Some(rootId))
    } yield actionProvider
  }

  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    val process = for {

      root <- {
        acc push "Looking up 'root' org"
        ResourceFactory.findRootOrg
      }
      creator <- Try {
        acc push "Looking up creator"
        ResourceFactory.findById(ResourceIds.User, identity) getOrElse {
          throw new RuntimeException(s"Could not locate creator with id '${identity}'")
        }
      }
      _ <- {
        acc push "Adding Start Stream Provider Type to /root/resourcetypes"
        addStartProviderType(root.id, creator)
      }
      x <- {
        acc push "Adding Stop Stream Provider Type to /root/resourcetypes"
        addStopProviderType(root.id, creator)
      }
      _ <- {
        // Create start instance
        acc push "Creating Start Provider instance in /root"
        val payload = Json.obj(
          "name" -> "start-stream-default",
          "properties" -> Json.obj()
        )
        newActionProviderInstance(root.id, START_PROVIDER_TYPE_ID, creator, payload)
      }
      _ <- {
        // Create stop instance
        acc push "Creating Stop Provider instance in /root"
        val payload = Json.obj(
          "name" -> "stop-stream-default",
          "properties" -> Json.obj()
        )
        newActionProviderInstance(root.id, STOP_PROVIDER_TYPE_ID, creator, payload)
      }
    } yield x
    
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

  def addStartProviderType(org: UUID, creator: GestaltResourceInstance) = Try {
    val owner = ResourceOwnerLink(ResourceIds.User, creator.id)
    SystemType(org, owner,
      typeId      = START_PROVIDER_TYPE_ID, 
      typeName    = START_PROVIDER_TYPE_NAME,
      desc        = Some("Start stream provider action."),
      extend      = Some(ResourceIds.ActionProvider),
      selfProps   = Map("provider_actions" -> providerStartActions)

    ).withActionInfo (
      ActionInfo(
        prefix = "providers", 
        verbs  = Seq.empty)
        
    ).withLineageInfo (
       LineageInfo(
           parent_types = Seq(
               ResourceIds.Org,
               ResourceIds.Workspace,
               ResourceIds.Environment
           ),
           child_types  = ENTITLEMENTS)
           
    ).withApiInfo (
          TypeApiInfo(rest_name = "providers")
    
    ).save()
    
    TypeFactory.findById(START_PROVIDER_TYPE_ID) getOrElse {
      throw new RuntimeException("Failed creating StartProvider Resource Type")
    }
    
    //setTypeLineage(newtype, creator)
  }
  

  def addStopProviderType(org: UUID, creator: GestaltResourceInstance) = Try {
    val owner = ResourceOwnerLink(ResourceIds.User, creator.id)
    SystemType(org, owner,
      typeId      = STOP_PROVIDER_TYPE_ID, 
      typeName    = STOP_PROVIDER_TYPE_NAME,
      desc        = Some("Stop stream provider action."),
      extend      = Some(ResourceIds.ActionProvider),
      selfProps   = Map("provider_actions" -> providerStopActions)

    ).withActionInfo (
      ActionInfo(
        prefix = "providers", 
        verbs  = Seq.empty)
        
    ).withLineageInfo (    
       LineageInfo(
           parent_types = Seq(
               ResourceIds.Org,
               ResourceIds.Workspace,
               ResourceIds.Environment
           ),
           child_types  = ENTITLEMENTS)
           
    ).withApiInfo (
          TypeApiInfo(rest_name = "providers")
    
    ).save()    
    
    TypeFactory.findById(STOP_PROVIDER_TYPE_ID) getOrElse {
      throw new RuntimeException("Failed creating StopProvider Resource Type")
    }    
    
    //setTypeLineage(newtype, creator)
  }

  private val providerStartActions = Json.stringify(Json.parse(s"""
  [
    {
      "name": "Start Stream",
      "implementation": {
        "kind": "MetaCallback",
        "method": "POST",
        "uri": "/streamspecs/{resource_id}?action=start",
        "input": {
          "kind": "inlineHtml",
          "style": "",
          "script": "d2l0aEhlYWRlcignU3RhcnQgU3RyZWFtU3BlYycpCndpdGhJbnN0cnVjdGlvbnMoJ0FyZSB5b3Ugc3VyZSB5b3Ugd2FudCB0byBzdGFydCB0aGlzIFN0cmVhbVNwZWMnKQp3aXRoRm9ybVRpdGxlKCdTcGVjIElEJykKCmdldCgnc3BlY19pZCcpLnZhbHVlID0gX2N0eC5yZXNvdXJjZS5pZCAKCl9fYmluZFN1Ym1pdChfY3R4Lmludm9rZV91cmwsIGdldFRva2VuKCkpCg==",
          "data": "PGRpdiBjbGFzcz0icm93Ij4KICA8ZGl2IGNsYXNzPSJpbnB1dC1maWVsZCBjb2wgczEyIj4KICAgIDxpbnB1dCByZXF1aXJlZCBkaXNhYmxlZAogICAgICAgICAgIHR5cGU9InRleHQiIGlkPSJzcGVjX2lkIj4KICA8L2Rpdj4KPC9kaXY+Cg=="
        }
      },
      "ui_locations": [
        {
          "name": "streamspec.edit"
        }
      ]
    }
  ]
  """))


  private val providerStopActions = Json.stringify(Json.parse(s"""
  [
    {
      "name": "Stop Stream",
      "implementation": {
        "kind": "MetaCallback",
        "method": "POST",
        "uri": "/streamspecs/{resource_id}?action=stop",
        "input": {
          "kind": "inlineHtml",
          "style": "",
          "script": "d2l0aEhlYWRlcignU3RvcCBTdHJlYW0nKQp3aXRoSW5zdHJ1Y3Rpb25zKCdBcmUgeW91IHN1cmUgeW91IHdhbnQgdG8gc3RvcCB0aGlzIFN0cmVhbSBQcm9jZXNzJykKd2l0aEZvcm1UaXRsZSgnU3RyZWFtIElEJykKCmdldCgnc3RyZWFtX2lkJykudmFsdWUgPSBfY3R4LnJlc291cmNlLmlkCgpfX2JpbmRTdWJtaXQoX2N0eC5pbnZva2VfdXJsLCBnZXRUb2tlbigpKQo=",
          "data": "PGRpdiBjbGFzcz0icm93Ij4KICA8ZGl2IGNsYXNzPSJpbnB1dC1maWVsZCBjb2wgczEyIj4KCTxpbnB1dCByZXF1aXJlZCBkaXNhYmxlZAogICAgICAgICAgIGlkPSJzdHJlYW1faWQiCiAgICAgICAgICAgdHlwZT0idGV4dCIKICAgICAgICAgIC8+CiAgPC9kaXY+CjwvZGl2Pg=="
        }
      },
      "ui_locations": [
        {
          "name": "streamspec.instances"
        }
      ]
    }
  ]
  """))
  
  private[migrations] def setTypeLineage(newType: GestaltResourceType, creator: GestaltResourceInstance) = {
    val newParentTypes = TypeMethods.getParentTypes(newType)
    val results = for {
      _ <- {
        acc push s"Updating parent lineage for type ${newType.name}"
        updateParentLineage(creator.id, newType, newParentTypes)
      }
      a <- {
        acc push s"Updating parent entitlements for type ${newType.name}"
        updateParentEntitlements(creator, creator.id, newType.id, newParentTypes)
      }
    } yield a    
  }  
  
  /**
   * Add a new child type to a parent at the type level. This enables child-type to be a 'childOf'
   * the parent type in the schema (child-type is now a child of each given parent-type).
   */
  private[migrations] def updateParentLineage(
      caller: UUID, 
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

  private[migrations] def updateParentEntitlements(
      rootUser: GestaltResourceInstance,
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
  
}