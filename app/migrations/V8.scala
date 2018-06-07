package migrations

import java.util.UUID

import scala.util.{Either, Left, Right}
import scala.util.{Try, Success, Failure}

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


/*
 * Add DataFeed Resource Type
 * Add StreamProvider Resource Type
 * Add StreamSpec Resource Type
 */

class V8 extends MetaMigration() {

  private val acc = new MessageAccumulator()
  
  private val START_PROVIDER_TYPE_ID = UUID.fromString("2167fbfc-60ab-42ac-bdd3-1ee5cf7b3437")
  private val START_PROVIDER_TYPE_NAME = "Gestalt::Action::StartStream"
  
  private val STOP_PROVIDER_TYPE_ID = UUID.fromString("12598413-a8b3-4ba9-89c6-fe375a5239da")
  private val STOP_PROVIDER_TYPE_NAME = "Gestalt::Action::StopStream"
  
  private val ENTITLEMENTS = Option(Seq(ResourceIds.Entitlement))
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    
    val owner = ResourceOwnerLink(ResourceIds.User, identity)
   
    val process = for {
      org <- ResourceFactory.findRootOrg.map(_.id)
      
      _ <- {
        acc push "Adding Start Stream Provider Type to /root/resourcetypes"
        addStartProviderType(org, owner)
      }
      x <- {
        acc push "Adding Stop Stream Provider Type to /root/resourcetypes"
        addStopProviderType(org, owner)
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
  
  def addRootStartInstance() = {
    
  }
  
  def addRootStopInstance() = {
    
  }
  
  def addStartProviderType(org: UUID, owner: ResourceOwnerLink) = Try {
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
      new RuntimeException("Failed creating StartProvider Resource Type")
    }    
  }
  

  def addStopProviderType(org: UUID, owner: ResourceOwnerLink) = Try {
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
      new RuntimeException("Failed creating StopProvider Resource Type")
    }    
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
  			  "script": "d2l0aEhlYWRlcignU3RhcnQgU3RyZWFtU3BlYycpCndpdGhJbnN0cnVjdGlvbnMoJ0FyZSB5b3Ugc3VyZSB5b3Ugd2FudCB0byBzdGFydCB0aGlzIFN0cmVhbVNwZWMnKQp3aXRoRm9ybVRpdGxlKCdTcGVjIElEJykKCmdldCgnc3BlY19pZCcpLnZhbHVlID0gX2N0eC5yZXNvdXJjZS5pZAoKY29uc3QgZm9ybSA9IGdldChHRl9DT05UUk9MU19GT1JNKTsKY29uc3QgZXhlY3V0ZUFjdGlvbiA9IChldmVudCkgPT4gewoJYWxlcnQoX2N0eC5pbnZva2VfdXJsKTsKICAgIGV2ZW50LnByZXZlbnREZWZhdWx0KCk7Cn0gCgpmb3JtLmFkZEV2ZW50TGlzdGVuZXIoInN1Ym1pdCIsIGV4ZWN1dGVBY3Rpb24pOwoKCgoKCgo=",
  			  "data": "PGRpdiBjbGFzcz0icm93Ij4KICA8ZGl2IGNsYXNzPSJpbnB1dC1maWVsZCBjb2wgczEyIj4KCTxpbnB1dCByZXF1aXJlZCBkaXNhYmxlZAogICAgICAgICAgIGlkPSJzcGVjX2lkIgogICAgICAgICAgIHR5cGU9InRleHQiCiAgICAgICAgICAvPgogIDwvZGl2Pgo8L2Rpdj4K"
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
	        	"script": "d2l0aEhlYWRlcignU3RvcCBTdHJlYW0nKQp3aXRoSW5zdHJ1Y3Rpb25zKCdBcmUgeW91IHN1cmUgeW91IHdhbnQgdG8gc3RvcCB0aGlzIFN0cmVhbSBQcm9jZXNzJykKd2l0aEZvcm1UaXRsZSgnU3RyZWFtIElEJykKCmdldCgnc3RyZWFtX2lkJykudmFsdWUgPSBfY3R4LnJlc291cmNlLmlkCgpfX2JpbmRTdWJtaXQoX2N0eC5pbnZva2VfdXJsLCBnZXRUb2tlbigpKQo=", "data": "PGRpdiBjbGFzcz0icm93Ij4KICA8ZGl2IGNsYXNzPSJpbnB1dC1maWVsZCBjb2wgczEyIj4KCTxpbnB1dCByZXF1aXJlZCBkaXNhYmxlZAogICAgICAgICAgIGlkPSJzdHJlYW1faWQiCiAgICAgICAgICAgdHlwZT0idGV4dCIKICAgICAgICAgIC8+CiAgPC9kaXY+CjwvZGl2Pg=="
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
}