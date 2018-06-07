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

class V7 extends MetaMigration() {

  private val acc = new MessageAccumulator()
  
  private val DATA_FEED_TYPE_ID = UUID.fromString("8f875ffc-69ff-48c8-9d6d-f3622b7b1062")
  private val DATA_FEED_TYPE_NAME = "Gestalt::Resource::Configuration::DataFeed"
  
  private val STREAM_PROVIDER_TYPE_ID = UUID.fromString("b7f764be-9ae6-4f70-9c42-849cc881125f")
  private val STREAM_PROVIDER_TYPE_NAME = "Gestalt::Configuration::Provider::StreamProvider"
  
  private val STREAM_SPEC_TYPE_ID = UUID.fromString("e9e90e0a-4f87-492e-afcc-2cd84057f226")
  private val STREAM_SPEC_TYPE_NAME = "Gestalt::Resource::Spec::StreamSpec"
  
  private val ENTITLEMENTS = Option(Seq(ResourceIds.Entitlement))
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {
    
    val owner = ResourceOwnerLink(ResourceIds.User, identity)
    
    val process = for {
      org <- ResourceFactory.findRootOrg.map(_.id)
      _ <- {
        acc push "Adding DataFeed Resource Type to /root/resourcetypes"
        addDataFeedType(org, owner)
      }
      _ <- {
        acc push "Adding StreamProvider Resource Type to /root/resourcetypes"
        addStreamProviderType(org, owner)
      }
      x <- {
        acc push "Adding StreamSpec Resource Type to /root/resourcetypes"
        addStreamSpecType(org, owner)
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
  
  def addDataFeedType(org: UUID, owner: ResourceOwnerLink) = Try {
    
    SystemType(org, owner,
      typeId      = DATA_FEED_TYPE_ID, 
      typeName    = DATA_FEED_TYPE_NAME,
      desc        = Some("Configuration for arbitrary data-feeds."),
      extend      = Some(ResourceIds.Configuration)
    
    ).withTypeProperties (
        TypeProperty("kind", "string", require = "required"),
        TypeProperty("provider", "json", require = "optional")
        
    ).withActionInfo (
      ActionInfo(
        prefix = "datafeed", 
        verbs  = Seq.empty)
        
    ).withLineageInfo (    
       LineageInfo(
           parent_types = Seq(ResourceIds.Environment),
           child_types  = ENTITLEMENTS)
           
    ).withApiInfo (
          TypeApiInfo(rest_name = "datafeeds")
    
    ).save()
    
    TypeFactory.findById(DATA_FEED_TYPE_ID) getOrElse {
      new RuntimeException("Failed creating DataFeed Resource Type")
    }
  }
  
  
  def addStreamProviderType(org: UUID, owner: ResourceOwnerLink) = Try {
    SystemType(org, owner,
      typeId      = STREAM_PROVIDER_TYPE_ID, 
      typeName    = STREAM_PROVIDER_TYPE_NAME,
      desc        = Some("Implements Gestalt data streams."),
      extend      = Some(ResourceIds.Provider)

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
    
    TypeFactory.findById(STREAM_PROVIDER_TYPE_ID) getOrElse {
      new RuntimeException("Failed creating StreamProvider Resource Type")
    }    
  }
  
  def addStreamSpecType(org: UUID, owner: ResourceOwnerLink) = Try {
    SystemType(org, owner,
      typeId      = STREAM_SPEC_TYPE_ID, 
      typeName    = STREAM_SPEC_TYPE_NAME,
      desc        = Some("Gestalt Stream Specification."),
      extend      = Some(ResourceIds.Configuration),
      selfProps   = Map("is_provider_backed" -> "true")
    
    ).withTypeProperties (    
    
      TypeProperty("cpus", "float", require = "required"),
      TypeProperty("mem", "int", require = "required"),
      TypeProperty("parallelization", "int", require = "required"),
      TypeProperty("processor", "json", require = "required"),
    	TypeProperty("streams", "json::list", require = "required"),
  		TypeProperty("persistence_ids", "uuid::list", require = "required"),
  		TypeProperty("lambda_provider", "json", require = "required"),
  		TypeProperty("laser_url", "string", require = "required"),
      TypeProperty("provider", "resource::uuid::link", require = "required", 
            refersTo = Some(STREAM_PROVIDER_TYPE_ID))
   
    ).withActionInfo (
      ActionInfo(
        prefix = "streamspec", 
        verbs  = Seq("start", "stop", "restart"))
        
    ).withLineageInfo (    
       LineageInfo(
           parent_types = Seq(ResourceIds.Environment),
           child_types  = ENTITLEMENTS)
           
    ).withApiInfo (
          TypeApiInfo(rest_name = "streamspecs")
    
    ).save()
   
    TypeFactory.findById(STREAM_SPEC_TYPE_ID) getOrElse {
      new RuntimeException("Failed creating StreamSpec Resource Type")
    }    
    
  }
  
}