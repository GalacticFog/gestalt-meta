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

import com.galacticfog.gestalt.meta.auth._
import controllers.util.JsonInput

import controllers.util.TypeMethods

/*
 * Add DataFeed Resource Type
 * Add StreamProvider Resource Type
 * Add StreamSpec Resource Type
 */

class V7 extends MetaMigration with AuthorizationMethods {

  private val acc = new MessageAccumulator()
  
  private val DATA_FEED_TYPE_ID = UUID.fromString("8f875ffc-69ff-48c8-9d6d-f3622b7b1062")
  private val DATA_FEED_TYPE_NAME = "Gestalt::Resource::Configuration::DataFeed"
  
  private val STREAM_PROVIDER_TYPE_ID = UUID.fromString("b7f764be-9ae6-4f70-9c42-849cc881125f")
  private val STREAM_PROVIDER_TYPE_NAME = "Gestalt::Configuration::Provider::StreamProvider"
  
  private val STREAM_SPEC_TYPE_ID = UUID.fromString("e9e90e0a-4f87-492e-afcc-2cd84057f226")
  private val STREAM_SPEC_TYPE_NAME = "Gestalt::Resource::Spec::StreamSpec"
  
  private val ENTITLEMENTS = Option(Seq(ResourceIds.Entitlement))
  
  private val SYSTEM_WORKSPACE_NAME = "gestalt-system-workspace"
  private val SYSTEM_ENVIRONMENT_NAME = "gestalt-system-environment"
  
  
  def newWorkspace(rootId: UUID, creator: GestaltResourceInstance) = {
    for {
      wrk <- CreateNewResource(
          org = rootId,
          creator = creator,
          json = Json.obj("name" -> SYSTEM_WORKSPACE_NAME),
          typeId = Option(ResourceIds.Workspace),
          parent = Option(rootId))
      _ = setNewResourceEntitlements(rootId, wrk.id, creator, Some(rootId))
    } yield wrk
  }
  
  def newEnvironment(rootId: UUID, workspaceId: UUID, envType: UUID, creator: GestaltResourceInstance) = {
    for { 
      env <- CreateNewResource(
        org = rootId,
        creator = creator,
        json = Json.obj(
            "name" -> SYSTEM_ENVIRONMENT_NAME,
            "properties" -> Json.obj(
                "environment_type" -> envType)
            ),
        typeId = Option(ResourceIds.Environment),
        parent = Option(workspaceId))
      _ = setNewResourceEntitlements(rootId, env.id, creator, Some(workspaceId))
    } yield env
  }
  
  /**
   * Lookup or create the 'system' environment where the Stream Provider lambda will go.
   * /root/gestalt-system-workspace/gestalt-system-environment
   */
  private[migrations] def getLambdaEnvironment(
      rootId: UUID, creator: GestaltResourceInstance): Try[GestaltResourceInstance] = {
    for {
      w <- ResourceFactory.findChildByName(rootId, ResourceIds.Workspace, SYSTEM_WORKSPACE_NAME).fold {
        acc push "gestalt-system Workspace not found. Creating."
          newWorkspace(rootId, creator)
        
        }{ workspace => Try(workspace) }
        
      e <- ResourceFactory.findChildByName(w.id, ResourceIds.Environment, SYSTEM_ENVIRONMENT_NAME).fold {
        
        acc push "gestalt-system Environment not found. Creating."
        val envType = ReferenceFactory.findByName(ResourceIds.EnvironmentType, "production").get        
        newEnvironment(rootId, w.id, envType.id, creator)
        
      }{ environment => Try(environment) }
    } yield e
  }
  
  private[migrations] def createStreamProviderLambda(
      orgId: UUID, 
      envId: UUID, 
      creator: GestaltResourceInstance, 
      payload: JsValue): Try[GestaltResourceInstance] = {
    
    log.debug("****PAYLOAD : " + Json.prettyPrint(payload))
    
    val providerId = {
      val jsId = Js.find(payload.as[JsObject], "/lambda/properties/provider/id") getOrElse {
        throw new BadRequestException("Bad migration payload. Missing: 'V7/lambda/properties/provider/id'")
      }
      UUID.fromString(jsId.as[String])
    }
    
    val lambdaJson = {
      Js.find(payload.as[JsObject], "/lambda") getOrElse {
        throw new BadRequestException("Bad migration payload. Missing 'V7/lambda'.")
      }
    }
    
    log.debug("LAMBDA-JSON:\n" + Json.prettyPrint(lambdaJson))
    
    acc push s"Looking up given Lambda Provider: ${providerId}"
    
    // Ensure given Lambda Provider Exists
    ResourceFactory.findById(providerId) getOrElse {
      throw new BadRequestException(s"Lambda Provider with ID '${providerId}' not found.")
    }

    for {
      lam <- CreateNewResource(
          org = orgId,
          creator = creator,
          json = lambdaJson,
          typeId = Option(ResourceIds.Lambda),
          parent = Option(envId))
      _ = setNewResourceEntitlements(orgId, lam.id, creator, Some(envId))
    } yield lam
  }
  
  
  def migrate(identity: UUID, payload: Option[JsValue] = None): Either[JsValue,JsValue] = {

    /*
     * Need to create lambda before stream-provider
     */

    val process = if (payload.isEmpty) {
      
      Failure(new RuntimeException("Must provide payload!!!"))
        
    } else { 
        for {
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
            acc push "Adding DataFeed Resource Type to /root/resourcetypes"
            addDataFeedType(root.id, root)
          }
          env <- {
            acc push "Looking up environment for Stream Provider lambda"
            getLambdaEnvironment(root.id, creator)
          }
          _ <- {
            // Create lambda
            acc push "Creating StreamProvider Lambda"
            createStreamProviderLambda(root.id, env.id, creator, payload.get)
          }
          _ <- {
            acc push "Adding StreamProvider Resource Type to /root/resourcetypes"
            addStreamProviderType(root.id, creator)
          }
          x <- {
            acc push "Adding StreamSpec Resource Type to /root/resourcetypes"
            addStreamSpecType(root.id, creator)
          }
      } yield x
    }
    
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
  

  def addDataFeedType(org: UUID, creator: GestaltResourceInstance) = Try {
    val owner = ResourceOwnerLink(ResourceIds.User, creator.id)
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
    
    val newtype = TypeFactory.findById(DATA_FEED_TYPE_ID) getOrElse {
      throw new RuntimeException("Failed creating DataFeed Resource Type")
    }

    setTypeLineage(newtype, creator)
  }
  

  def addStreamProviderType(org: UUID, creator: GestaltResourceInstance) = Try {
    val owner = ResourceOwnerLink(ResourceIds.User, creator.id)
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
    
    val newtype = TypeFactory.findById(STREAM_PROVIDER_TYPE_ID) getOrElse {
      throw new RuntimeException("Failed creating StreamProvider Resource Type")
    }
    setTypeLineage(newtype, creator)
  }
  
  def addStreamSpecType(org: UUID, creator: GestaltResourceInstance) = Try {
    val owner = ResourceOwnerLink(ResourceIds.User, creator.id)
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
   
    val newtype = TypeFactory.findById(STREAM_SPEC_TYPE_ID) getOrElse {
      throw new RuntimeException("Failed creating StreamSpec Resource Type")
    }    
    setTypeLineage(newtype, creator)
  }
 
  
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