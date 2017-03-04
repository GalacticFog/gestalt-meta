package com.galacticfog.gestalt.meta.providers

import play.api.Logger
import play.api.libs.json._

import scala.util.{Try,Success,Failure}

import com.galacticfog.gestalt.json.Js
import play.api.libs.json.Reads._ // Custom validation helpers
import play.api.libs.functional.syntax._ // Combinator syntax
import java.util.UUID

import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.providers._


import com.galacticfog.gestalt.data.CoVariant
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.api.sdk.ResourceLabel

import com.galacticfog.gestalt.json._
import com.galacticfog.gestalt.meta.api.errors._

import services._
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink

object ProviderManager {

  private[this] val log = Logger(this.getClass)
  
  def getenv(pm: ProviderMap) = {
    
    //val flat = pm.flatten()
  
  }

  def loadAllProviders() = {
    val ps = findEagerProviders()
    
    /*
     * 
     * 
     * foreach provider, check its service environment
     * if exists
     * 		foreach service:
     * 		check if container is running
     * 		if container.isrunning:
     * 			return
     * 	  else
     * 			launch container
     * else
     * 		create environment
     * 		foreach service:
     * 			launch the container
     * 
     * 
     */
    
    
    def processServices(parent: GestaltResourceInstance, ss: Seq[ProviderService], acc: Seq[UUID]): Seq[UUID] = {
      ss match {
        case Nil => acc
        case h :: t => {
          
          // Lookup and load CaaS Provider resource.
          val pid = ProviderService.providerId(h) getOrElse {
            throw new RuntimeException("Could not parse [container.properties.provider.id].")
          }
          
          val caas = ResourceFactory.findById(pid) getOrElse {
            throw new ResourceNotFoundException(s"CaaS Provider with ID '$pid' not found.")
          }
          
          // Lookup and load/create the provider container-environment.
          val env = getOrCreateProviderEnvironment(parent)

          /*
           * Now launch the container.
           */
          
          acc
        }
      }
    }
    


    def go(ps: Seq[ProviderMap], acc: Seq[UUID]): Seq[UUID] = {
      ps match {
        case Nil => acc
        case h :: t => {          
          
          processServices(h.resource, h.services.toList, Seq.empty)
          go(t, acc)
        }
      }
    }
    go(ps, Seq.empty)
    /*
     * 1.) Get list of containers we *think* we have running (if any)
     */
  }
  
    import com.galacticfog.gestalt.data._
    import com.galacticfog.gestalt.meta.api.sdk._
    
    def createProviderEnvironment(provider: GestaltResourceInstance) = {
      val org = provider.orgId
      
      val props = Map(
          "environment_type" -> EnvironmentType.id("other").toString)
          
      val resource = GestaltResourceInstance(
          id = UUID.randomUUID(),
          typeId = ResourceIds.Environment,
          orgId = org,
          owner = provider.owner,
          name = "services",
          description = Some(""),
          state = ResourceState.id(ResourceStates.Active),
          properties = Some(props))
      
      val owner = provider.owner
      
      ResourceFactory.create(owner.typeId, owner.id)(
          resource,
          parentId = Some(provider.id), 
          validate = true)
    }
    
  /**
   * Return a list of the containers
   */
  def getProviderContainers(provider: UUID, environment: Option[UUID] = None): Seq[GestaltResourceInstance] = {
    val envid = environment orElse getProviderEnvironment(provider).map(_.id)
    envid.fold(Seq[GestaltResourceInstance]()) { eid =>
      ResourceFactory.findChildrenOfType(ResourceIds.Container, eid)
    }
  }
  
  def getOrCreateProviderEnvironment(provider: GestaltResourceInstance): GestaltResourceInstance = {
    getProviderEnvironment(provider.id) getOrElse {
      log.info(s"Creating Container-Environment for Provider '${provider.id}'.")
      
      createProviderEnvironment(provider) match {
        case Failure(e) => {
          log.error(s"Failed creating Environment for Provider '${provider.id}'")
          throw e
        }
        case Success(environment) => environment
      }
    }
  }
  
  /**
   * Get the environment where the provider's service containers are located.
   */
  def getProviderEnvironment(provider: UUID): Option[GestaltResourceInstance] = {
    ResourceFactory.findChildByName(provider, ResourceIds.Environment, "services")
  }
  
  /**
   * Launch eager providers
   */
  def launchProvider(pm: ProviderMap, environment: UUID) = {
    
    /* 
     * 1.) Parse the 'provider' from the container_spec.
     * 2.) Use that to get the ContainerService implementation
     * 3.) launch the container
     *
     * A challenge here is determining if a service is already started. This function will
     * start the containers for the the entire graph of providers represented by the given
     * ProviderMap. The given provider *may* be a dependency of some other provider, so it
     * doesn't need to be started when we're given that provider as argument. How do we know?
     * 
     * We can create a 'status' property that indicates the running status of the provider's containers.
     * We can look for the container in the provider /env to determine its status.
     * 
     * ??? Which of these is the right strategy ???  
     */
    
  }
  
  def getProviderImpl(typeId: UUID): Try[CaasService] = Try {
    typeId match {
      case ResourceIds.CaasProvider     => new KubernetesService(typeId)
      case ResourceIds.MarathonProvider => new MarathonService()
      case _ => throw BadRequestException(s"No implementation for provider type '$typeId' was found.")
    }
  }  
  
  def withProviderInfo(json: JsValue) = {    
    val jprops = (json \ "properties")
    
    val pid = (jprops \ "provider" \ "id").as[String]

    val p = ResourceFactory.findById(UUID.fromString(pid)).get
    val newprops = (jprops.as[JsObject] ++ Json.obj("provider" -> Json.obj("name" -> p.name, "id" -> p.id)))
    
    json.as[JsObject] ++ Json.obj("properties" -> newprops)
  }
  
  /**
   * Parse the provider ID from container.properties
   */
  def parseProvider(c: GestaltResourceInstance): UUID = {
    UUID.fromString((Json.parse(c.properties.get("provider")) \ "id").as[String])
  }  
  
  /**
   * Get a list of all providers that have an associated service (container) that
   * requires eager initialization.
   */
  def findEagerProviders() = {
    val providers = ResourceFactory.findAllOfType(CoVariant(ResourceIds.Provider))
    providers collect { case p if isEager(p) => ProviderMap(p) }
  }
  
  /**
   * Boolean indicating if the given provider resource requires a
   * container.
   */
  def requiresService(r: GestaltResourceInstance): Boolean = {
    val svcs = for {
      p <- r.properties
      s <- p.get("services")
    } yield s
    svcs.isDefined
  }
  
  /**
   * Determine if a provider resource has an associated service, and
   * whether that service is eagerly initialized.
   */
  def isEager(r: GestaltResourceInstance): Boolean = {
    if (requiresService(r)) {
      val prs = ProviderService.fromResource(r)
      prs exists { s =>
        val bind = s.init.binding.getOrElse("eager") 
        bind == "eager" }
    } else false
  }
  
  /**
   * Get a Map of environment variables for the given ProviderMap. The
   * variables are merged with linked dependencies with public and private merged.
   */
  def getMergedEnvironment(pm: ProviderMap): Map[String, String] = {
    val allmaps = depthCollect(pm, Set.empty, 0)
    val dependencyOrder = allmaps.toList.sortBy(_.idx).reverse
    val envs = mapLinkedEnvironments(dependencyOrder)

    flattenEnv(envs(pm.id))
  }
  
  /**
   * Flatten the public and private environment maps into a single Map
   */
  def flattenEnv(penv: ProviderEnv): Map[String, String] = {
    def getmap(m: Option[Map[String,String]]) = m getOrElse Map.empty
    getmap(penv.public) ++ getmap(penv.privatev)
  }
  
  /**
   * Perform a depth-first search from the given ProviderMap marking each 
   * dependency ProviderMap with an index (indicating depth).
   */
  private[providers] def depthCollect(current: ProviderMap, acc: Set[ProviderMap], idx: Int): Set[ProviderMap] = {
    val target = current.copy(idx = idx)
    target.dependencies.foldLeft(acc) { (results, next) =>
      if (results.contains(next)) results
      else depthCollect(next, results + target, idx + 1)
    } + target
  }
  
  /**
   * Create a map of provider envs with the public variables from any links merged
   * into the private variables of the 'linker'
   */
  private[providers] def mapLinkedEnvironments(ps: Seq[ProviderMap]): Map[UUID, ProviderEnv] = {
    def mergevars(provider: ProviderMap): Map[UUID, Option[ProviderEnv]] = {    
      val tups = 
        if (provider.dependencies.isEmpty) Seq((provider.id, provider.envConfig))      
        else {
          provider.dependencies map { link =>
            
            // Lookup provider corresponding to current link to get environment.
            val targetLink = select(link.id, ps).get
            
            // Extract public vars from link.env and merge into current provider privatevars.
            val newenv = targetLink.envConfig map { providerenv =>
              
              val oldenvconfig = provider.envConfig getOrElse ProviderEnv(None,None)
              val oldprivate = oldenvconfig.privatev getOrElse Map.empty
              val newpublic = providerenv.public map { vars =>
                vars map { case (k, v) => 
                  ("%s_%s".format(provider.prefixMap(link.id), k), v)
                } 
              } getOrElse Map[String,String]()
              
              oldenvconfig.copy(privatev = Some(oldprivate ++ newpublic))
            }
            (provider.id, newenv)
          }
      }
      tups.toMap
    }
    
    (ps map { provider =>
      val m = mergevars(provider)
      (provider.id, m(provider.id).get)
    }).toMap
  }
  
  def select(id: UUID, ps: Seq[ProviderMap]) = ps.filter(_.id == id).headOption
  
} 