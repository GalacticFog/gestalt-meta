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

import services._

import controllers.util.JsonInput
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global

import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.patch._


object ProviderManager extends AuthorizationMethods with JsonInput {

  private[this] val log = Logger(this.getClass)
  
  private type ServiceList = Seq[Future[GestaltResourceInstance]]
  
  def getenv(pm: ProviderMap) = {
    
  }

  def loadAllProviders(): Map[ProviderMap,ServiceList] = {
    val ps = findEagerProviders()
    processProviders(ps, Map.empty)
  }
  
  def loadProvider(pm: ProviderMap): Map[ProviderMap,ServiceList] = {
    val ps = depthCollect(pm, Set.empty, 0).toList.reverse
    processProviders(ps, Map.empty)
  }
  
  private[providers] def processProviders(
      ps: Seq[ProviderMap], acc: Map[ProviderMap,ServiceList]): Map[ProviderMap,ServiceList] = {
    ps match {
      case Nil => acc
      case provider :: tail => {
        
        /*
         * The map value should probably be an Either[Throwable,ServiceList]
         */
        
        // Start any containers associated with this provider.
        val servicelist = processServices(provider, provider.services.toList, Seq.empty)
        processProviders(tail, Map(provider -> servicelist) ++ acc)
      }
    }
  }
  
  private[providers] def processServices(
      parent: ProviderMap, 
      ss: Seq[ProviderService], 
      acc: Seq[Future[GestaltResourceInstance]]): Seq[Future[GestaltResourceInstance]] = {
    
    ss match {
      case Nil => acc
      case service :: tail => {

        val caas = loadCaasProvider(service)
        /*
         * TODO: Check if there is already a service running for the current container.
         */
        val account = getUserAccount(parent)
        val environment = getOrCreateProviderEnvironment(parent.resource, account)
        val containers = ResourceFactory.findChildrenOfType(ResourceIds.Container, environment.id)
        val label = "%s, %s".format(parent.id, parent.name)
        
        val fcontainer = {
          if (containers.isEmpty) {
            log.info(s"No containers found for Provider [${label}]...launching...")
            launchContainer(parent, service, environment, caas)
          }
          else {
            log.info(s"Found running containers for Provider ${label}. Nothing to do.")
            
            Future(containers(0))
          }
        }
        processServices(parent, tail, fcontainer +: acc)
      }
    }
  }
  
  def isLaunched(pm: ProviderMap) = {
    
  }
  
  private[providers] def mergeContainerVars(spec: JsValue, vars: Map[String,String]): Try[JsValue] = {
    val cvars = Js.find(spec.as[JsObject], "/properties/env") map { env =>
      Js.parse[Map[String,String]](env) match {
        case Failure(e) => {
          log.error(s"Failed parsing [container.properties.env] to Map. found : ${env}")
          throw e
        }
        case Success(v) => v
      }
    } getOrElse Map.empty
    val merged = vars ++ cvars
    val patch = PatchDocument(PatchOp.Replace("/properties/env", Json.toJson(merged)))
    patch.applyPatch(spec.as[JsObject])
  }
  
  private[providers] def normalizeContainerPayload(spec: JsValue, providerVars: Map[String,String]) = {
    mergeContainerVars(withProviderInfo(spec), providerVars)match {
      case Failure(e) => 
        throw new UnprocessableEntityException(s"Invalid container_spec: " + e.getMessage)
      case Success(p) => p
    }
  }
  
  /**
   * 
   * @return the newly created container resource.
   */
  private[providers] def launchContainer(
      pm: ProviderMap, 
      service: ProviderService, 
      environment: GestaltResourceInstance,
      caas: GestaltResourceInstance): Future[GestaltResourceInstance] = {

    val fqon = getFqon(pm.org) getOrElse {
      throw new RuntimeException(s"Failed to lookup Provider Org FQON for ID '${pm.id}'")
    }
    
    val variables = getMergedEnvironment(pm)
    val payload = normalizeContainerPayload(service.container_spec, variables)     
    
    val uri = s"/${fqon}/providers/${pm.id}/environments/${environment.id}"
    
    val req = new FakeRequest(uri)
    val account = getUserAccount(pm)

    val transform = CaasTransform(pm.org, account, payload)
    val ctx = ProviderContext(req, caas.id, None)
    
    log.info(s"Creating container for provider '${pm.id}' in Meta...")
    val metaCreate = for {
      r1          <- Try(transform.resource)
      resource    <- createMetaContainer(account, r1, environment.id)
      serviceImpl <- getProviderImpl(caas.typeId)
    } yield (serviceImpl, resource)
    
    val created = metaCreate match {
      case Failure(e) => {
        log.error("Failed creating container in Meta.")
        throw e
      }
      case Success((service, metaResource)) => {
        
        log.info("Meta container created: " + metaResource.id)
        log.info("Creating container in backend CaaS...")
        
        for {
          updated   <- service.create(ctx, metaResource)
          container <- updateContainer(updated, account.account.id)
        } yield container
      }
    }
    created
  }

  
  def loadCaasProvider(service: ProviderService) = {
    val pid = ProviderService.providerId(service) getOrElse {
      throw new RuntimeException("Could not parse [container.properties.provider.id].")
    }
    
    ResourceFactory.findById(pid) getOrElse {
      throw new ResourceNotFoundException(s"CaaS Provider with ID '$pid' not found.")
    }    
  }
  
  def createMetaContainer(user: AuthAccountWithCreds, container: GestaltResourceInstance, env: UUID) = {
    ResourceFactory.create(ResourceIds.User, user.account.id)(container, Some(env))
  }
  
  private def updateContainer(container: GestaltResourceInstance, identity: UUID): Future[GestaltResourceInstance] = {
    ResourceFactory.update(container, identity) match {
      case Failure(e) => Future.failed(e)
      case Success(r) => Future(r)
    }
  }
  
  def getUserAccount(pm: ProviderMap) = {
    val owner = pm.resource.owner
    val userinfo = Map(
      "id" -> owner.id.toString,
      "username" -> (owner.name getOrElse "none")
    )
    val directoryinfo = Map("org" -> pm.org.toString)
    dummyAuthAccountWithCreds(userinfo, directoryinfo)
  }
  
  /**
   * Get FQON given an Org ID
   */
  def getFqon(org: UUID): Option[String] = {
    for {
      o <- ResourceFactory.findById(ResourceIds.Org, org)
      p <- o.properties
      f <- p.get("fqon")
    } yield f
  }  


  private[providers] def createProviderEnvironment(
      provider: GestaltResourceInstance, 
      account: AuthAccountWithCreds) = {
    
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
        validate = true) map { env =>  
      setNewEntitlements(org, env.id, account, Some(provider.id))
      env
    }
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
  
  def getOrCreateProviderEnvironment(
      provider: GestaltResourceInstance,
      account: AuthAccountWithCreds): GestaltResourceInstance = {
    
    getProviderEnvironment(provider.id) getOrElse {
      log.info(s"Creating Container-Environment for Provider '${provider.id}'.")
      
      createProviderEnvironment(provider, account) match {
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
    
    //issue right now is ProviderContext requires a play.api.mvc.Request!!!
    
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
  
  
  import com.galacticfog.gestalt.meta.api._
  
  
  /**
    * Ensure Container input JSON is well-formed and valid. Ensures that required properties
    * are given and fills in default values where appropriate.
    */
  private [this] def normalizeInputContainer(inputJson: JsValue): JsObject = {
    val defaults = containerWithDefaults(inputJson)
    val newprops = (Json.toJson(defaults).as[JsObject]) ++ (inputJson \ "properties").as[JsObject]
    (inputJson.as[JsObject] ++ Json.obj("resource_type" -> ResourceIds.Container.toString)) ++ Json.obj("properties" -> newprops)
  }
  
  def containerWithDefaults(json: JsValue) = {
    val ctype = (json \ "properties" \ "container_type").asOpt[String] match {
      case Some(t) if ! t.trim.isEmpty => t
      case _ => throw new IllegalArgumentException(s"'container_type' is missing or empty.")
    }
    val image = (json \ "properties" \ "image") match {
      case u: JsUndefined => throw new IllegalArgumentException(s"'image' is missing.")
      case v => v.as[String]
    }
    val prv   = (json \ "properties" \ "provider") match {
      case u: JsUndefined => throw new IllegalArgumentException(s"'provider' is missing.")
      case v => v.validate[ContainerSpec.InputProvider].map {
        case p: ContainerSpec.InputProvider => p
      }.recoverTotal { e =>
        throw new IllegalArgumentException("Invalid provider JSON: " + Js.errorString(e))
      }
    }
    ContainerSpec(
      name = "",
      container_type = ctype,
      image = image,
      provider = prv
    )
  }  
  
  trait ResourceTransform extends GestaltProviderService
  case class CaasTransform(org: UUID, caller: AuthAccountWithCreds, json: JsValue) extends ResourceTransform {
    lazy val resource = jsonToInput(org, caller, normalizeInputContainer(json))
    lazy val spec = ContainerSpec.fromResourceInstance(resource)
  }

  trait ServiceProvider[A <: GestaltProviderService] {
    /**
     * Get a GestaltService implementation.
     */
    def get[A](provider: UUID): Try[A]
  }  
  
} 