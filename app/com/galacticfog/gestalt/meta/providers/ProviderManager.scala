package com.galacticfog.gestalt.meta.providers


import play.api.Logger
import play.api.libs.json._

import scala.util.{Failure, Success, Try}
import com.galacticfog.gestalt.json.Js
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import java.util.UUID
import javax.inject.{Inject, Singleton}

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
//import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.patch._
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.ContainerSpec.PortMapping

import com.galacticfog.gestalt.meta.api._


@Singleton
class ProviderManager @Inject() ( kubernetesService: KubernetesService,
                                  marathonService: MarathonService ) extends AuthorizationMethods with JsonInput {
  
  private[this] val log = Logger(this.getClass)
  
  private type ServiceList = Seq[Future[GestaltResourceInstance]]
  
  def loadProviders(root: Option[ProviderMap] = None): Future[Seq[(ProviderMap,Seq[GestaltResourceInstance])]] = {
    val ps = {
      if (root.isEmpty) findEagerProviders()
      else depthCollect(root.get, Set.empty, 0).toList.reverse
    }
    processAllProviders(ps)
  }
  
  private[providers] def processAllProviders(ps: Seq[ProviderMap]) = {
    log.debug("Entered procAllProviders(_)...")
    Future.sequence(ps map { p => processProvider(p) })
  }
  
  private[providers] def processProvider(p: ProviderMap): Future[(ProviderMap,Seq[GestaltResourceInstance])] = {
    log.debug("Entered procProvider(_)...")
    val servicelist = processServices(p, p.services.toList, Seq.empty)
    for {
      vars <- mapPorts(servicelist)
      env = vars.toMap ++ getMergedEnvironment(p)
      np  = updatePublicEnv(p, env)
      sl   <- Future.sequence(servicelist)
    } yield (np -> sl)
  }
  
  /**
   * Add the values in newVars to the given ProviderMap's envConfig.public
   */
  import scala.collection.immutable.ListMap
  private[providers] def updatePublicEnv(p: ProviderMap, newVars: Map[String, String]): ProviderMap = {
    log.debug("Entered updatePublicEnv(_,_)...")
    val penv = p.envConfig map { env =>
      val sorted = env.public map { pub => 
        ListMap((newVars.toMap ++ pub).toSeq.sortBy(_._1):_*)
      }
      env.copy(public = sorted /*env.public map { pub => newVars.toMap ++ pub }*/)
    }
    p.copy(env = penv)
  }
  
  private[providers] def mapPorts(ss: Seq[Future[GestaltResourceInstance]]): Future[Seq[(String, String)]] = {
    log.debug("Entered mapPorts(_)...")
    Future.sequence(ss) map { services =>
      val t = services map { s =>
        parsePortMappings(s) filter { _.expose_endpoint == Some(true) } flatMap {  
          portMappingToVariables(_)
        }
      }
      t.flatten
    }
  }
  
  /*
   * create method 'usesVhost' if true, create the vhost provider variables.
   */
  
  /**
   * 
   */
  private[providers] def portMappingToVariables(
      mapping: ContainerSpec.PortMapping): Seq[(String,String)] = {
    
    log.debug("Entered portMappingToVariables(_)...")
    log.debug("PortMapping => " + mapping)
    
    /*
     * port_mapping.name becomes the name of the variable. This function
     * should be used to ensure the name is a valid variable name.
     */
    def trMappingName(mn: String): String = {
      mn.trim.replaceAll("-","_").toUpperCase
    }
    
    val basename = trMappingName(mapping.name.get)
    val protocol = mapping.protocol
    val host = mapping.service_address.get.host
    val port = mapping.service_address.get.port
    
    def varname(s: String) = "%s_%s".format(basename, s)

    val newvars = Seq(
      varname("PROTOCOL") -> protocol,
      varname("HOST") -> host,
      varname("PORT") -> port.toString)
    
    val vhostVars = mapping.virtual_hosts.fold(Map[String,String]()) { vs =>
      getvhostvars(basename, vs, Map.empty, 0)
    }
    newvars ++ vhostVars
  }
  
  def mkvhostvar(prefix: String, host: String, index: Int) = {
    val key = "%s_VHOST_%d".format(prefix, index)
    Map(key -> host)
  }
  
  def getvhostvars(prefix: String, hosts: Seq[String], acc: Map[String,String], index: Int): Map[String,String] = {
    val out = hosts match {
      case Nil => acc
      case h :: t => {
        getvhostvars(prefix, t, mkvhostvar(prefix, h, index) ++ acc, index+1)
      }
    }
    out
  }  
  
  /**
   * Parse the port_mappings from a Meta container resourcce to PortMapping objects.
   * @param r a Meta container resource
   */
  private[providers] def parsePortMappings(r: GestaltResourceInstance): Seq[PortMapping] = {
    log.debug("Entered parsePortMappings(_)...")
    r.properties.get.get("port_mappings") map { pmstr =>
      Js.parse[Seq[PortMapping]](Json.parse(pmstr)) match {
        case Success(pm) => pm
        case Failure(e) => {
          log.error("Failed parsing PortMappings from Resource." + e.getMessage)
          throw e
        }
      }
    } getOrElse Seq.empty
  }
  
  private[providers] def processServices(
      parent: ProviderMap, 
      ss: Seq[ProviderService], 
      acc: Seq[Future[GestaltResourceInstance]]): Seq[Future[GestaltResourceInstance]] = {
    
    ss match {
      case Nil => acc
      case service :: tail => {
        
        val caas = loadCaasProvider(service)

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
  
  /**
   * 
   */
  private[providers] def normalizeContainerPayload(spec: JsValue, providerVars: Map[String,String]) = {
    mergeContainerVars(withProviderInfo(spec), providerVars) match {
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
        log.info("Setting entitlements on new container.")
        setNewEntitlements(pm.org, environment.id, account, Some(pm.id))
        
        log.info("Creating container in backend CaaS...")
        for {
          updated   <- service.create(ctx, metaResource)
          container <- {
            println("***UPDATE LABELS: " + updated.properties.get.get("labels"))
            updateContainer(updated, account.account.id)
          }
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
    log.debug(s"Entered updateContainer(_,_)...")
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
    val env = mapLinkedEnvironments(pm)
    env.fold(Map[String,String]())(_.flatten)
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
  private[providers] def mapLinkedEnvironments(provider: ProviderMap): Option[ProviderEnv] = {

    if (provider.dependencies.isEmpty) provider.envConfig
    else {

      def go(mps: Seq[ProviderMap], acc: ProviderEnv): ProviderEnv = {
        mps match {
          case Nil => acc
          case link :: tail => {

            // Lookup provider corresponding to current link to get environment.
            val targetLink = select(link.id, mps).get

            targetLink.envConfig match {
              case None => go(tail, acc)
              case Some(providerenv) => {
                val oldprivate = acc.privatev getOrElse Map.empty
                val newpublic = providerenv.public map { vars =>
                  vars map {
                    case (k, v) =>
                      ("%s_%s".format(provider.prefixMap(link.id), k), v)
                  }
                } getOrElse Map[String, String]()

                val nenv = acc.copy(privatev = Some(oldprivate ++ newpublic))
                go(tail, nenv)
              }
            }
          }
        }
      }

      // These are all the linked provider public variables named with the correct prefix.
      val newenvironment = go(provider.dependencies, ProviderEnv(None, None))

      // These are the {prefix}_META_ID variables with the ID of each linked provider.
      val linkIdVariables = provider.dependencies.foldLeft(Map[String, String]()) { (results, current) =>
        val id = current.id
        val prefix = provider.prefixMap(id)
        Map((prefix + "_META_ID") -> id.toString) ++ results
      }

      // Merge in linked provider public variables
      val merged = ProviderEnv.merge(provider.envConfig.get, newenvironment)

      // Merge in linked provider ID variables and return
      Option(ProviderEnv.merge(merged, ProviderEnv(None, privatev = Some(linkIdVariables))))
    }
  }  

  /**
   * Find a ProviderMap by ID in a Seq[ProviderMap]
   */
  private[providers] def select(id: UUID, ps: Seq[ProviderMap]) = ps.filter(_.id == id).headOption

  def getProviderImpl(typeId: UUID): Try[CaasService] = Try {
    typeId match {
      case ResourceIds.KubeProvider     => kubernetesService
      case ResourceIds.DcosProvider => marathonService
      case _ => throw BadRequestException(s"No implementation for provider type '$typeId' was found.")
    }
  }
  
  /*
   * TODO: Everything below is 'container-centric' - should be in Container services somewhere.
   */
  def withProviderInfo(json: JsValue): JsObject = {    

    val oldprops = Js.find(json.as[JsObject], "/properties") getOrElse {
      throw new UnprocessableEntityException(s"Invalid container JSON. No propertites found.")
    }
    Js.find(json.as[JsObject], "/properties/provider/id").fold {
      throw new UnprocessableEntityException(s"Invalid provider JSON. [properties.provider.id] not found.")
    }{ pid =>
      parseUUID(pid.as[String]).fold {
        throw new UnprocessableEntityException(s"Invalid provider ID (not a GUID). found: '$pid'")
      }{ uid =>
        ResourceFactory.findById(uid).fold {
          throw new UnprocessableEntityException(
              s"Container Provider not found. Value from [properties.provider.id]: $pid")
        }{ provider =>
          // Plug provider values into json object and add to properties.
          val pobj = Json.obj("provider" -> Json.obj("id" -> provider.id, "name" -> provider.name)) 
          json.as[JsObject] ++ Json.obj("properties" -> (oldprops.as[JsObject] ++ pobj))
        }
      }
    }
  }
  
  /**
   * Parse the provider ID from container.properties
   */
  def parseProvider(c: GestaltResourceInstance): UUID = {
    UUID.fromString((Json.parse(c.properties.get("provider")) \ "id").as[String])
  }
  
  /**
    * Ensure Container input JSON is well-formed and valid. Ensures that required properties
    * are given and fills in default values where appropriate.
    */
  private [this] def normalizeInputContainer(inputJson: JsValue): JsObject = {
    val defaults = containerWithDefaults(inputJson)
    val newprops = (Json.toJson(defaults).as[JsObject]) ++ (inputJson \ "properties").as[JsObject]
    (inputJson.as[JsObject] ++ Json.obj("resource_type" -> ResourceIds.Container.toString)) ++ Json.obj("properties" -> newprops)
  }
  
  /**
   * Ensure the given container JSON contains required properties, using defaults
   * where possible.
   */
  private def containerWithDefaults(json: JsValue) = {

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