package com.galacticfog.gestalt.meta.policy


import play.api.Logger
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data.models._

import scala.util.Try

import java.util.UUID
import play.api.libs.json._
import com.galacticfog.gestalt.json.Js

trait PolicyResource[A] {
    private val log = Logger(this.getClass)
    
    protected val properties: Map[String,String]
    
    def getValue(res: A, propertyName: String): Try[String]
    
    def exists(propertyName: String): Boolean = {
      properties.keySet contains propertyName
    }
    
}

trait ResourceProperties {
  
    private val log = Logger(this.getClass)
    
    protected val properties: Map[String,String]
    
    def getValue(res: ResourceLike, propertyName: String): Try[String]
    
    def exists(propertyName: String): Boolean = {
      properties.keySet contains propertyName
    }
    
    //def compareJson
    
    def compare[T](test: T, predicate: Predicate[T]): Boolean = {
      
      def jstring(js: T) = Json.parse(js.toString).as[String]
      def jsint(js: Int) = JsNumber(js).as[String].toInt
      
      val property = predicate.property
      val selector = stripProperty(property)
      
      if (!properties.contains( selector )) 
        throw new IllegalArgumentException(s"Unknown property: '$property'")
      else {
        
        val b = predicate.value
        val op = predicate.operator
        val datatype = properties( selector )
        
        datatype match {
          case "string" => CompareString.compare(test.toString, b.asInstanceOf[JsValue].as[String], op)
          case "int"    => CompareInt.compare(test.toString.toInt, jstring(b).toInt, op)
          case "float"  => CompareFloat.compare(test.toString.toDouble, jstring(b).toDouble, op)
          case "string->string::list" => CompareSingleToList().compare(csv2seq(jstring(b)), test.toString, op)
          case "string::list->string::list" => CompareListToList().compare(csv2seq(jstring(b)), toSeqString(test.toString), op)
          case _ => throw new IllegalArgumentException(s"Unsupported datatype: '$datatype'")
        }
      }
    }
    
    def csv2seq(csv: String) = {
      csv.split(",").map(_.trim).toList
    }
    
    def toSeqString(s: String): Seq[String] = {
      log.debug(s"ResourceProperties.toSeq($s)")
      
      Json.parse(s).validate[Seq[String]].map {
        case v: Seq[String] => v
      }.recoverTotal { e =>
        val msg = "Failed parsing Predicate value to Seq[String]: " + Js.errorString(e)
        log.error(msg)
        throw new BadRequestException(msg)
      }
    }
    
    def getProperty(r: ResourceLike, property: String): Option[String] = {
      r.properties flatMap { _.get(property) }
    }    
    
    def getOrEmpty(r: ResourceLike, property: String): String = {
      r.properties flatMap { _.get(property) } getOrElse EmptyProperty
    }
    def getOrEmptySeq(r: ResourceLike, property: String): String = {
      r.properties flatMap { _.get(property) } getOrElse EmptyPropertySeq
    }
    protected[policy] val baseResourceProperties = Map(
        "id"    -> "string", 
        "name"  -> "string", 
        "type"  -> "string", 
        "org"   -> "string",
        "owner" -> "string",
        "state" -> "string")
        
    protected[policy] def matchBaseProperty(res: ResourceLike): PartialFunction[String, String] = {
      case a if a == "id"    => res.id.toString
      case b if b == "name"  => res.name
      case c if c == "type"  => res.typeId.toString
      case d if d == "org"   => res.orgId.toString
      case e if e == "owner" => res.owner.toString
    }
  
    protected[policy] def invalidProperty: PartialFunction[String, String] = {
      case e => throw new BadRequestException(s"Unknown property: $e")
    }
    
    private[policy] def stripProperty(property: String) = 
      property.drop(1+property.lastIndexOf("."))
      
  }
  
  

  /*
   * TODO: This needs to be ResourceContainerProperties - should apply to Org, Workspace, and Environment
   * Difference is how to find things in the different scopes, i.e. containers.
   * 
   * Environment pulls all child containers
   * Workspace pulls all environments, then containers
   * Org pulls all child orgs, then each workspace in each org, then each environment in each workspace.
   * 
   */
  object DefaultResourceProperties extends ResourceProperties {
    
    private val log = Logger(this.getClass)
    
    override val properties = baseResourceProperties
    
    def getValue(res: ResourceLike, propertyName: String): Try[String] = Try {
      log.debug("Getting value of property: " + propertyName)
      matchBaseProperty(res)(stripProperty(propertyName))
    }
  }
  
  object DefaultResourceProperties2 extends ResourceProperties {
    
    private val log = Logger(this.getClass)
    
    override val properties = baseResourceProperties
    
    def getValue(res: ResourceLike, propertyName: String): Try[String] = Try {

      log.debug("Getting value of property: " + propertyName)
      matchBaseProperty(res)(stripProperty(propertyName))
    }
  }  
  
  object EnvironmentProperties extends ResourceProperties {
    
    override val properties = baseResourceProperties ++ Map( 
      "containers.count"         -> "int",
      "container.name"           -> "string",
      "container.cpus"           -> "float",
      "container.memory"         -> "int",
      "container.numInstances"   -> "int",
      "container.image"          -> "string->string::list",
      "container.user"           -> "string->string::list",
      "container.acceptedResourceRoles" -> "string::list->string::list", 
      "container.labels"        -> "string::list->string::list",
      "container.constraints"   -> "string::list->string::list",
      "container.network"       -> "string->string::list"
    )
    
    def getValue(res: ResourceLike, propertyName: String): Try[String] = Try {
      val getprop = {
        matchBaseProperty(res)    orElse 
        matchSpecialProperty(res) orElse 
        invalidProperty
      }
      getprop(propertyName)
    }
    
    def matchSpecialProperty(res: ResourceLike): PartialFunction[String, String] = {
      case a if a == "containers.count" => {
        /*
         * TODO: containers.count presents an odd case when used in limit policies for create/scale. When its current value
         * is at the limit, and the operator is '<=', the action will be allowed because we are still within parameters.
         * Actually creating the new container will increment the count past the allowed limit. So as it stands, the limit
         * policy will always allow one more container than set in the rule.
         */
        containerCount(res.id).toString
      }
      case a if a == "container.name"         => res.name
      case a if a == "container.user"         => getOrEmpty(res, "user")
      case a if a == "container.cpus"         => getOrEmpty(res, "cpus")
      case a if a == "container.memory"       => getOrEmpty(res, "memory")             
      case a if a == "container.image"        => getOrEmpty(res, "image")
      case a if a == "container.numInstances" => getOrEmpty(res, "num_instances")
      case a if a == "container.labels"       => getOrEmptySeq(res, "labels")
      case a if a == "container.constraints"  => getOrEmptySeq(res, "constraints")
      case a if a == "container.acceptedResourceRoles" => getOrEmptySeq(res, "acceptedResourceRoles")
      case a if a == "container.network"  => getOrEmpty(res, "network")
    }

    def containerCount[T](env: UUID) = {
      ResourceFactory.findChildrenOfType(ResourceIds.Container, env).size.asInstanceOf[T]
    }
  }
  
  