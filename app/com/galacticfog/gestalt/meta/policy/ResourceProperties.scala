package com.galacticfog.gestalt.meta.policy


import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data.models._

import scala.util.{Try,Success,Failure}

import java.util.UUID


trait ResourceProperties {
    
    protected val properties: Map[String,String]

    def getValue(res: ResourceLike, propertyName: String): Try[String]
    
    def exists(propertyName: String): Boolean = {
      properties.keySet contains propertyName
    }
    
    def compare[T](test: T, predicate: Predicate[T]): Boolean = {
      val property = predicate.property
      if (!properties.contains(property)) 
        throw new IllegalArgumentException(s"Unknown property: '$property'")
      else {
        
        val b = predicate.value
        val op = predicate.operator
        val datatype = properties(predicate.property)
        
        datatype match {
          case "string" => CompareString.compare(test.toString, b.toString, op)
          case "int"    => CompareInt.compare(test.toString.toInt, b.toString.toInt, op)
          case _ => throw new IllegalArgumentException(s"Unsupported datatype: '$datatype'")
        }
      }
    }
    
    protected val baseResourceProperties = Map(
        "id"    -> "string", 
        "name"  -> "string", 
        "type"  -> "string", 
        "org"   -> "string",
        "owner" -> "string",
        "state" -> "string")
        
    protected def matchBaseProperty(res: ResourceLike): PartialFunction[String, String] = {
      case a if a == "id"    => res.id.toString
      case b if b == "name"  => res.name
      case c if c == "type"  => res.typeId.toString
      case d if d == "org"   => res.orgId.toString
      case e if e == "owner" => res.owner.toString
    }
  
    protected def invalidProperty: PartialFunction[String, String] = {
      case e => throw new IllegalArgumentException(s"Unknown property: $e")
    }
    
  }
  
  
  object EnvironmentProperties extends ResourceProperties {
    
    override val properties = baseResourceProperties ++ Map( 
      "containers.count" -> "int"    
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
    }

    def containerCount[T](env: UUID) = {
      ResourceFactory.findChildrenOfType(ResourceIds.Container, env).size.asInstanceOf[T]
    }
  }