package controllers.util

import play.api.http.HeaderNames
import play.api.libs.concurrent.Execution.Implicits.defaultContext

import scala.concurrent.Future
import controllers.util.db._
import play.api.Logger

import scala.util.{Failure, Success}
import scalikejdbc._
import com.galacticfog.gestalt.data._

import scala.util.Try
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import java.util.UUID

import com.galacticfog.gestalt.data.models._
import play.api.mvc.{Request, RequestHeader, Result}
import com.galacticfog.gestalt.meta.api.errors._
import play.api.libs.json._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.output._
import controllers.SecurityResources
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.Resource
import com.galacticfog.gestalt.patch._
import controllers.util.JsonUtil._
import com.galacticfog.gestalt.meta.auth.AuthorizationMethods
import com.galacticfog.gestalt.meta.auth.ActionMethods
import play.api.mvc.Result

trait PolicyMethods {
  
  /**
   * Injects 'parent' link into Policy resource JSON
   */
  def transformPolicy(json: JsValue, data: Map[String, String]): Try[JsValue]  = Try {
    val parentId = {
      if (!data.contains("parent")) 
        throw new RuntimeException(s"`parent` key is missing from data Map. This is a bug.")
      else UUID.fromString(data("parent"))
    }
    
    val link = Json.toJson {
      parentLink(parentId, None).getOrElse {
        throw new BadRequestException(s"Given parent resource with ID '$parentId' does not exist.")
      }
    }
    JsonUtil.withJsonPropValue(json.as[JsObject], "parent", link)
  }
  
  /**
   * Set /resource_type, /properties/parent, and /properties/defined_at
   * values on Rule JSON.
   */
  def transformRule(json3: JsValue, data: Map[String, String]): Try[JsValue] = Try {
    
    val parentId = data.get("parent").getOrElse {
      throw new RuntimeException("Could not find `parent` value in data-map. This is a bug")
    }
    
    val typeId = data.get("typeId").getOrElse {
      throw new RuntimeException("Could not find `typeId` value in data-map. This is a bug.")
    }
    
    // set `/resource_type`
    val json = json3.as[JsObject] ++ Json.obj("resource_type" -> JsString(typeId))
    
    // set `/properties/parent` 
    val json2 = JsonUtil.withJsonPropValue(json, "parent", JsString(parentId))
    
    // set `/properties/defined_at
    val definedAt = ResourceFactory.findById(ResourceIds.Policy, UUID.fromString(parentId)).fold {
      throw new RuntimeException("Could not find parent policy.")
    }{ p =>
      Json.parse(p.properties.get("parent"))
    }
    JsonUtil.withJsonPropValue(json2, "defined_at", definedAt)
  }

  /* ************************************************************************************
   * TODO: Note these next few methods for translating from strings to the rule-type IDs
   * are hacky and not reliable. The name to UUID function should query for sub-types of
   * ::Rule, and we need to add a `short_name` property to the ::Rule schema if those 
   * lookups are ever going to be reliable (i.e. "limit" or "event" to UUID)
   */
  
  private lazy val RULE_TYPE_NAME = Resources.Rule
  
  /**
   * Translate a Rule-type short name (i.e. limit, event) into the corresponding Type UUID
   */
  def resolveRuleTypeFromString(json: JsValue): Try[UUID] = Try {
    Js.find(json.as[JsObject], "/resource_type").fold {
      throw new BadRequestException("You must provide a resource_type")
    } { tpe =>
      ruleTypeId(expandRuleTypeName(tpe.as[String])).get
    }
  }
  
  /**
   * Create a fully-qualified Rule type name from it's simple name, i.e.,
   * 'event' becomes 'Gestalt::Resource::Rule::Event'
   */ 
   protected[controllers] def expandRuleTypeName(shortName: String) = {
    if (Seq("config", "event", "limit").contains(shortName.trim.toLowerCase)) {
      "%s::%s".format(RULE_TYPE_NAME, shortName.trim.capitalize)
    } else throw new BadRequestException(s"Invalid Rule type - found: '$shortName'")
  }
  
  /**
   * Get Rule type ID from its fully-qualified name.
   */
  protected [controllers] def ruleTypeId(typeName: String) = {
    typeName match {
      case a if a == Resources.RuleEvent  => Some(ResourceIds.RuleEvent)
      case b if b == Resources.RuleConfig => Some(ResourceIds.RuleConfig)
      case c if c == Resources.RuleLimit  => Some(ResourceIds.RuleLimit)
      case _ => None
    }
  }  
  
  protected [controllers] def parentLink(pid: UUID, baseUri: Option[String]): Option[ResourceLink] = {
    ResourceFactory.findById(pid) map { toLink( _, baseUri ) }
  }
}