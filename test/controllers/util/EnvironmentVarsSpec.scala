package controllers.util


import java.util.UUID

import com.galacticfog.gestalt.data.bootstrap.Bootstrap
import controllers.util.db.ConnectionManager
import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Scope
import play.api.libs.json._

import com.galacticfog.gestalt.meta.test.ResourceScope
import com.galacticfog.gestalt.meta.api.Resource

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import scala.util.Success


class EnvironmentVarsSpec extends Specification with ResourceScope with BeforeAll {
  
  override def beforeAll(): Unit = pristineDatabase()

  "safeAdd" should {
    
    "add the given key/value to the map if it does not already exist." in {
      val m1:Map[String,String] = Map()
      val m2 = EnvironmentVars.safeAdd(m1, "foo", "bar")
      
      m2.size === 1
      m2.get("foo") must beSome
    }
    
    "return the Map unchanged if the key already exists" in {
      val m1 = Map("foo" -> "bar")
      m1.size === 1
      m1.get("foo") must beSome
      
      val m2 = EnvironmentVars.safeAdd(m1, "foo", "bar")
      m2.size === 1
      m2.get("foo") must beSome
    }
  }
  
  "merge" should {
    
    "add all key/value pairs in map2 to map1 if they do not already exist" in {
      val m1 = Map("alpha" -> "foo", "beta" -> "bar")
      m1.size === 2
      
      val m2 = EnvironmentVars.merge(m1, Map("gamma" -> "baz"))
      m2.size === 3
      m2.get("gamma") must beSome("baz")
    }
    
    "ignore keys in map2 if they already exist in map1" in {
      val m1 = Map("alpha" -> "foo", "beta" -> "bar")
      m1.size === 2
      
      val m2 = EnvironmentVars.merge(m1, Map("alpha" -> "NEW", "beta" -> "NEW", "gamma" -> "baz"))
      m2.size === 3
      m2.get("alpha") must beSome("foo")
      m2.get("beta")  must beSome("bar")
      m2.get("gamma") must beSome("baz")
    }
  }
  
  "mergeBottomUp" should {
    
    "merge an indexed Seq of Maps giving precedence to lowest index" in {
      val maps = Seq(
          (0, Map("alpha" -> "first")),
          (1, Map("alpha" -> "second", "beta" -> "second")),
          (2, Map("alpha" -> "third", "beta" -> "third", "gamma" -> "third")))
      
      val m1 = EnvironmentVars.mergeBottomUp(maps)
      
      m1.get("alpha") must beSome("first")
      m1.get("beta")  must beSome("second")
      m1.get("gamma") must beSome("third")
    }
  }
  
  
  "get" should {
    
    "get the merged Environment Variables for the given resources" in {
      val org = Resource.fromPath("root")
      org must beSome
      
      val (wid,eid) = createWorkspaceEnvironment(org.get.id,
          workspaceProps   = Map("env" -> Json.stringify(Json.toJson(Map("VAR1" -> "workspace")))),
          environmentProps = Map("env" -> Json.stringify(Json.toJson(Map("VAR1" -> "environment", "VAR2" -> "environment")))))
      
      val wenv = EnvironmentVars.get(org.get.id, wid)
      wenv.size === 1
      wenv.get("VAR1") must beSome("workspace")
      
      val eenv = EnvironmentVars.get(org.get.id, eid)
      eenv.size === 2
      eenv.get("VAR1") must beSome("environment")
      eenv.get("VAR2") must beSome("environment")
    }
  }

}