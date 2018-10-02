package com.galacticfog.gestalt.meta.api.patch


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import org.joda.time.DateTime

import com.galacticfog.gestalt.patch._


class EnvOpTransformerSpec extends Specification {

  "isProtected" should {
    
    "return TRUE if given path begins with '/properties/workspace'" in {
      EnvOpTransformer.isProtected("/properties/workspace/id") must beTrue
    }
    
    "return FALSE if the given path does not begin with '/properties/workspace'" in {
      EnvOpTransformer.isProtected("/properties/environment_type") must beFalse
    }
    
    
  }
  
}