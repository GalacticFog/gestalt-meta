package com.galacticfog.gestalt.meta.api.patch



import org.specs2.mutable._




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