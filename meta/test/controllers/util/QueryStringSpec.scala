package controllers.util

import org.specs2.mutable._
import com.galacticfog.gestalt.meta.api.errors.BadRequestException


class QueryStringSpec extends Specification {
  
  
  val qs1 = Map(
      "a" -> Seq("alpha"),
      "b" -> Seq("bravo"),
      "c" -> Seq("lima", "mike", "november"),
      "d" -> Seq())
  
  val qsBool = Map(
      "a" -> Seq("true"),
      "b" -> Seq("false"),
      "c" -> Seq("true", "true"),
      "d" -> Seq("not_boolean"))
      
  "single" should {
    
    "return a scalar value if the param exists" >> {
      val result = QueryString.single(qs1, "a")
      result must beSome
      result.get === "alpha"
    }
    
    "return None if the param does not exist" >> {
      QueryString.single(qs1, "not_exist") must beNone
    }    
    
    "fail if more than one value is given" >> {
      QueryString.single(qs1, "c") must throwA[BadRequestException]
    }
    
    "STRICT == FALSE: return None if param exists but no values given" >> {
      QueryString.single(qs1, "d", strict = false) must beNone
    }    

    "STRICT == TRUE: fail if param exists but no values given" >> {
      QueryString.single(qs1, "d", strict = true) must throwA[BadRequestException]
    }    
    
  }
  
  "list" should {
    
    "return a list of one or more values if the param exists" >> {
      val result = QueryString.list(qs1, "c")
      result.size === 3
    }
    
    "return an empty list if the param does not exist" >> {
      QueryString.list(qs1, "not_exist").size === 0
    }
    
    "STRICT == FALSE: return an empty list if param exists but no values given" >> {
      QueryString.list(qs1, "d", strict = false).isEmpty === true
    }    

    "STRICT == TRUE: fail if param exists but no values given" >> {
      QueryString.single(qs1, "d", strict = true) must throwA[BadRequestException]
    }     
  }
  
  "singleBoolean" should {
    
    "return a boolean value if the param is found and the value is boolean" >> {
      QueryString.singleBoolean(qsBool, "a") === true
      QueryString.singleBoolean(qsBool, "b") === false
    }
    
    "return false if the param does not exist" >> {
      QueryString.singleBoolean(qsBool, "not_exist") === false
    }
    
    "fail if param is given but the value is not boolean" >> {
      QueryString.singleBoolean(qsBool, "d") must throwA[BadRequestException]
    }
    
    "fail if more than one value is given" >> {
      QueryString.singleBoolean(qsBool, "c") must throwA[BadRequestException]
    }
    
  }
  
  
}