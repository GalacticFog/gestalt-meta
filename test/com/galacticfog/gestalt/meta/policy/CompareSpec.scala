package com.galacticfog.gestalt.meta.policy


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._

import org.joda.time.DateTime

import org.specs2.specification.Scope

import play.api.libs.json._

trait CompareScope extends Scope {
  
  val stringA = "whereiswaldo?"
  
  val stringListA = Seq("alpha", "bravo", "charlie")
}

class CompareSpec extends Specification {

  "CompareListToList[Int].compare()" should {
    
    "inList" should {
      
      "return TRUE if ALL OF the values in Seq B exist in Seq A" in {
        CompareListToList().compare(Seq(1,2,3,4,5), Seq(2,3,4), "inList") must beTrue
        CompareListToList().compare(Seq(1,2,3,4,5), Seq(1), "inList") must beTrue
        CompareListToList().compare(Seq(1), Seq(1), "inList") must beTrue
      }
      
      "return FALSE if ANY OF the values in Seq B do not exist in Seq A" in {
        CompareListToList().compare(Seq(1,2,3,4,5), Seq(4,5,6), "inList") must beFalse
        CompareListToList().compare(Seq(1), Seq(1,2), "inList") must beFalse
        CompareListToList().compare(Seq(1), Seq(2), "inList") must beFalse
        CompareListToList().compare(Seq(1,2,3), Seq(4), "inList") must beFalse
      }      
      
    }
    
  }
  
  
  "CompareSingleToList[String].compare()" should {
    
    "except" should {
      "return TRUE if String B does not exist in Seq A" in new CompareScope{
        CompareSingleToList().compare(stringListA, "foxtrot", "except") must beTrue
      }
      
      "return FALSE if String B exists in Seq A" in new CompareScope {
        CompareSingleToList().compare(stringListA, "bravo", "except") must beFalse
      }      
    }
    
    "inList" should {
      
      "return TRUE if String B is in Seq[String] A" in new CompareScope {
        CompareSingleToList().compare(stringListA, "alpha", "inList") must beTrue
      }
      
      "return FALSE if String B is not in Seq[String] A" in new CompareScope {
        CompareSingleToList().compare(stringListA, "echo", "inList") must beFalse
      }

    }
    
  }

  "CompareSingleToList[Int].compare()" should {
    
    "inList" should {
      
      "return TRUE if Int B is in Seq[Int] A" in new CompareScope {
        CompareSingleToList().compare(Seq(1,2,3), 3, "inList") must beTrue
      }
      
      "return FALSE if Int B is in Seq[Int] A" in new CompareScope {
        CompareSingleToList().compare(Seq(1,2,3), 4, "inList") must beFalse
      }
    }
    
  }
  
  
  "CompareString.compare()" should {
    
    "==" should {
      
      "return TRUE when String B is exactly equal to String A" in new CompareScope {
        CompareString.compare(stringA, "whereiswaldo?", "==") must beTrue
      }
      
      "return FALSE when String B is not exactly equal to String A" in new CompareScope {
        CompareString.compare(stringA, "WhereIsWaldo", "==") must beFalse
      }
      
    }
    
    
    "contains" should {
      
      "return TRUE when String A contains String B" in new CompareScope {
        CompareString.compare(stringA, "waldo", "contains") must beTrue
      }
      
      "return FALSE when String A does not contain String B" in new CompareScope {
        CompareString.compare(stringA, "notwaldo", "contains") must beFalse        
      }
      
    }
    
    "startsWith" should {
      
      "return TRUE when String A begins with the same characters as String B" in new CompareScope {
        CompareString.compare("abcde", "a", "startsWith") must beTrue
        CompareString.compare("abcde", "abc", "startsWith") must beTrue
        CompareString.compare("abcde", "abcde", "startsWith") must beTrue
      }
      
      "return FALSE when String A does not begin with the same characters as String B" in new CompareScope {
        CompareString.compare("abcde", "cde", "startsWith") must beFalse
      }
      
    }
    
    "endsWith" should {
      
      "return TRUE when String A ends with the same characters as String B" in new CompareScope {
        CompareString.compare("abcde", "e", "endsWith") must beTrue
        CompareString.compare("abcde", "cde", "endsWith") must beTrue
        CompareString.compare("abcde", "abcde", "endsWith") must beTrue
      }
      
      "return FALSE when String A does not end with the same characters as String B" in new CompareScope {
        CompareString.compare("abcde", "abc", "endsWith") must beFalse
      }
      
    }
      
  }

  def jv = toJsValue _
  def ja(s: String*): JsArray = toJsValue(s.mkString(",")).as[JsArray]
    
  "CompareSingle" should {
   
    "equals" should {
  
      "return TRUE when OBJECT a equals OBJECT b" in {
                
        val a = jv("""{"alpha": "foo", "bravo": "bar", "charlie": "baz"}""")
        val b = jv("""{"bravo": "bar", "charlie": "baz", "alpha": "foo"}""")
        val c = jv("""{"alpha": "foo", "bravo": "bar", "charlie": "qux"}""")
        val d = jv("""{"delta": "foo", "echo": "bar", "foxtrot": "baz"}""")
        
        
        // object
        CompareSingle.compare(a, b, "equals" ) must beTrue
        CompareSingle.compare(a, c, "equals" ) must beFalse
        CompareSingle.compare(a, d, "equals" ) must beFalse
        
        // string
        CompareSingle.compare(jv("foo"), jv("foo"), "equals" ) must beTrue
        CompareSingle.compare(jv("foo"), jv("bar"), "equals" ) must beFalse
        
        // array
        CompareSingle.compare(ja("foo", "bar"), ja("foo", "bar"), "equals" ) must beTrue
        CompareSingle.compare(ja("foo", "bar"), ja("bar", "baz"), "equals" ) must beFalse
        
        // boolean
        CompareSingle.compare(jv("true"), jv("TRUE"), "equals" ) must beTrue
        CompareSingle.compare(jv("FALSE"), jv("false"), "equals" ) must beTrue
        CompareSingle.compare(jv("true"), jv("blue"), "equals" ) must beFalse
        CompareSingle.compare(jv("true"), jv("false"), "equals" ) must beFalse
        
        // int
        CompareSingle.compare(jv("123"), jv("123"), "equals" ) must beTrue
        CompareSingle.compare(jv("123"), jv("foo"), "equals" ) must beFalse
        CompareSingle.compare(jv("foo"), jv("123"), "equals" ) must beFalse
        
        // double
        CompareSingle.compare(jv("1.23"), jv("1.23"), "equals" ) must beTrue
        CompareSingle.compare(jv("1.23"), jv("foo"), "equals" ) must beFalse
      } 
      
    }
    
    "contains" should {
      "return TRUE if a contains character sequence b" in new CompareScope {
        CompareSingle.compare(jv(stringA), jv("waldo"), "contains") must beTrue
        CompareSingle.compare(jv(stringA), jv("fred"), "contains") must beFalse
      }
    }
    
    "startsWith" should {
      "return TRUE if a starts with character sequence b" in new CompareScope {
        CompareSingle.compare(jv(stringA), jv("where"), "startsWith") must beTrue
        CompareSingle.compare(jv(stringA), jv("fred"), "startsWith") must beFalse        
      }
    }
    
    "endsWith" should {
      "return TRUE if a ends with character sequence b" in new CompareScope {
        CompareSingle.compare(jv(stringA), jv("waldo?"), "endsWith") must beTrue
        CompareSingle.compare(jv(stringA), jv("fred"), "endsWith") must beFalse        
      }
    }
  }
  
  "CompareSingleToArray" should {
    
    "contains" should {
      
      "return TRUE if value b is in array a" in {
        CompareSingleToArray.compare(ja("1, 2, 3"), jv("1"), "contains") must beTrue
        CompareSingleToArray.compare(ja("1, 2, 3"), jv("4"), "contains") must beFalse
        CompareSingleToArray.compare(ja("1, 2, 3"), jv("foo"), "contains") must beFalse
      }
    }
    
    "except" should {
      "return TRUE if value b is NOT in array a" in {
        CompareSingleToArray.compare(ja("1, 2, 3"), jv("4"), "except") must beTrue
        CompareSingleToArray.compare(ja("1, 2, 3"), jv("1"), "except") must beFalse
      }
    }
  }
  
  "CompareArrayToArray" should {
    
    "inlist" should {
      
      "return TRUE if ALL OF the values in a exist in b" in {
        CompareArrayToArray.compare(ja("1,2,3"), ja("1,2,3,4,5"), "inlist") must beTrue
        CompareArrayToArray.compare(ja("4,5,6"), ja("1,2,3,4,5"), "inlist") must beFalse
      }
      
    }
    
  }
  
}