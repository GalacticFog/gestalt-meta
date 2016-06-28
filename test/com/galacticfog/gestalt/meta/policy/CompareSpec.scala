package com.galacticfog.gestalt.meta.policy


import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._

import org.joda.time.DateTime

import org.specs2.specification.Scope



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
  
}