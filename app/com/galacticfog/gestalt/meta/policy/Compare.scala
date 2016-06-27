package com.galacticfog.gestalt.meta.policy

import play.api.{Logger => log}

  trait Compare[A,B] {
    /**
     * 
     * @param a the control value to test
     * @param b the value to use for comparison
     */
    def compare(a: A, b: B, op: String): Boolean
  }
  
  
  
  /*
   * compareSingleToList
   * compareListToList
   */
  
  object CompareSingleToList {
    def apply[A]() = new CompareSingleToList[A]()
  }
  
  class CompareSingleToList[A] extends Compare[Seq[A],A] {
    def compare(a: Seq[A], b: A, op: String) = {
      log.debug(s"CompareSingleToList.compare($a, $b)")
      op.trim.toLowerCase match {
        case "contains"   => a contains b
        case "inlist"   => a contains b
        case "oneof"   => a contains b
        case "except" => !(a contains b)
        case _ => throw new IllegalArgumentException(s"Invalid String comparison: '$op'")
      }
    }
  }
  
  object CompareListToList {
    def apply[A]() = new CompareListToList[A]()
  }
  class CompareListToList[A] extends Compare[Seq[A], Seq[A]] {
    def compare(a: Seq[A], b: Seq[A], op: String) = {
      log.debug(s"CompareListToList.compare($a, $b)")
      op.trim.toLowerCase match {
        case "inlist" => b.diff(a).isEmpty
        case _ => throw new IllegalArgumentException(s"Invalid String comparison: '$op'")
      }
    }
  }
  
  object CompareString extends Compare[String,String] {
    
    def compare(a: String, b: String, op: String) = {
      log.debug(s"CompareString.compare($a, $b)")
      op.trim.toLowerCase match {
        case "=="         => a == b
        case "equals"     => a == b
        case "contains"   => a contains b
        case "startswith" => a startsWith b
        case "endswith"   => a endsWith b
        case _ => throw new IllegalArgumentException(s"Invalid String comparison: '$op'")
      }
    }
  }  
  
  object CompareInt extends Compare[Int,Int] {
    def compare(a: Int, b: Int, op: String) = {
      op.trim.toLowerCase match {
        case "==" => a == b
        case "<=" => a <= b
        case ">=" => a >= b
        case ">"  => a > b
        case "<"  => a < b
        case _ => throw new IllegalArgumentException(s"Invalid Int comparison: '$op'")
      }
    }
  }
  
  object CompareFloat extends Compare[Double,Double] {
    
    def compare(a: Double, b: Double, op: String) = {
      log.debug(s"CompareFloat.compare($a, $b)")
      op.trim.toLowerCase match {
        case "==" => a == b
        case "<=" => a <= b
        case ">=" => a >= b
        case ">"  => a > b
        case "<"  => a < b
        case _ => throw new IllegalArgumentException(s"Invalid Int comparison: '$op'")
      }
    }
  }
  
  