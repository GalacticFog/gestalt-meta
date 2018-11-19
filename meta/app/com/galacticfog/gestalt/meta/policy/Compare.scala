package com.galacticfog.gestalt.meta.policy

import play.api.Logger
import play.api.libs.json._

  trait Compare[A,B] {
    /**
     * 
     * @param a the control value to test
     * @param b the value to use for comparison
     */
    def compare(a: A, b: B, op: String): Boolean
  }
  
  /* containsKey
   * containsValue
   * key equals value
   * Check if key is in Map hasKey
   * Check if key is NOT in Map notKey
   * Check if key equals value
   * Check if value is in Map
   * 
   */

  class CompareKeyToMap[K,V] extends Compare[Map[K,V], K] {
    def compare(a: Map[K,V], b: K, op: String): Boolean = {
      ???
    }
  }
  
  class CompareValueToMap[K,V] extends Compare[Map[K,V], V] {
    def compare(a: Map[K,V], b: V, op: String): Boolean = {
      op.trim.toLowerCase match {
        case "equals" => ???
        case "==" => ???
        case "notequals" => ???
        case "!=" => ???
        case "startswith" => ???
        case "endswith" => ???
        case "contains" => ???
      }
    }
  }
  
  class CompareMapToMap[K,V] extends Compare[Map[K,V],Map[K,V]] {
    def compare(a: Map[K,V], b: Map[K,V], op: String) = {
      op.trim.toLowerCase match {
        case "containskey" => ???
        case "!containskey" => ???
        case "containskeys" => ???
        case "containskeysany" => ???
      }
    }
  }

  class CompareListToMap[K,V] extends Compare[Map[K,V],Seq[K]] {
    def compare(a: Map[K,V], b: Seq[K], op: String) = {
      op.trim.toLowerCase match {
        case "containskeys"  => b.diff(a.keys.toSeq).isEmpty
        case "!containskeys" => ???
      }
    }
  }  
  
  
  object CompareSingleToList {
    def apply[A]() = new CompareSingleToList[A]()
  }
  class CompareSingleToList[A] extends Compare[Seq[A],A] {
    private val log = Logger(this.getClass)
    
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
    private val log = Logger(this.getClass)
    def compare(a: Seq[A], b: Seq[A], op: String) = {
      log.debug(s"CompareListToList.compare($a, $b)")
      op.trim.toLowerCase match {
        case "inlist" => b.diff(a).isEmpty
        case _ => throw new IllegalArgumentException(s"Invalid String comparison: '$op'")
      }
    }
  }

object CompareArrayToArray extends Compare[JsArray, JsArray] {
  private val log = Logger(this.getClass)
  
  def compare(a: JsArray, b: JsArray, op: String) = {

    val a1 = a.as[Seq[JsValue]]
    val b1 = b.as[Seq[JsValue]]

    op.trim.toLowerCase match {
      case "inlist" => {
        log.debug(s"${a1} exists_in ${b1}")
        (a1 diff b1).isEmpty //b1.diff(a1).isEmpty
      }
      case _        => throw new IllegalArgumentException(s"Invalid comparison: '$op'")
    }
  }
}

object CompareSingleToArray extends Compare[JsArray, JsValue] {
  val log = Logger(this.getClass)
  
  def compare(a: JsArray, b: JsValue, op: String) = {
    log.debug("Testing : %s %s %s".format(a.toString, op, b.toString))
    val a1 = a.as[Seq[JsValue]]

    op.trim.toLowerCase match {
      case "contains" => a1 contains b
      case "inlist"   => a1 contains b
      case "oneof"    => a1 contains b
      case "except"   => !(a1 contains b)
      case _          => throw new IllegalArgumentException(s"Invalid comparison: '$op'")
    }
  }
}
object CompareSingleNumeric extends Compare[JsNumber, JsNumber] {

  def compare(a: JsNumber, b: JsNumber, op: String): Boolean = {

    val a1 = a.value
    val b1 = b.value

    op.trim.toLowerCase match {
      case "equals" => a1 == b1
      case "==" => a1 == b1
      case "!=" => a1 != b1
      case "<=" => a1 <= b1
      case ">=" => a1 >= b1
      case ">"  => a1 > b1
      case "<"  => a1 < b1
      case _ => throw new IllegalArgumentException(s"Invalid comparison op: '$op'")
    }
  }
}

object CompareSingle extends Compare[JsValue, JsValue] {
  def compare(a: JsValue, b: JsValue, op: String) = {
    op.trim.toLowerCase match {
      case "=="         => a == b
      case "equals"     => a == b
      case "!="         => a != b
      case "startswith" => {
        println(s"${a.as[String]} startsWith ${b.as[String]} : ${a.as[String] startsWith b.as[String]}" )
        a.as[String] startsWith b.as[String]
      }
      case "contains"   => a.as[String] contains b.as[String]
      case "endswith"   => a.as[String] endsWith b.as[String]
      case _ => throw new IllegalArgumentException(s"Invalid comparison op: '$op'")
    }
  }
}
  
  object CompareString extends Compare[String,String] {
    private val log = Logger(this.getClass)
    def compare(a: String, b: String, op: String) = {
      log.debug(s"CompareString.compare($a, $b)")
      op.trim.toLowerCase match {
        case "=="         => a == b
        case "equals"     => a == b
        case "!="         => a != b
        case "contains"   => a contains b
        case "startswith" => a startsWith b
        case "endswith"   => a endsWith b
        case _ => throw new IllegalArgumentException(s"Invalid String comparison: '$op'")
      }
    }
  }
  
  object CompareInt extends Compare[Int,Int] {
    private val log = Logger(this.getClass)
    def compare(a: Int, b: Int, op: String) = {
      log.debug(s"CompareInt.compare($a, $b)")
      op.trim match {
        case "==" => a == b
        case "!=" => a != b
        case "<=" => a <= b
        case ">=" => a >= b
        case ">"  => a > b
        case "<"  => a < b
        case _ => throw new IllegalArgumentException(s"Invalid Int comparison: '$op'")
      }
    }
  }
  
  object CompareFloat extends Compare[Double,Double] {
    private val log = Logger(this.getClass)
    def compare(a: Double, b: Double, op: String) = {
      log.debug(s"CompareFloat.compare($a, $b)")
      op.trim match {
        case "==" => a == b
        case "!=" => a != b
        case "<=" => a <= b
        case ">=" => a >= b
        case ">"  => a > b
        case "<"  => a < b
        case _ => throw new IllegalArgumentException(s"Invalid Int comparison: '$op'")
      }
    }
  }
  
  