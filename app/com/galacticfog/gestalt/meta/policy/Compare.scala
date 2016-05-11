package com.galacticfog.gestalt.meta.policy

  trait Compare[T] {
    def compare(a: T, b: T, op: String): Boolean
  }
  
  object CompareString extends Compare[String] {
    def compare(a: String, b: String, op: String) = {
      op.trim.toLowerCase match {
        case "=="         => a == b
        case "contains"   => a contains b
        case "startswith" => a startsWith b
        case "endswith"   => a endsWith b
        case _ => throw new IllegalArgumentException(s"Invalid String comparison: '$op'")
      }
    }
  }
  
  object CompareInt extends Compare[Int] {
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