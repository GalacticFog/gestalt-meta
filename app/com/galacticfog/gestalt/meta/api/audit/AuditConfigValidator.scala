package com.galacticfog.gestalt.meta.api.audit

import scalaz._
import Scalaz._

/**
 * Contains functions for validating META_AUDIT configuration values.
 */
trait AuditConfigValidator {
  
  private type EnvConfig = Map[String,String]
  
  def checkEnabled(m: EnvConfig): Validation[String, EnvConfig] = {
    if (m.get(Keys.Enabled).isEmpty) (m + (Keys.Enabled -> "false")).success
    else {
      val value = m(Keys.Enabled).trim.toLowerCase
      if (value == "true" || value == "false") m.success
      else 
        s"Invalid value for '${Keys.Enabled}'. expected: Boolean, found: '$value'".failure
    }
  }
  
  def checkName(m: EnvConfig): Validation[String, EnvConfig] = {
    if (m.get(Keys.Name).isDefined) m.success
    else s"Must supply a value for '${Keys.Name}'".failure
  }
  
  def checkFile(m: EnvConfig): Validation[String, EnvConfig] = {
    if (m.get(Keys.LogFile).isDefined) m.success
    else s"Must supply a value for '${Keys.LogFile}'".failure
  }
  
  def checkRolling(m: EnvConfig): Validation[String, EnvConfig] = {
    val accepted = Seq("year", "month", "day", "hour", "minute")
    
    if (m.get(Keys.LogRollover).isEmpty) 
      m.success
    else if (accepted.contains(m(Keys.LogRollover).trim.toLowerCase)) 
      m.success
    else 
      s"Invalid log rollover value. Must be one of '${accepted.mkString(",")}'. found: '${m(Keys.LogRollover)}'".failure
  }
}


object AuditEnv extends AuditConfigValidator {

  def validate(m: Map[String, String]) = {
    val checks = List(checkName _, checkFile _, checkRolling _)
    checks.traverseU( _ andThen (_.toValidationNel) apply m) map {
      case x :: _ => x
    }
  }
}