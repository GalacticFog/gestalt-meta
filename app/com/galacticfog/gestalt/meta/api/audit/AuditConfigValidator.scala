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
  
  import java.io.File
  import scalaz.{Success, Failure}

  /*
   * 1. File location env var MUST be present
   * 2. File must exist, or directory parent of file must exist
   * 3. File must be writable
   */  
  def checkFile(m: EnvConfig): Validation[String, EnvConfig] = {
    
    if (m.get(Keys.LogFile).isEmpty)
      s"Must supply a value for '${Keys.LogFile}'".failure
    else {
      val file: Option[File] = {
        val f = new File(m(Keys.LogFile))
        if (f.exists) Some(f)
        else {
          /*
           * File doesn't exist, check if parent directory exists
           * (file can be created at runtime)
           */
          val dir = getParentPath(f)
          val f2 = new File(dir)
          if (f2.exists) Some(f2)
          else None
        }
      }

      file match {
        case None =>       // File must be valid location
          s"Given audit-log file does not exist. found path: ${m(Keys.LogFile)}".failure
        case Some(f) => {  // File must be writable
          if (f.canWrite) m.success 
          else s"Given audit-log location is not writable".failure
        }
      }
    }
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
  
  /**
   * Get the path to the parent directory of a file.
   * This function is needed because File.getParent returns null when the given file doesn't exist.
   * For our purposes, we may be creating the file at runtime so we need to know what the parent
   * _will be_ so we can check the write-permissions.
   */
  private def getParentPath(f: File): String = {
    f.getAbsolutePath.stripSuffix("/").split("/").dropRight(1).mkString("/")  
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