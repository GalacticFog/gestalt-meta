package com.galacticfog.gestalt.meta.api.audit

import cats.Apply
import cats.data._
import cats.data.Validated._

/**
 * Contains functions for validating META_AUDIT configuration values.
 */
trait AuditConfigValidator {
  
  protected type EnvConfig = Map[String,String]
  
  def checkEnabled(m: EnvConfig): Validated[String, EnvConfig] = {
    if (m.get(Keys.Enabled).isEmpty) Valid((m + (Keys.Enabled -> "false")))
    else {
      val value = m(Keys.Enabled).trim.toLowerCase
      if (value == "true" || value == "false") Valid(m)
      else 
        Invalid(s"Invalid value for '${Keys.Enabled}'. expected: Boolean, found: '$value'")
    }
  }
  
  def checkName(m: EnvConfig): Validated[String, EnvConfig] = {
    if (m.get(Keys.Name).isDefined) Valid(m)
    else Invalid(s"Must supply a value for '${Keys.Name}'")
  }
  
  import java.io.File

  /*
   * 1. File location env var MUST be present
   * 2. File must exist, or directory parent of file must exist
   * 3. File must be writable
   */  
  def checkFile(m: EnvConfig): Validated[String, EnvConfig] = {
    
    if (m.get(Keys.LogFile).isEmpty)
      Invalid(s"Must supply a value for '${Keys.LogFile}'")
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
          Invalid(s"Given audit-log file does not exist. found path: ${m(Keys.LogFile)}")
        case Some(f) => {  // File must be writable
          if (f.canWrite) Valid(m) 
          else Invalid(s"Given audit-log location is not writable")
        }
      }
    }
  }
    
  def checkRolling(m: EnvConfig): Validated[String, EnvConfig] = {
    val accepted = Seq("year", "month", "day", "hour", "minute")
    
    if (m.get(Keys.LogRollover).isEmpty) 
      Valid(m)
    else if (accepted.contains(m(Keys.LogRollover).trim.toLowerCase)) 
      Valid(m)
    else 
      Invalid(s"Invalid log rollover value. Must be one of '${accepted.mkString(",")}'. found: '${m(Keys.LogRollover)}'")
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

  type VNS[A] = ValidatedNel[String,A]

  def validate(m: Map[String, String]): ValidatedNel[String,EnvConfig] = {
    Apply[VNS].map3(
      checkName(m).toValidatedNel,
      checkFile(m).toValidatedNel,
      checkRolling(m).toValidatedNel
    ) { case(a, b, c) =>
      a ++ b ++ c
    }
  }
}