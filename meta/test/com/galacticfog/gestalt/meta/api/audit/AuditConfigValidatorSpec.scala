package com.galacticfog.gestalt.meta.api.audit

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import org.joda.time.DateTime

import scalaz.{Success => VSuccess, Failure => VFailure}

class AuditConfigValidatorSpec extends Specification {
  
  
  object Test extends AuditConfigValidator
  
  "checkEnabled" should {
    
    "succeed (and set to 'false') when META_AUDIT_ENABLED is NOT present" >> {
      val results = Test.checkEnabled(Map())
      
      results.isSuccess === true
      
      val m = results.getOrElse(Map())
      m.contains(Keys.Enabled) === true
      m(Keys.Enabled).toBoolean === false
    }
    
    "succeed when set to a boolean value" >> {
      Test.checkEnabled(Map(Keys.Enabled -> "true")).isSuccess === true
      Test.checkEnabled(Map(Keys.Enabled -> "false")).isSuccess === true
      Test.checkEnabled(Map(Keys.Enabled -> "True")).isSuccess === true
      Test.checkEnabled(Map(Keys.Enabled -> "FALSE")).isSuccess === true 
    }
    
    "fail when the given value cannot be cast to Boolean" >> {
      Test.checkEnabled(Map(Keys.Enabled -> "T")).isFailure === true
      Test.checkEnabled(Map(Keys.Enabled -> "F")).isFailure === true
      Test.checkEnabled(Map(Keys.Enabled -> "foo")).isFailure === true
      Test.checkEnabled(Map(Keys.Enabled -> "Yes")).isFailure === true
      Test.checkEnabled(Map(Keys.Enabled -> "NO")).isFailure === true 
    }
  }
  
  "checkName" should {
    "succeed when META_AUDIT_LOGGER_NAME is given" >> {
      Test.checkName(Map(Keys.Name -> "alpha")).isSuccess === true  
    }
    
    "fail when not present" >> {
      Test.checkName(Map()).isFailure === true
    }
  }
  import java.io.File
  import java.nio.file.Files
  
  "checkFile" should {
    
    "succeed when given an existing, writable file name" >> {
      val fileprefix = java.util.UUID.randomUUID.toString
      val filesuffix = ".tmp"
      val filename = fileprefix + filesuffix
      val file = File.createTempFile(fileprefix, filesuffix)

      Test.checkFile(Map(Keys.LogFile -> file.getAbsolutePath)).isSuccess === true
    }
    
    "succeed when given a file that does not exist, but is in a writable directory" >> {
      val dir = Files.createTempDirectory(null)
      val file = "%s/%s".format(dir.toString, "not-exist.log")  
      /*
       * Should succeed because even through the file doesn't exist, the temp directory
       * is writable - logback will create the file at runtime.
       */
      Test.checkFile(Map(Keys.LogFile -> file)).isSuccess === true
    }
    
    "fail when META_AUDIT_LOG_FILE is not given" >> {
      Test.checkFile(Map()).isFailure === true
    }
    
    "fail when given an invalid file location (does not exist)" >> {
      Test.checkFile(Map(Keys.LogFile -> "/var/log/no_way_this_dir_exists_foo/audit.log")).isFailure === true
    }
    
    "fail when given a file location that is not writable" >> {
      /*
       * Our unit-tests run under Docker as 'root'.  Not sure how to ensure a location
       * is NOT writable by root to test this validation.
       */
      pending 
    }
  }
  
//  
//  "checkRolling" should {
//    failure
//  }
  
}