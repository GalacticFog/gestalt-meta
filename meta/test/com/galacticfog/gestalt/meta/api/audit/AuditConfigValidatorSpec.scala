package com.galacticfog.gestalt.meta.api.audit

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._

import org.specs2.mutable._
import org.specs2.specification._
import org.specs2.specification.Step
import play.api.libs.json._

import org.joda.time.DateTime
import cats.data.NonEmptyList
import cats.data.Validated._

class AuditConfigValidatorSpec extends Specification {
  
  
  object Test extends AuditConfigValidator
  
  "checkEnabled" should {
    
    "succeed (and set to 'false') when META_AUDIT_ENABLED is NOT present" >> {
      val results = Test.checkEnabled(Map())
      
      results.isValid === true
      
      val m = results.getOrElse(Map())
      m.contains(Keys.Enabled) === true
      m(Keys.Enabled).toBoolean === false
    }
    
    "succeed when set to a boolean value" >> {
      Test.checkEnabled(Map(Keys.Enabled -> "true")).isValid === true
      Test.checkEnabled(Map(Keys.Enabled -> "false")).isValid === true
      Test.checkEnabled(Map(Keys.Enabled -> "True")).isValid === true
      Test.checkEnabled(Map(Keys.Enabled -> "FALSE")).isValid === true 
    }
    
    "fail when the given value cannot be cast to Boolean" >> {
      Test.checkEnabled(Map(Keys.Enabled -> "T")).isInvalid === true
      Test.checkEnabled(Map(Keys.Enabled -> "F")).isInvalid === true
      Test.checkEnabled(Map(Keys.Enabled -> "foo")).isInvalid === true
      Test.checkEnabled(Map(Keys.Enabled -> "Yes")).isInvalid === true
      Test.checkEnabled(Map(Keys.Enabled -> "NO")).isInvalid === true 
    }
  }
  
  "checkName" should {
    "succeed when META_AUDIT_LOGGER_NAME is given" >> {
      Test.checkName(Map(Keys.Name -> "alpha")).isValid === true  
    }
    
    "fail when not present" >> {
      Test.checkName(Map()).isInvalid === true
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

      Test.checkFile(Map(Keys.LogFile -> file.getAbsolutePath)).isValid === true
    }
    
    "succeed when given a file that does not exist, but is in a writable directory" >> {
      val dir = Files.createTempDirectory(null)
      val file = "%s/%s".format(dir.toString, "not-exist.log")  
      /*
       * Should succeed because even through the file doesn't exist, the temp directory
       * is writable - logback will create the file at runtime.
       */
      Test.checkFile(Map(Keys.LogFile -> file)).isValid === true
    }
    
    "fail when META_AUDIT_LOG_FILE is not given" >> {
      Test.checkFile(Map()).isInvalid === true
    }
    
    "fail when given an invalid file location (does not exist)" >> {
      Test.checkFile(Map(Keys.LogFile -> "/var/log/no_way_this_dir_exists_foo/audit.log")).isInvalid === true
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

  "validate" should {
    val validSample = Map(Keys.Enabled -> "true", Keys.Name -> "alpha", Keys.LogFile -> "...")
    val invalidSample = Map(Keys.Enabled -> "foo")
    AuditEnv.validate(validSample) must beEqualTo(Valid(validSample))
    AuditEnv.validate(invalidSample) must beEqualTo(Invalid(NonEmptyList.of(
      "Must supply a value for 'META_AUDIT_LOGGER_NAME'", "Must supply a value for 'META_AUDIT_LOG_FILE'")))
  }
  
}