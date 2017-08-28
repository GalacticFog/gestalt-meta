package com.galacticfog.gestalt.meta.api.audit


import play.api.libs.json._
import org.slf4j.LoggerFactory
import scalaz.{Success => VSuccess, Failure => VFailure}


object Audit {
  
  protected val internalLog = LoggerFactory.getLogger(this.getClass)
  
  lazy val logger: AuditLogger = init()
  
  
  def log(message: String, level: String = "info", marker: Option[String] = None) = {
    logger.log(message, level, marker)  
  }
  
  
  /**
   * Status/health check for auditing functions. Validates the environment configuration and reports
   * whether or not Auditing *could* take place with the current configuration.  `status` 'OK' and
   * `enabled` 'true' indicates that the system is currently under audit.
   */
  def check(envmap: Option[Map[String,String]] = None): JsValue = {
    
    val auditEnv = envmap getOrElse sys.env filter {  
      case (k, v) => k.startsWith(Keys.VarPrefix) 
    }
    
    AuditEnv.validate(auditEnv) match {
      case VSuccess(env) => Json.obj(
            "config_state"   -> "OK",
            "enabled"        -> env(Keys.Enabled),
            "settings"       -> Json.toJson(env))
     
      case VFailure(errs) => Json.obj(
            "config_state"   -> "ERROR",
            "enabled"        -> auditEnv(Keys.Enabled),
            "errors"         -> Json.toJson(errs.list),
            "settings"       -> Json.toJson(auditEnv))
    }
  }

  /**
   * Initialize the audit Logger. Use values from argument Map if given, otherwise
   * use system environment.
   */
  protected def init(envmap: Option[Map[String,String]] = None): AuditLogger = {
    internalLog.info("Attempting to intialize Audit Logging...")
    
    val auditEnv = envOrDefault(envmap)
    
    if (!loggingEnabled(Some(auditEnv))) {
      internalLog.info("Audit logging is DISABLED")
      new NoOpLogger()
    } 
    else {
      
      AuditEnv.validate(auditEnv) match {
        
        case VFailure(errs) => {
          internalLog.error(s"Bad Audit Configuration. Errors: [${errs.list.mkString(",")}]")
          internalLog.warn(s"Defaulting to no-op Logger. Audit logs will NOT be created for this Meta instance.")
          new NoOpLogger()
          
        }
        case VSuccess(vars) => {
          internalLog.info(s"Audit configuration validated [SUCCESS]. Determining logger type...")
          
          val name      = vars.get(Keys.Name).get
          val file      = vars.get(Keys.LogFile).get
          //val failExit  = vars.get(Keys.ExitOnFailure).fold(false)(_.toBoolean)
          val exclusive = vars.get(Keys.LogExclusive).fold(true)(_.toBoolean)
          val rollover  = vars.get(Keys.LogRollover).fold("none")(a => a)
          
          if (rollover == "none") {
            internalLog.info("Configuring standard File Logger.")
            FileLogger(name, file, exclusive)
            
          } else { 
            internalLog.info(s"Configuring Rolling-File Logger ($rollover)")
            RollingFileLogger(name, file, exclusive, rollover)    
          }
        }
      }
    }
  }
  
  /**
   * Determine if logging is currently enabled.
   */
  private def loggingEnabled(map: Option[Map[String,String]] = None) = {
    val env = envOrDefault(map)
    env.get(Keys.Enabled).nonEmpty && env(Keys.Enabled).toBoolean
  }
  
  /**
   * Unpack the given map if Some, filter and return META_AUDIT variables from the
   * system env if map is None.
   */
  private def envOrDefault(m: Option[Map[String, String]]): Map[String, String] = {
    m getOrElse sys.env filter { case (k, v) => k.startsWith(Keys.VarPrefix) }
  }  
}


/**
 * Constants for looking up the audit feature environment variables.
 */
object Keys {
  val VarPrefix     = "META_AUDIT"
  val Enabled       = s"${VarPrefix}_ENABLED"        // default 'false'
  val Name          = s"${VarPrefix}_LOGGER_NAME"    // required
  val LogFile       = s"${VarPrefix}_LOG_FILE"       // required
  val LogExclusive  = s"${VarPrefix}_LOG_EXCLUSIVE"  // default 'true'
  val LogRollover   = s"${VarPrefix}_LOG_ROLLOVER"   // default 'daily'
  val ExitOnFailure = s"${VarPrefix}_EXIT_FAILURE"   // default 'false'
}


