package com.galacticfog.gestalt.meta.api.audit


import org.slf4j.{LoggerFactory, MarkerFactory}
import ch.qos.logback.classic.{Level, Logger, LoggerContext}
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.core.encoder.Encoder
import ch.qos.logback.core.FileAppender
import ch.qos.logback.core.rolling.RollingFileAppender
import ch.qos.logback.core.rolling.TimeBasedRollingPolicy



trait AuditLogger {
  def log(message: String, level: String, marker: Option[String])  
}


trait FileBasedAuditLogger extends AuditLogger {
  
  //private[this] val internalLog = LoggerFactory.getLogger("AuditLogger")
  
  protected val logger: Logger
  
  private lazy val pattern1 = """%-4relative [%thread] %-5level %logger{35} - %msg %n"""
  private lazy val pattern2 = """%d{HH:mm:ss.SSS} |-%-5level| %logger{0}: %message%n%xException"""
  
  def log(message: String, level: String = "info", marker: Option[String] = None) = {
    f(level)(message)
  }
  
  protected def newLogger(name: String, file: String, rolling: String, exclusive: Boolean) = {
    val context = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]
    
    val encoder = new PatternLayoutEncoder()
    encoder.setPattern(pattern2)
    encoder.setContext(context)
    encoder.start()
    
    val appender = newAppender(context, encoder, file, rolling)
    appender.start()
    
    val log = LoggerFactory.getLogger(name).asInstanceOf[Logger]
    log.addAppender(appender)
    log.setAdditive(!exclusive)
    log    
  }
  
  private def newAppender(context: LoggerContext, encoder: Encoder[ILoggingEvent], file: String, rollover: String) = {
    rolloverPattern(rollover).fold {
      newFileAppender(context, encoder, file)
    }{ pattern => 
      newRollingFileAppender(context, encoder, file, pattern)
    }
  }

  private def newFileAppender(context: LoggerContext, encoder: Encoder[ILoggingEvent], file: String) = {
    val appender = new FileAppender[ILoggingEvent]
    
    appender.setContext(context)
    appender.setEncoder(encoder)
    appender.setFile(file)
    appender
  }
  
  private def newRollingFileAppender(
      context: LoggerContext, encoder: Encoder[ILoggingEvent], file: String, datePattern: String) = {
    val appender = new RollingFileAppender[ILoggingEvent]
    
    appender.setContext(context)
    appender.setEncoder(encoder)
    appender.setFile(file)
    
    val rp = new TimeBasedRollingPolicy()
    rp.setParent(appender)
    rp.setContext(context)
    rp.setFileNamePattern("%s-%s".format(datePattern, file))
    rp.start()
    
    appender.setRollingPolicy(rp)
    appender
  }

  private def rolloverPattern(s: String): Option[String] = {
    s.trim.toLowerCase match {
      case "month"  => Some("%d{yyyy-MM}")
      case "day"    => Some("%d{yyyy-MM-dd}")
      case "hour"   => Some("%d{yyyy-MM-dd_HH}")
      case "minute" => Some("%d{yyyy-MM-dd_HH_mm}")
      case "none"   => None
      case _ => {
       // internalLog.warn(s"Unrecognized log-file rollover pattern. Setting to 'daily': {yyyy-MM-dd}")
        Some("%d{yyyy-MM-dd}")
      }
    }
  }
  
  protected def f(s: String) = {
    s.trim.toLowerCase match {
      case "trace" => logger.trace(_: String)
      case "debug" => logger.debug(_: String)
      case "info"  => logger.info(_: String)
      case "warn"  => logger.warn(_: String)
      case "error" => logger.error(_: String)
      case _ => throw new RuntimeException(s"Unknown log-level found '$s'.")
    }
  }  
}