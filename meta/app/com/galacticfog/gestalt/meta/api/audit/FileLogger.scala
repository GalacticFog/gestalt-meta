package com.galacticfog.gestalt.meta.api.audit


case class FileLogger(name: String, file: String, exclusive: Boolean) extends FileBasedAuditLogger {
  override lazy val logger = newLogger(name, file, "none", exclusive)
}

case class RollingFileLogger(name: String, file: String, exclusive: Boolean, rollover: String) extends FileBasedAuditLogger {
  override lazy val logger = newLogger(name, file, rollover, exclusive)  
}

class NoOpLogger() extends FileBasedAuditLogger {
  override lazy val logger = null
  override def log(message: String, level: String = "info", marker: Option[String] = None) = ()
}