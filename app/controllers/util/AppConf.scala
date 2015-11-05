package controllers.util

import play.api.Play.current

import scala.collection.JavaConverters._

/** 
 *  TEMP: gives access to objects in application.conf 
 */

object AppConf {

  def getConfigObject(key: String) = {
    current.configuration.getObject(key)
  }
    
  def get(configItem: String): Option[Map[String,Object]] = {
    AppConf.getConfigObject(configItem) match {
      case None    => throw new RuntimeException(ErrConfigNotFound.format(configItem))
      case Some(o) => Some(o.unwrapped.asScala.toMap)
    }    
  }
  private val ErrConfigNotFound = "Configuration item '%s' not found in application.conf"
}