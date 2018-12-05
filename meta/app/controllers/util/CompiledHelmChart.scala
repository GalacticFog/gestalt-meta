package controllers.util

import org.yaml.snakeyaml._
//import scala.collection.JavaConverters._
import scala.collection.JavaConversions._

import org.yaml.snakeyaml.DumperOptions

object CompiledHelmChart {
  
  def load(yaml: String): List[Map[String, Object]] = {
    new Yaml().loadAll(yaml).toList.filter(_ != null).map(toScalaMap)
  }
  
  /**
   * Split a multi-document YAML file into a sequence of YAML Strings
   */
  def splitToYamlString(yaml: String): List[String] = {
    val options: DumperOptions = new DumperOptions();
		options.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK);
		
		new Yaml(options).loadAll(yaml).toList.filter(_ != null).map { y =>
		  val writer = new java.io.StringWriter()
		  new Yaml().dump(y, writer)
		  writer.toString()
		}
  }
  
  /**
   * Convert a YAML Object to a Scala Map[String, Object]    
   */
  def toScalaMap(jmap: Object) = {
    /*
     * mapAsScalaMap returns a mutable.Map - the final toMap call 
     * converts to immutable.Map
     */
    mapAsScalaMap(jmap.asInstanceOf[java.util.Map[String,Object]]).toMap
  }
  
}