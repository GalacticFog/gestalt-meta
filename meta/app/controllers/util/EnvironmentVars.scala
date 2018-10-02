package controllers.util

import com.galacticfog.gestalt.data.ResourceFactory
import java.util.UUID
import play.api.libs.json.Json


object EnvironmentVars {

  
  /**
   * Get merged Environment Variables for the given Resource instance.
   */
  def get(org: UUID, instanceId: UUID) = {
    
    val rs = ResourceFactory.findEnvironmentVariables(instanceId)
    
    val all = rs map { case (k,v) =>
      if (v.properties.isEmpty) None
      else v.properties.get.get("env") match {
          case None => None
          case Some(vars) => {
            Option(k -> Json.parse(vars).validate[Map[String,String]].get)
        }
      }
    } filter { _.isDefined } flatMap { v => v }
    
    mergeBottomUp(all)
  }  
  
  
  /**
   * Merge an indexed Seq of Maps such that a given key from a Map with a lower index will be taken
   * over the corresponding key from a Map with a higher index. Essentially, iterate over the seq
   * from lowest index to highest, take each new key that you find, ignore that key if you find it in
   * a map with a higher index.
   */
  def mergeBottomUp(vs: Seq[(Int, Map[String,String])]): Map[String,String] = {

    def go(vars: Seq[(Int,Map[String,String])], acc: Map[String,String]): Map[String,String] = {
      vars match {
        case Nil => acc
        case h :: t => go(t, merge(acc, h._2))
      }    
    }
    go(vs.sortWith((a,b) => a._1 < b._1), Map())
  }
  
  
  /**
   * Add values from map2 into map1 if the key does not already exist in map1.
   */
  private[util] def merge(map1: Map[String,String], map2: Map[String,String]): Map[String,String] = {
    
    def loop(rs: List[(String,String)], acc: Map[String,String]): Map[String,String] = {
      rs match {
        case Nil    => acc
        case h :: t => loop( t, safeAdd( acc, h._1, h._2 ) )
      }      
    }
    if ( map2.isEmpty ) map1 else {
      loop( map2.toList, map1 )
    }
  }

  
  /**
   * Add key/value to Map if it does not already exist.
   */
  private[util] def safeAdd(m: Map[String,String], key: String, value: String): Map[String,String] = {
    if (!m.contains( key )) m ++ Map(key -> value) else m
  }
  
}