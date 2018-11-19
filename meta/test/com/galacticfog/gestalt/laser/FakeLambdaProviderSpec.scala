// package com.galacticfog.gestalt.laser

// import java.util.UUID
// import org.specs2.mutable.Specification
// import com.galacticfog.gestalt.data.uuid2string
// import play.api.libs.json.JsValue


// class FakeLambdaProviderSpec extends Specification {

//   class ValueMap(map: Map[String,Any]) {

//     def getOrElse[A](key: String, default: A): A = {
//       if (map.get(key).isEmpty) default else map(key).asInstanceOf[A]
//     }
    
//     def string(key: String, default: String): String = {
//       getOrElse[String](key, default)
//     }
        
//     def int(key: String, default: Int): Int = {
//       getOrElse[Int](key, default)
//     }
    
//     def double(key: String, default: Double): Double = {
//       getOrElse[Double](key, default)
//     }
    
//     def boolean(key: String, default: Boolean): Boolean = {
//       getOrElse[Boolean](key, default)
//     }

//     def jsvalue(key: String, default: JsValue): JsValue = {
//       getOrElse[JsValue](key, default)
//     }
    
    
//     def getOrElseOpt[A](key: String, default: Option[A] = None): Option[A] = {
//       if (map.get(key).isEmpty) default
//       else map.get(key) map ( _.asInstanceOf[A] ) 
//     }
    
//     def stringOpt(key: String, default: Option[String] = None) = {
//       getOrElseOpt[String](key, default)
//     }
    
//     def booleanOpt(key: String, default: Option[Boolean] = None) = {
//       getOrElseOpt[Boolean](key, default)
//     }
    
//     def jsvalueOpt(key: String, default: Option[JsValue] = None): Option[JsValue] = {
//       getOrElseOpt[JsValue](key, default)
//     }    
//   }
  
//   object ValueMap {
//     implicit class Optable[A](v: A) {
//       def asOpt = Option(v)
//     }
//   }
  
//   def getOrElse[A](map: Map[String,Any], key: String, default: A): A = {
//     if (map.get(key).isEmpty) default else map(key).asInstanceOf[A]
//   }
//   def getString(map: Map[String,Any], key: String, default: String): String = {
//     getOrElse[String](map, key, default)
//   }
//   def getInt(map: Map[String,Any], key: String, default: Int): Int = {
//     getOrElse[Int](map, key, default)
//   }
  
//   "getOrElse[A]" should {
    
//     "return the value as an A when it is contained in the Map" in {
//       val m = Map(
//           "int" -> 1, 
//           "double" -> 2.5,
//           "float" -> 1.7f,
//           "boolean" -> true, 
//           "string" -> "foo",
//           "seq_string" -> Seq("a", "b", "c"),
//           "seq_int" -> Seq(1,2,3))
      
//       val r1 = getOrElse[Int](m, "int", 0)
//       val r2 = getOrElse[Double](m, "double", 0.0)
//       val r3 = getOrElse[Float](m, "float", 0f)
//       val r4 = getOrElse[Boolean](m, "boolean", false)
//       val r5 = getOrElse[String](m, "string", "bar")
//       val r6 = getOrElse[Seq[String]](m, "seq_string", Seq("none"))
//       val r7 = getOrElse[Seq[Int]](m, "seq_int", Seq(0))
 
//       r1 === 1
//       r2 === 2.5
//       r3 === 1.7f
//       r4 === true
//       r5 === "foo"
//       r6 === Seq("a", "b", "c")
//       r7 === Seq(1, 2, 3)
//     }
    
//     "return the default as an A when key is not in the Map" in {
//       val m: Map[String,Any] = Map.empty
      
//       val r1 = getOrElse[Int](m, "int", 0)
//       val r2 = getOrElse[Double](m, "double", 0.0)
//       val r3 = getOrElse[Float](m, "float", 0f)
//       val r4 = getOrElse[Boolean](m, "boolean", false)
//       val r5 = getOrElse[String](m, "string", "bar")
//       val r6 = getOrElse[Seq[String]](m, "seq_string", Seq("none"))
//       val r7 = getOrElse[Seq[Int]](m, "seq_int", Seq(0))
 
//       r1 === 0
//       r2 === 0.0
//       r3 === 0f
//       r4 === false
//       r5 === "bar"
//       r6 === Seq("none")
//       r7 === Seq(0)
//     }
    
//     "fail when the value does not match the type parameter" in {
//       getOrElse[Int](Map("not_int" -> "foo"), "not_int", 0) must throwA[ClassCastException]
//     }
    
//   }
  
  
//   def newLaserLambda(lambdap: Map[String,Any], artifactp: Map[String,Any]) = {
    
//     val lvm = new ValueMap(lambdap)
//     val avm = new ValueMap(artifactp)
    
//     LaserLambda(
//       id          = avm.stringOpt("id", Option(UUID.randomUUID)),
//       eventFilter = avm.stringOpt("eventFilter"),
//       public      = avm.boolean("public", false),
//       provider    = avm.jsvalueOpt("provider"),
//       artifactDescription = LaserArtifactDescription(
//           artifactUri  = avm.stringOpt("artifactUri"),
//           runtime      = avm.string("runtime", "scala"),
//           handler      = avm.string("handler", "foo.js;foo"),
//           compressed   = avm.boolean("compressed", true),
//           memorySize   = avm.int("memorySize", 512),
//           cpus         = avm.double("cpus", 0.2),
//           description  = avm.stringOpt("description"),
//           publish      = avm.boolean("publish", false),
//           role         = avm.string("role", "none"),
//           timeoutSecs  = avm.int("timeoutSecs", 30),
//           code         = avm.stringOpt("code") ) )
      
//   }
    
// }