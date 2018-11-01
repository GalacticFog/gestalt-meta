package com.galacticfog.gestalt.meta.api.output




import com.galacticfog.gestalt.meta.api.sdk.{ResourceLabel, ResourceIds}
import com.galacticfog.gestalt.meta.test._
import play.api.test.PlaySpecification

import com.galacticfog.gestalt.data._
import scala.util.Try


class OutputSpec extends PlaySpecification with MetaRepositoryOps {
  
  sequential
  
  override def beforeAll(): Unit = { pristineDatabase() }
  
  "renderResourceTypeOutput" should {
    
    "render all resource-types successfully" >> {
      val results = {
        TypeFactory.findAll.map { tpe =>
          (tpe.id, Try(Output.renderResourceTypeOutput(tpe, None)))
        }
      }
      
      val t = Try(Output.renderResourceTypeOutput(TypeFactory.findById(ResourceIds.RuleLimit).get, None))
      val failed = results.filter { case (k, v) => v.isFailure }
      
      if (failed.nonEmpty) {
      failed.foreach { case (k,v) => println(s"FAILED: ${ResourceLabel(k)}") }
    }
    
    failed.isEmpty === true      
    }
    
  }

}