package com.galacticfog.gestalt.meta.api.output


import java.util.UUID

import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.providers.{ProviderEnv, ProviderMap}
import com.galacticfog.gestalt.meta.test._
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll
import play.api.libs.json.JsValue.jsValueToJsLookup
import play.api.libs.json.{JsObject, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.{PlaySpecification, WithApplication}
import play.api.inject.bind
import services.{DockerClientFactory, MarathonClientFactory, SkuberFactory}

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp}
import org.specs2.execute.Result
import scala.util.{Try,Success,Failure}


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
      
      val failed = results.filter { case (k, v) => v.isFailure }
      
      if (failed.nonEmpty) {
      failed.foreach { case (k,v) => println(s"FAILED: ${k}") }
    }
    
    failed.isEmpty === true      
    }
    
  }

}