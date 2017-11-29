package com.galacticfog.gestalt.meta.api.patch

import java.util.UUID

import com.galacticfog.gestalt.data.{PropertyFactory, TypeFactory}
import com.galacticfog.gestalt.data.bootstrap.{SystemType, TypeProperty}
import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.output.gestaltTypePropertyFormat
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.patch.PatchOp

import play.api.libs.json.Json

class PatchPropertySpec extends MetaRepositoryOps {
  
  "applyPatch" should {
    
    "successfully apply patch-ops to TypeProperties" >> {
     
      val id = UUID.randomUUID
      val name = id.toString
      val property = UUID.randomUUID.toString
      
      /*
       * Create new type with property and write to database. 
       */
      SystemType(
          dummyRootOrgId, dummyOwner,
          typeId   = id,
          typeName = name
      ).withTypeProperties(
        TypeProperty(property, "string")
      ).save()      
      
      val tpe = TypeFactory.findById(id)
      tpe must beSome
      
      val p1 = PropertyFactory.findByName(id, property)
      p1 must beSome
      p1.get.isSealed === false
      p1.get.isSystem === false
      p1.get.name === property
     
      val newname = UUID.randomUUID.toString
      val patch = PatchDocument(
          PatchOp.Replace("/isSealed", true),
          PatchOp.Replace("/isSystem", true),
          PatchOp.Replace("/name", newname))
      
      val result = PatchProperty.applyPatch(p1.get, patch)
      
      result must beSuccessfulTry
      
      val p2 = Js.parse[GestaltTypeProperty](result.get)(gestaltTypePropertyFormat)
      p2 must beSuccessfulTry
      
      println(Json.prettyPrint(result.get))
      
      p2.get.name === newname
      p2.get.isSealed === true
      p2.get.isSystem === true
    }
    
  }
  
}