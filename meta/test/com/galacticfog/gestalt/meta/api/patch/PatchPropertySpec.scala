package com.galacticfog.gestalt.meta.api.patch

import java.util.UUID

import com.galacticfog.gestalt.data.{PropertyFactory, TypeFactory}
import com.galacticfog.gestalt.data.bootstrap.{SystemType, TypeProperty}
import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.json.Js
import com.galacticfog.gestalt.meta.api.output.gestaltTypePropertyFormat
import com.galacticfog.gestalt.meta.test._//MetaRepositoryOps
import com.galacticfog.gestalt.patch.PatchDocument
import com.galacticfog.gestalt.patch.PatchOp
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds



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
      
      p2.get.name === newname
      p2.get.isSealed === true
      p2.get.isSystem === true
    }    
  }
  
  "swapValue" should {
    
    "use the provided function to swap the PatchOp.value" >> {
      val op1 = PatchOp.Replace("/foo", "alpha")
      
      val newvalue = "bravo"
      val op2 = PatchProperty.swapValue(op1)(_ => newvalue)
      op2.value must beSome
      op2.value.get.as[String] === newvalue.toString
    }
    
    "throw an exception if the given function fails" >> {
            val op1 = PatchOp.Replace("/foo", "alpha")
      
      val expectedMessage = "@ERROR_MESSAGE@"
      val badFunction = (s: String) => throw new RuntimeException(expectedMessage)
      
      (PatchProperty.swapValue(op1)(badFunction) must throwA[BadRequestException])
        .message must contain(expectedMessage)
    } 
  }
  
  "validateOp" should {
    
    "NOT allow 'add' operations against root (first-level) keys" >> {
      PatchProperty.validateOp(PatchOp.Add("/foo", "bar")) must throwA[BadRequestException]
    }
    
    "NOT allow 'remove' operations against root (first-level) keys" >> {
      PatchProperty.validateOp(PatchOp.Remove("/foo")) must throwA[BadRequestException]
    }
    
    "allow 'add' operations on keys beneath '/properties'" >> {
      val op = PatchOp.Add("/properties/foo", "bar")
      PatchProperty.validateOp(op) === op
    }
    
    "allow 'remove' operations on keys beneath '/properties'" >> {
      val op = PatchOp.Remove("/properties/foo")
      PatchProperty.validateOp(op) === op
    }
    
    "simply return if the op is neither 'add' nor 'remove'" >> {
      val op = PatchOp.Replace("/foo", "bar")
      PatchProperty.validateOp(op) === op
    }
  }
  
  "updateRefersTo" should {
    
    "convert a valid typename to a UUID" >> {
      
      // Create a type to reference
      val id = UUID.randomUUID
      val name = id.toString + "-name"
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
      
      val op1 = PatchOp.Add("/refers_to", name)
      val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
      val op2 = PatchProperty.updateRefersTo(op1, Seq(op1), dummyprop)
      
      op2.value.get.as[String] === id.toString
    }
    
    "throw an exception attempting to remove /refers_to and data_type is a reference" >> {
      
      // Create a type to reference
      val id = UUID.randomUUID
      val name = id.toString + "-name"
      val property = UUID.randomUUID.toString
      
      /*
       * Create new type with property and write to database. 
       */
      SystemType(
          dummyRootOrgId, dummyOwner,
          typeId   = id,
          typeName = name
      ).withTypeProperties(
        TypeProperty(property, "resource::uuid", refersTo = Some(ResourceIds.Environment))
      ).save()       
      
      val op1 = PatchOp.Remove("/refers_to")
      val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
      PatchProperty.updateRefersTo(op1, Seq(op1), dummyprop) must throwA[BadRequestException]
    }
    
    "succeed removing refers_to when there is an op to change data_type to a non-reference" >> {
      // Create a type to reference
      val id = UUID.randomUUID
      val name = id.toString + "-name"
      val property = UUID.randomUUID.toString
      
      /*
       * Create new type with property and write to database. 
       */
      SystemType(
          dummyRootOrgId, dummyOwner,
          typeId   = id,
          typeName = name
      ).withTypeProperties(
        TypeProperty(property, "resource::uuid", refersTo = Some(ResourceIds.Environment))
      ).save()       
      
      val op1 = PatchOp.Remove("/refers_to")
      val op2 = PatchOp.Replace("/data_type", "string")
      val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
      PatchProperty.updateRefersTo(op1, Seq(op1, op2), dummyprop) must not(throwA[BadRequestException])

    }
    
    "throw an exception when the given typename does not exist" >> {
      val op = PatchOp.Replace("/refers_to", "not::a::type")
      val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
      PatchProperty.updateRefersTo(op, Seq(op), dummyprop) must throwA[BadRequestException]
    }
    
    "throw an exception when given an op with a path other than 'refers_to'" >> {
      val op = PatchOp.Remove("/foo")
      val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
      PatchProperty.updateRefersTo(op, Seq(op), dummyprop) must throwA[RuntimeException]
    }
  }
  
  
  "updateDataType" should {
    
    //def updateDatatype(currentOp: PatchOp, allOps: Seq[PatchOp], prop: GestaltTypeProperty) = {
    
    "update the datatype value to a valid UUID" >> {
      
      val originalValue = "int"
      val op = PatchOp.Replace("/data_type", originalValue)
      val prop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
      
      val ops = PatchProperty.updateDatatype(op, Seq(op), prop)
      ops.size === 1
      
      val updatedOp = ops.find(_.path == "/datatype")
      updatedOp must beSome
      updatedOp.get.value.get.as[String] === DataType.id(originalValue).toString
    }
    
    /*
     * NON-REFERENCE => REFERENCE
     */
    "succeed if the caller supplies a valid 'remove refers_to' op" >> {
      // Create a type to reference
      val tid1 = UUID.randomUUID
      val tname1 = tid1.toString + "-name"
      
      val id = UUID.randomUUID
      val name = id.toString + "-name"
      val propertyName = UUID.randomUUID.toString
      
      /*
       * Create a type we can reference 
       */
      SystemType(
          dummyRootOrgId, dummyOwner,
          typeId   = tid1,
          typeName = tname1
      ).save()
      
      SystemType(
          dummyRootOrgId, dummyOwner,
          typeId   = id,
          typeName = name
      ).withTypeProperties(
          TypeProperty(propertyName, "string") // <- not a reference
      ).save()
      
      
      val prop = PropertyFactory.findByName(id, propertyName)
      prop must beSome
      
      val originalValue = "resource::uuid"
      val op = PatchOp.Replace("/data_type", originalValue) // <- this IS a reference. 
      val oplist = Seq(op, PatchOp.Add("/refers_to", tid1))
      
      val newops = PatchProperty.updateDatatype(op, oplist, prop.get)
      newops.size === 1
      
      val updatedOp = newops.find(_.path == "/datatype")
      updatedOp must beSome
      updatedOp.get.value.get.as[String] === DataType.id(originalValue).toString
    }
    
    /*
     * REFERENCE => NON-REFERENCE
     * This covers the scenario where the old datatype was a reference but the new one is not. That use-case
     * requires a 'remove' op for the refers_to attribute. The code should inject that op if it's not present.
     */
    "add a 'remove' op for refers_to when datatype changes from reference to non-reference and no 'remove' exists in the op list" >> {
      // Create a type to reference
      val tid1 = UUID.randomUUID
      val tname1 = tid1.toString + "-name"
      
      val id = UUID.randomUUID
      val name = id.toString + "-name"
      val propertyName = UUID.randomUUID.toString
      
      /*
       * Create a type we can reference 
       */
      SystemType(
          dummyRootOrgId, dummyOwner,
          typeId   = tid1,
          typeName = tname1
      ).save()
      
      SystemType(
          dummyRootOrgId, dummyOwner,
          typeId   = id,
          typeName = name
      ).withTypeProperties(
          TypeProperty(propertyName, "resource::uuid", refersTo = Some(tid1)) // <- this is a reference type
      ).save()
      
      
      val prop = PropertyFactory.findByName(id, propertyName)
      prop must beSome
      
      val op = PatchOp.Replace("/data_type", "string") // <- this is NOT a reference type. 
      val oplist = Seq(op)
      
      val newops = PatchProperty.updateDatatype(op, oplist, prop.get)
      newops.size === 2
      newops.exists( o => o.path == "/refersTo" && o.op == "remove") === true
    }
  }
  
  "normalizeOps" should {
    
    "visibility_type" should {
      
      "translate value to a UUID when given a valid visibility_type name" >> {
        val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
        val value = "hidden"
        val ps = PatchProperty.normalizeOps(Seq(PatchOp.Replace("/visibility_type", value)), dummyprop)
        val p = ps.find(_.path == "/visibilityType")
        p must beSome
        p.get.value.get.as[String] === VisibilityType.id(value).toString
      }
      
      "fail when given an invalid visibility_type name" >> {
        val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
        val value = "invalid_value"
        PatchProperty.normalizeOps(Seq(PatchOp.Replace("/visibility_type", value)), dummyprop) must throwA[BadRequestException]
      }
    }
    
    "requirement_type" should {
      "translate value to a UUID when given a valid requirement_type name" >> {
        val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
        val value = "optional"
        val ps = PatchProperty.normalizeOps(Seq(PatchOp.Replace("/requirement_type", value)), dummyprop)
        val p = ps.find(_.path == "/requirementType")
        p must beSome
        p.get.value.get.as[String] === RequirementType.id(value).toString
      }
      
      "fail when given an invalid requirement_type name" >> {
        val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
        val value = "invalid_value"
        PatchProperty.normalizeOps(Seq(PatchOp.Replace("/requirement_type", value)), dummyprop) must throwA[BadRequestException]
      }
    }
    
    "data_type" should {
    
      "translate value to a UUID when given a valid data_type name" >> {
        val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
        val value = "json"
        val ps = PatchProperty.normalizeOps(Seq(PatchOp.Replace("/data_type", value)), dummyprop)
        val p = ps.find(_.path == "/datatype")
        p must beSome
        p.get.value.get.as[String] === DataType.id(value).toString
      }
      
      "fail when given an invalid data_type name" >> {
        val dummyprop = TypeProperty("foo", "string").toGestalt(dummyRootOrgId, dummyOwner, ResourceIds.Org)
        val value = "invalid_value"
        PatchProperty.normalizeOps(Seq(PatchOp.Replace("/data_type", value)), dummyprop) must throwA[BadRequestException]
      }
      
      "add a remove op for reference to non-reference" >> {
        // Create a type to reference
        val tid1 = UUID.randomUUID
        val tname1 = tid1.toString + "-name"
        
        val id = UUID.randomUUID
        val name = id.toString + "-name"
        val propertyName = UUID.randomUUID.toString
        
        /*
         * Create a type we can reference 
         */
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = tid1,
            typeName = tname1
        ).save()
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = id,
            typeName = name
        ).withTypeProperties(
            TypeProperty(propertyName, "resource::uuid", refersTo = Some(tid1)) // <- this is a reference type
        ).save()
        
        
        val prop = PropertyFactory.findByName(id, propertyName)
        prop must beSome
        
        val op = PatchOp.Replace("/data_type", "string") // <- this is NOT a reference type. 
        val oplist = Seq(op)
        
        val normalized = PatchProperty.normalizeOps(Seq(op), prop.get)

        normalized.size === 2
        normalized.exists( o => o.path == "/refersTo" && o.op == "remove") === true
      }
    
      "ensure an 'add /refers_to' op is present for non-reference to reference" >> {
        val tid1 = UUID.randomUUID
        val tname1 = tid1.toString + "-name"
        
        val id = UUID.randomUUID
        val name = id.toString + "-name"
        val propertyName = UUID.randomUUID.toString
        
        /*
         * Create a type we can reference 
         */
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = tid1,
            typeName = tname1
        ).save()
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = id,
            typeName = name
        ).withTypeProperties(
            TypeProperty(propertyName, "string") // <- not a reference
        ).save()
        
        val prop = PropertyFactory.findByName(id, propertyName)
        prop must beSome
        
        val originalValue = "resource::uuid"
        val op = PatchOp.Replace("/data_type", originalValue) // <- this IS a reference. 

        PatchProperty.normalizeOps(Seq(op), prop.get) must throwA[BadRequestException]
      }
    }
    
  }
}