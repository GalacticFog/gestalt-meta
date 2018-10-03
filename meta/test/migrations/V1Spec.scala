package migrations

import com.galacticfog.gestalt.data.PropertyFactory
import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.data.bootstrap.SystemType
import com.galacticfog.gestalt.data.bootstrap.TypeProperty
import com.galacticfog.gestalt.meta.api.errors.ConflictException
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps

import play.api.test.PlaySpecification

class V1Spec extends PlaySpecification with MetaRepositoryOps {
  
  "V1" >> {
    
    val v = new V1()
    
    "assert" should {
      
      "succeed when the predicate evaluates to TRUE" >> {
        v.assert(true)("not good") must not(throwA[RuntimeException])
      }
      
      "throw an exception when predicate evaluates to FALSE" >> {
        v.assert(false)("not good") must throwA[RuntimeException]
      }  
    }
    
    "verifyClonedProps" should {
      "succeed when 'expected' and 'given' match exactly" >> {
        v.verifyClonedProps(Seq("a","b","c"), Seq("a","b","c")) must not(throwA[RuntimeException])
      }
      
      "throw an exception if there is a difference between 'expected' and 'given'" >> {
        v.verifyClonedProps(Seq("a"), Seq("a","b","c")) must throwA[RuntimeException]
        v.verifyClonedProps(Seq("a", "b", "c"), Seq("b")) must throwA[RuntimeException]
      }
    }
    
    "removePropertiesFromType" should {
      
      "succeed when all of the name properties are removed from the type" >> {
        
        val typeId = uuid
        val prop1 = uuid.toString
        val prop2 = uuid.toString
        val prop3 = uuid.toString
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = typeId,
            typeName = typeId.toString
        ).withTypeProperties(
          TypeProperty(prop1, "string"),
          TypeProperty(prop2, "string"),
          TypeProperty(prop3, "string")
        ).save()      
    
        val thetype = TypeFactory.findById(typeId)
        thetype must beSome
        
        val props = PropertyFactory.findByType(typeId)
        props.size === 3
        props.exists(_.name == prop1) === true
        props.exists(_.name == prop2) === true
        props.exists(_.name == prop3) === true
        
        v.removePropertiesFromType(typeId, Seq(prop1, prop2, prop3)) must beSuccessfulTry

        val propsAfter = PropertyFactory.findByType(typeId)
        propsAfter.size === 0
        propsAfter.exists(_.name == prop1) === false
        propsAfter.exists(_.name == prop2) === false
        propsAfter.exists(_.name == prop3) === false
      }
    }
    
    "cloneProperties" should {
      
      "succeed when all named properties are copied from type-1 to type-2" >> {
        val typeId = uuid
        val prop1 = uuid.toString
        val prop2 = uuid.toString
        val prop3 = uuid.toString
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = typeId,
            typeName = typeId.toString
        ).withTypeProperties(
          TypeProperty(prop1, "string"),
          TypeProperty(prop2, "string"),
          TypeProperty(prop3, "string")
        ).save()      
    
        val thetype = TypeFactory.findById(typeId)
        thetype must beSome
        
        val props = PropertyFactory.findByType(typeId)
        props.size === 3
        props.exists(_.name == prop1) === true
        props.exists(_.name == prop2) === true
        props.exists(_.name == prop3) === true
        
        
        val type2 = uuid
        SystemType(
          dummyRootOrgId, dummyOwner,
          typeId   = type2,
          typeName = type2.toString
        ).save()
        
        val target = TypeFactory.findById(type2)
        target  must beSome
        
        PropertyFactory.findByType(type2).isEmpty === true
        
        val clone = v.cloneProperties(dummyRootOrgId, target.get, props)
        clone.nonEmpty === true
        clone.size === props.size
        clone.exists(_.isFailure) === false
        
        val propsAfter = PropertyFactory.findByType(type2)
        propsAfter.size === 3
        //
        // Test that all 3 props exist on new type AND were assigned to the new type.
        //
        propsAfter.exists(p => p.name == prop1 && p.appliesTo == type2) === true
        propsAfter.exists(p => p.name == prop2 && p.appliesTo == type2) === true
        propsAfter.exists(p => p.name == prop3 && p.appliesTo == type2) === true

      }
      
      //
      // Type 2 already has a property with same name as a clone property.
      //
      "fail if there is a property conflict" >> {
        val typeId = uuid
        val prop1 = uuid.toString
        val prop2 = uuid.toString
        val prop3 = uuid.toString
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = typeId,
            typeName = typeId.toString
        ).withTypeProperties(
          TypeProperty(prop1, "string"),
          TypeProperty(prop2, "string"),
          TypeProperty(prop3, "string")
        ).save()      
    
        val thetype = TypeFactory.findById(typeId)
        thetype must beSome
        
        val props = PropertyFactory.findByType(typeId)
        props.size === 3
        props.exists(_.name == prop1) === true
        props.exists(_.name == prop2) === true
        props.exists(_.name == prop3) === true
        
        
        val type2 = uuid
        SystemType(
          dummyRootOrgId, dummyOwner,
          typeId   = type2,
          typeName = type2.toString
        ).withTypeProperties(
          TypeProperty(prop1, "string") // <- This property should cause a conflict on clone.          
        ).save()
        
        val target = TypeFactory.findById(type2)
        target  must beSome
        
        PropertyFactory.findByType(type2).exists(_.name == prop1)
        
        v.cloneProperties(dummyRootOrgId, target.get, props) must throwA[ConflictException]
      }
    }
    
    //updateInstancePropertyNames(identity: UUID, rss: Seq[GestaltResourceInstance], propMap: Map[String, String])
    "updateInstancePropertyNames" should {
      
      "update the name of one or more properties on all given instances" >> {

        val typeId = uuid
        val prop1 = uuid.toString
        val prop2 = uuid.toString
        val prop3 = uuid.toString
        
        SystemType(
            dummyRootOrgId, dummyOwner,
            typeId   = typeId,
            typeName = typeId.toString
        ).withTypeProperties(
          TypeProperty(prop1, "string"),
          TypeProperty(prop2, "string"),
          TypeProperty(prop3, "string")
        ).save()      
    
        val thetype = TypeFactory.findById(typeId)
        thetype must beSome
        
        val props = PropertyFactory.findByType(typeId)
        props.size === 3
        props.exists(_.name == prop1) === true
        props.exists(_.name == prop2) === true
        props.exists(_.name == prop3) === true
        
        val instanceProps = Map(
            prop1 -> "a", 
            prop2 -> "b", 
            prop3 -> "c"
         )
        
        val instance1 = createInstance(typeId, uuid.toString, properties = Some(instanceProps)).get
        val instance2 = createInstance(typeId, uuid.toString, properties = Some(instanceProps)).get
        val instance3 = createInstance(typeId, uuid.toString, properties = Some(instanceProps)).get
        
        instance1.properties.get.contains(prop1) === true
        instance1.properties.get.contains(prop2) === true
        instance1.properties.get.contains(prop3) === true
         
        val newProp1Name = uuid.toString
        val newProp3Name = uuid.toString
        val newNameMap = Map(prop1 -> newProp1Name, prop3 -> newProp3Name)
        
        val updated = v.updateInstancePropertyNames(Seq(instance1, instance2, instance3), newNameMap)
        
        val newInstance1 = updated.find(_.id == instance1.id).get
        val newInstance2 = updated.find(_.id == instance2.id).get
        val newInstance3 = updated.find(_.id == instance3.id).get
        
        newInstance1.properties.get.size === 3
        newInstance1.properties.get.get(prop1) must beNone // <-- we renamed to newProp1Name
        newInstance1.properties.get.get(prop3) must beNone // <-- we renamed to newProp3Name
        newInstance1.properties.get.get(prop2) must beSome
        newInstance1.properties.get.get(newProp1Name) must beSome
        newInstance1.properties.get.get(newProp3Name) must beSome
        
        newInstance2.properties.get.size === 3
        newInstance2.properties.get.get(prop1) must beNone // <-- we renamed to newProp1Name
        newInstance2.properties.get.get(prop3) must beNone // <-- we renamed to newProp3Name
        newInstance2.properties.get.get(prop2) must beSome
        newInstance2.properties.get.get(newProp1Name) must beSome
        newInstance2.properties.get.get(newProp3Name) must beSome
        
        newInstance3.properties.get.size === 3
        newInstance3.properties.get.get(prop1) must beNone // <-- we renamed to newProp1Name
        newInstance3.properties.get.get(prop3) must beNone // <-- we renamed to newProp3Name
        newInstance3.properties.get.get(prop2) must beSome
        newInstance3.properties.get.get(newProp1Name) must beSome
        newInstance3.properties.get.get(newProp3Name) must beSome        
      }
    }
  } 
  
}
