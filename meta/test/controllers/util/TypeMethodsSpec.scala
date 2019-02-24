package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.TypeFactory
import com.galacticfog.gestalt.data.bootstrap._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.sdk
import com.galacticfog.gestalt.meta.api.sdk.{GestaltTypePropertyInput, ResourceIds}
import com.galacticfog.gestalt.meta.test._
import controllers.PropertyController
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.PlaySpecification
import play.api.libs.json._


class TypeMethodsSpec extends PlaySpecification with MetaRepositoryOps {
    
  "getParentTypes" should {
      
    "return a list of parent types if they exist" >> {
      val providerTypeId = uuid()
      val parentTypes = Seq(ResourceIds.Org, ResourceIds.Workspace, ResourceIds.Environment)
      
      val newtype = SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = providerTypeId,
        typeName = providerTypeId.toString,
        extend = Some(ResourceIds.Resource)
      ).withLineageInfo(
          
        LineageInfo(parent_types = parentTypes)
        
      ).save()        
        
      val test = TypeMethods.getParentTypes(newtype)
      test.sorted === parentTypes.sorted
    }
    
    "return an empty list of there are none" >> {
            
      val newtype = SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = uuid(),
        typeName = "brand-new-type"
      ).save()

      TypeMethods.getParentTypes(newtype).isEmpty === true
    }

//    "fail if any of the values are invalid UUIDs" >> {
//      val input = Json.parse(s"""
//        |{
//        |  "name": "my-new-type",
//        |  "properties": {
//        |    "lineage": {
//        |      "parent_types": [ "${uuid.toString}", "foo", "${uuid.toString}" ]
//        |    }
//        |  }
//        |}
//        """.stripMargin)
//        
//        val org = dummyRootOrgId
//        val owner = UUID.fromString(dummyOwner.id)
//        val newtype = { 
//          val t = TypeMethods.payloadToResource(org, owner, input)
//          t must beSuccessfulTry
//          t.get
//        }
//        TypeMethods.getParentTypes(newtype).isEmpty === true
//    }
    
  }

  "addChildTypes" should {
    
    "add new uuids to `type.properties.lineage.child_types`" >> {
      val parentType = {
        val t = TypeFactory.findById(ResourceIds.Workspace)
        t must beSome
        t.get
      }
      val newchild1 = uuid()
      val newchild2 = uuid()
      
      val userid = UUID.fromString(dummyOwner.id)
      val result = TypeMethods.addChildTypes(userid, parentType, Seq(newchild1, newchild2))
      result must beSuccessfulTry
      
      val test = {
        val t = TypeFactory.findById(ResourceIds.Workspace)
        t must beSome
        val st = SystemType.fromResourceType(t.get)
        st.lineage.get.child_types.get
      }
      test.contains(newchild1) === true
      test.contains(newchild2) === true
    }
    
    "succeed if `properties.lineage` does not exist" >> {
      val parentTypeId = uuid()
      
      SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = parentTypeId,
        typeName = parentTypeId.toString
      ).save()
      
      val parentType = {
        val t = TypeFactory.findById(parentTypeId)
        t must beSome
        t.get
      }
      
      SystemType.fromResourceType(parentType).lineage must beNone
      
      val newchild1 = uuid()
      val newchild2 = uuid()
      
      val userid = UUID.fromString(dummyOwner.id)
      TypeMethods.addChildTypes(userid, parentType, Seq(newchild1, newchild2)) must beSuccessfulTry
      
      val test = {
        val t = TypeFactory.findById(parentTypeId)
        t must beSome
        val st = SystemType.fromResourceType(t.get)
        st.lineage.get.child_types.get
      }
      
      test.contains(newchild1) === true
      test.contains(newchild2) === true
    }
    
    "succeed if `properties.lineage.child_types` does not exist" >> {
      val parentTypeId = uuid()
      
      SystemType(
        dummyRootOrgId, dummyOwner,
        typeId   = parentTypeId,
        typeName = parentTypeId.toString
      ).withLineageInfo(
          LineageInfo(parent_types = Seq.empty, child_types = None)
      ).save()
      
      val parentType = {
        val t = TypeFactory.findById(parentTypeId)
        t must beSome
        t.get
      }
      
      SystemType.fromResourceType(parentType).lineage.get.child_types must beNone
      
      val newchild1 = uuid()
      val newchild2 = uuid()
      
      val userid = UUID.fromString(dummyOwner.id)
      TypeMethods.addChildTypes(userid, parentType, Seq(newchild1, newchild2)) must beSuccessfulTry
      
      val test = {
        val t = TypeFactory.findById(parentTypeId)
        t must beSome
        val st = SystemType.fromResourceType(t.get)
        st.lineage.get.child_types.get
      }
      
      test.contains(newchild1) === true
      test.contains(newchild2) === true
    }
  }
  
  "makeNewParents" should {
    
    "add the given child-id to each resource-type named in 'parents'" >> {
      
      val childId = uuid()
      
      val w1 = SystemType.fromResourceType(TypeFactory.findById(ResourceIds.Workspace).get)
      val e1 = SystemType.fromResourceType(TypeFactory.findById(ResourceIds.Environment).get)
      
      w1.lineage.get.child_types.get.contains(childId) === false
      e1.lineage.get.child_types.get.contains(childId) === false
      
      val user = UUID.fromString(dummyOwner.id)
      val results = TypeMethods.makeNewParents(user, childId, Seq(ResourceIds.Workspace, ResourceIds.Environment))
      
      results.exists(_.isFailure) === false
      
      val w2 = SystemType.fromResourceType(TypeFactory.findById(ResourceIds.Workspace).get)
      val e2 = SystemType.fromResourceType(TypeFactory.findById(ResourceIds.Environment).get)
      
      w2.lineage.get.child_types.get.contains(childId) === true
      e2.lineage.get.child_types.get.contains(childId) === true
    }
  }

  def properties(actions: JsValue, api: JsValue, lineage: JsValue) = {
    
  }
  
  def mkactions(prefix: String, verbs: Seq[String] = Seq.empty) = {
    Json.obj("prefix" -> prefix, "verbs" -> verbs)
  }
  
  def mkapi(restName: String) = {
    Json.obj("api" -> Json.obj("rest_name" -> restName))
  }
  
  def mklineage(parentTypes: Seq[UUID], childTypes: Seq[UUID] = Seq.empty) = {
    Json.obj("parent_types" -> parentTypes, "child_types" -> childTypes)
  }
  

  
  "validateCreatePayload" should {
    
    "accept valid data" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      "prefix": "${uuid.toString}",
        |      "verbs": []
        |    },
        |    "api": {
        |      "rest_name": "${uuid.toString}"
        |    },
        |    "lineage": {
        |      "parent_types": ["${ResourceIds.Org}"],
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      TypeMethods.validateCreatePayload(payload) must beSuccessfulTry
    }
    
    "fail if actions.prefix is missing" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      
        |    },
        |    "api": {
        |      "rest_name": "${uuid.toString}"
        |    },
        |    "lineage": {
        |      "parent_types": ["${ResourceIds.Org}"],
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      TypeMethods.validateCreatePayload(payload) must beFailedTry
    }
    
    "fail if actions.prefix is null" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      "prefix": null
        |    },
        |    "api": {
        |      "rest_name": "${uuid.toString}"
        |    },
        |    "lineage": {
        |      "parent_types": ["${ResourceIds.Org}"],
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      TypeMethods.validateCreatePayload(payload) must beFailedTry
    }
    
    "fail if actions.prefix is empty" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      "prefix": ""
        |    },
        |    "api": {
        |      "rest_name": "${uuid.toString}"
        |    },
        |    "lineage": {
        |      "parent_types": ["${ResourceIds.Org}"],
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      TypeMethods.validateCreatePayload(payload) must beFailedTry
    }
    
    "fail if actions.prefix is not globally unique" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      "prefix": "environment"
        |    },
        |    "api": {
        |      "rest_name": "${uuid.toString}"
        |    },
        |    "lineage": {
        |      "parent_types": ["${ResourceIds.Org}"],
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      TypeMethods.validateCreatePayload(payload) must beFailedTry.withThrowable[ConflictException]
    }
    
    "fail if api.rest_name is missing" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      "prefix": "${uuid.toString}",
        |      "verbs": []
        |    },
        |    "lineage": {
        |      "parent_types": ["${ResourceIds.Org}"],
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      TypeMethods.validateCreatePayload(payload) must beFailedTry
    }
    
    "fail if api.rest_name is not globally unique" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      "prefix": "${uuid.toString}",
        |      "verbs": []
        |    },
        |    "api": {
        |      "rest_name": "environments"
        |    },
        |    "lineage": {
        |      "parent_types": ["${ResourceIds.Org}"],
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      TypeMethods.validateCreatePayload(payload) must beFailedTry.withThrowable[ConflictException]
    }
    
    "fail if lineage.parent_types is missing" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      "prefix": "${uuid.toString}",
        |      "verbs": []
        |    },
        |    "api": {
        |      "rest_name": "${uuid.toString}"
        |    },
        |    "lineage": {
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      TypeMethods.validateCreatePayload(payload) must beFailedTry.withThrowable[BadRequestException]
    }
    
    "fail if lineage.parent_types is empty" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      "prefix": "${uuid.toString}",
        |      "verbs": []
        |    },
        |    "api": {
        |      "rest_name": "${uuid.toString}"
        |    },
        |    "lineage": {
        |      "parent_types": [],
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      TypeMethods.validateCreatePayload(payload) must beFailedTry.withThrowable[BadRequestException]
    }
    
    "add 'actions.verbs' if missing from payload" >> {
      val payload = Json.parse(      
        s"""
        |{
        |  "name": "",
        |  "properties": {
        |    "actions": {
        |      "prefix": "${uuid.toString}"
        |    },
        |    "api": {
        |      "rest_name": "${uuid.toString}"
        |    },
        |    "lineage": {
        |      "parent_types": ["${ResourceIds.Org}"],
        |      "child_types": []
        |    }
        |  }
        |}    
        """.stripMargin)
      
      val json = TypeMethods.validateCreatePayload(payload) 
      json must beSuccessfulTry
    }    
    
    "PROVIDER TYPES ONLY" >> {
      
      "accept duplicate actions.prefix value of 'provider'" >> {
        val payload = Json.parse(      
          s"""
          |{
          |  "name": "",
          |  "properties": {
          |    "actions": {
          |      "prefix": "provider",
          |      "verbs": []
          |    },
          |    "api": {
          |      "rest_name": "${uuid.toString}"
          |    },
          |    "lineage": {
          |      "parent_types": ["${ResourceIds.Org}"],
          |      "child_types": []
          |    }
          |  }
          |}    
          """.stripMargin)
        
        TypeMethods.validateCreatePayload(payload) must beSuccessfulTry
      }
      
      "accept duplicate api.rest_name value of 'providers'" >> {
        val payload = Json.parse(      
          s"""
          |{
          |  "name": "",
          |  "properties": {
          |    "actions": {
          |      "prefix": "provider",
          |      "verbs": []
          |    },
          |    "api": {
          |      "rest_name": "providers"
          |    },
          |    "lineage": {
          |      "parent_types": ["${ResourceIds.Org}"],
          |      "child_types": []
          |    }
          |  }
          |}    
          """.stripMargin)
        
        TypeMethods.validateCreatePayload(payload) must beSuccessfulTry
      }
      
      "ignore missing api.rest_name value" >> {
        val payload = Json.parse(      
          s"""
          |{
          |  "name": "",
          |  "properties": {
          |    "actions": {
          |      "prefix": "provider",
          |      "verbs": []
          |    },
          |    "lineage": {
          |      "parent_types": ["${ResourceIds.Org}"],
          |      "child_types": []
          |    }
          |  }
          |}    
          """.stripMargin)
        
        TypeMethods.validateCreatePayload(payload) must beSuccessfulTry
      }
    }
  }
  
/*
  def typeIsProvider(json: JsValue, prefix: Option[String], restName: Option[String]): Boolean = {
    Js.find(json.as[JsObject], "/extend").fold(false) { ext =>
      val issubtype = ResourceFactory.isSubTypeOf(UUID.fromString(ext.as[String]), ResourceIds.Provider)
      if (issubtype) {
        val goodprefix = prefix.fold(true)(_.trim.toLowerCase == "provider")
        val goodrestname = restName.fold(true)(_.trim.toLowerCase == "providers")
        goodprefix && goodrestname
      } else false
    }
  }  
 */
  
  "typeIsProvider" should {
    
    /*
     * 'good data' here means:
     * - the type extends a subtype of Provider
     * - actions.prefix is missing or set to 'provider'
     * - api.rest_name is missing or set to 'providers'
     */
    "return TRUE when given good data" >> {
        val payload = Json.parse(      
          s"""
          |{
          |  "name": "",
          |  "extend": "${ResourceIds.Provider}",
          |  "properties": {
          |    "actions": {
          |      "prefix": "provider",
          |      "verbs": []
          |    },
          |    "api": {
          |      "rest_name": "providers"
          |    },
          |    "lineage": {
          |      "parent_types": ["${ResourceIds.Org}"],
          |      "child_types": []
          |    }
          |  }
          |}    
          """.stripMargin).as[JsObject]
        
        TypeMethods.typeIsProvider(payload, Some("provider"), Some("providers")) === true      
    }
    
    "return FALSE when type does NOT extend Provider" >> {
        val payload = Json.parse(      
          s"""
          |{
          |  "name": "",
          |  "properties": {
          |    "actions": {
          |      "prefix": "provider",
          |      "verbs": []
          |    },
          |    "api": {
          |      "rest_name": "providers"
          |    },
          |    "lineage": {
          |      "parent_types": ["${ResourceIds.Org}"],
          |      "child_types": []
          |    }
          |  }
          |}    
          """.stripMargin).as[JsObject]
        
        TypeMethods.typeIsProvider(payload, Some("provider"), Some("providers")) === false      
    }    
    
  }

  "GestaltTypePropertyInput" should {

    "parse refers_to as a valid typename" >> {
      val json = Json.parse(
        s"""{
          |  "data_type": "resource::uuid",
          |  "name": "container_id",
          |  "public": true,
          |  "refers_to": "${sdk.Resources.Container}",
          |  "requirement_type": "optional",
          |  "visibility_type": "plain"
          |}
        """.stripMargin
      ).as[JsObject]
      val JsSuccess(input, _) = PropertyController.normalizeInputProperty(uuid(), json).validate[GestaltTypePropertyInput]
      input.refers_to must beSome(ResourceIds.Container)
    }

    "parse refers_to as a UUID" >> {
      val json = Json.parse(
        s"""{
           |  "data_type": "resource::uuid",
           |  "name": "container_id",
           |  "public": true,
           |  "refers_to": "${ResourceIds.Container}",
           |  "requirement_type": "optional",
           |  "visibility_type": "plain"
           |}
        """.stripMargin
      ).as[JsObject]
      val JsSuccess(input, _) = PropertyController.normalizeInputProperty(uuid(), json).validate[GestaltTypePropertyInput]
      input.refers_to must beSome(ResourceIds.Container)
    }

    "return helpful message if refers_to parses to a non-existent type name" >> {
      val json = Json.parse(
        s"""{
           |  "data_type": "resource::uuid",
           |  "name": "container_id",
           |  "public": true,
           |  "refers_to": "Invalid::Type::Name",
           |  "requirement_type": "optional",
           |  "visibility_type": "plain"
           |}
        """.stripMargin
      ).as[JsObject]
      PropertyController.normalizeInputProperty(uuid(), json).validate[GestaltTypePropertyInput] must throwAn[BadRequestException]("error.expected.existing-resource-type-name")
    }

    "return helpful message if refers_to parses to a non-existent type id" >> {
      val json = Json.parse(
        s"""{
           |  "data_type": "resource::uuid",
           |  "name": "container_id",
           |  "public": true,
           |  "refers_to": "${uuid()}",
           |  "requirement_type": "optional",
           |  "visibility_type": "plain"
           |}
        """.stripMargin
      ).as[JsObject]
      PropertyController.normalizeInputProperty(uuid(), json).validate[GestaltTypePropertyInput] must throwAn[BadRequestException]("error.expected.existing-resource-type-id")
    }

    "return helpful message if refers_to does not parse as string or UUID" >> {
      val json = Json.parse(
        s"""{
           |  "data_type": "resource::uuid",
           |  "name": "container_id",
           |  "public": true,
           |  "refers_to": 0,
           |  "requirement_type": "optional",
           |  "visibility_type": "plain"
           |}
        """.stripMargin
      ).as[JsObject]
      PropertyController.normalizeInputProperty(uuid(), json).validate[GestaltTypePropertyInput] must throwAn[BadRequestException]("error.expected.uuid-type-id-or-string-type-name")
    }

  }
  
}
