package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.data.{ResourceFactory, TypeFactory}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.laser.{LaserEndpoint, LaserLambda}
import com.galacticfog.gestalt.meta.api.ContainerSpec
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.{GestaltResourceInput, HostConfig, JsonClient, ResourceIds, ResourceStates}
import com.galacticfog.gestalt.meta.test._
import com.galacticfog.gestalt.patch.{PatchDocument, PatchOp, PatchOps}
import com.galacticfog.gestalt.security.api.GestaltSecurityConfig
import controllers.SecurityResources
import org.mockito.Matchers.{eq => meq}
import org.specs2.matcher.JsonMatchers
import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.{BeforeAll, Scope}
import play.api.inject.bind
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.libs.json.{JsBoolean, JsValue, Json}
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import play.api.test.{FakeRequest, PlaySpecification}
import play.api.mvc._
import play.api.mvc.Results._
import play.api.http.HttpVerbs._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.ws.WSResponse
import com.galacticfog.gestalt.laser._
import play.api.http.HttpVerbs

import scala.concurrent.Future
import scala.util.Success
import com.galacticfog.gestalt.data.bootstrap._


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

  "updateChildTypes" should {
    
    "add new uuids to `type.properties.lineage.child_types`" >> {
      val parentType = {
        val t = TypeFactory.findById(ResourceIds.Workspace)
        t must beSome
        t.get
      }
      val newchild1 = uuid()
      val newchild2 = uuid()
      
      val userid = UUID.fromString(dummyOwner.id)
      val result = TypeMethods.updateChildTypes(userid, parentType, Seq(newchild1, newchild2))
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
      TypeMethods.updateChildTypes(userid, parentType, Seq(newchild1, newchild2)) must beSuccessfulTry
      
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
      TypeMethods.updateChildTypes(userid, parentType, Seq(newchild1, newchild2)) must beSuccessfulTry
      
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
}
