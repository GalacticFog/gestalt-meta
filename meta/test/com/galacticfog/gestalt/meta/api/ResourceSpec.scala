package com.galacticfog.gestalt.meta.api

import java.util.UUID

import org.specs2.matcher.ValueCheck.typedValueCheck
import org.specs2.specification.BeforeAll
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceFactory.findById
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.errors.BadRequestException
import com.galacticfog.gestalt.meta.api.sdk.{ResourceIds, ResourceOwnerLink}
import com.galacticfog.gestalt.meta.test.ResourceScope
import controllers.ResourceController
import controllers.util.GestaltProviderMocking

import com.galacticfog.gestalt.meta.api.output.Output
import play.api.libs.json.{JsObject, Json}
import play.api.mvc.AnyContentAsEmpty
import play.api.test.FakeHeaders
import play.api.test.FakeRequest
import play.api.test.PlaySpecification
import play.api.test.WithApplication

class ResourceSpec extends PlaySpecification with ResourceScope with GestaltProviderMocking with BeforeAll {

  stopOnFail
  sequential


  lazy val owner = ResourceOwnerLink(ResourceIds.User, adminUserId)
  
  override def beforeAll() = pristineDatabase()

  "output" should {
    "render properties as non-null even if None" in {
      val json = Output.renderInstance(createInstance(
        typeId = ResourceIds.Group,
        name = "dummy-group",
        properties = None
      ).get)
      (json \ "properties").asOpt[JsObject] must beSome(Json.obj())
    }
  }

  "typeOrElse" should {

    "return the ResourceType ID for the given resource API name" in {

      // TODO: This list of valid types is not comprehensive
      Resource.typeOrElse("orgs") must haveClass[UUID]
      Resource.typeOrElse("workspaces") must haveClass[UUID]
      Resource.typeOrElse("environments") must haveClass[UUID]
      Resource.typeOrElse("containers") must haveClass[UUID]
      Resource.typeOrElse("providers") must haveClass[UUID]
    }

    "throw an exception when the type name doesn't exist" in {
      Resource.typeOrElse("invalid_type") must throwA[BadRequestException]
    }
  }

  "mapPathData" should {

    "succeed when the path points to an FQON" in { 
      
      val m = Resource.mapPathData("/alpha")

      m must beAnInstanceOf[Map[_, _]]
      m.size === 1
    }
    
    "succeed when the path points to a first-level resource" in {
      val id = uuid.toString
      val m = Resource.mapPathData(s"/alpha/bravo/${id}")

      m must beAnInstanceOf[Map[_, _]]
      m.size === 3
      m(Resource.Fqon) === "alpha"
      m(Resource.TargetType) === "bravo"
      m(Resource.TargetId) === id
    }

    "succeed when the path points to a second-level resource" in {

      val pid = uuid.toString
      val tid = uuid.toString
      val m = Resource.mapPathData(s"/fqon/workspaces/${pid}/environments/${tid}")

      m must beAnInstanceOf[Map[_, _]]
      m.size === 5
      m(Resource.Fqon) === "fqon"
      m(Resource.ParentType) === "workspaces"
      m(Resource.ParentId) === pid
      m(Resource.TargetType) === "environments"
      m(Resource.TargetId) === tid
    }

    "fail with Exception if the path is empty" in {
      Resource.mapPathData("") must throwAn[IllegalArgumentException]
    }

    "fail with Exception if any path component is empty" in {
      Resource.mapPathData("/alpha//beta") must throwAn[IllegalArgumentException]
    }

    "fail with Exception if the path length is even" in {
      Resource.mapPathData("/alpha/beta") must throwAn[IllegalArgumentException]
      Resource.mapPathData("/alpha/beta/charlie/delta") must throwAn[IllegalArgumentException]
    }

    "fail with Exception if the path points to a resource deeper than the second-level" in {
      Resource.mapPathData("/fqon/workspaces/123/environments/456/lambdas/789") must throwAn[IllegalArgumentException]
    }
    
    "fail if any of the IDs are not valid v4 UUIDs" >> {
      val p1 = "/fqon/workspaces/INVALID-UUID"
      val p2 = s"/fqon/workspaces/${UUID.randomUUID.toString}/environments/INVALID-UUID"
      
      Resource.mapPathData(p1) must throwA[BadRequestException]
      Resource.mapPathData(p2) must throwA[BadRequestException]
    }
  }
  
  
  "mapListPathData" should {
  
    "succeed when the path points to a first-level resource type" in {
      val m = Resource.mapListPathData("/fqon/target")
      m.size === 2
      m.get(Resource.Fqon) must beSome("fqon")
      m.get(Resource.TargetType) must beSome("target")
    }
    
    "succeed when the path points to a second-level resource type" in {
      val pid = uuid.toString
      val m = Resource.mapListPathData(s"/fqon/parent/${pid}/target")
      m.size === 4
      m.get(Resource.Fqon) must beSome("fqon")
      m.get(Resource.ParentType) must beSome("parent")
      m.get(Resource.ParentId) must beSome(pid)
      m.get(Resource.TargetType) must beSome("target")
    }
    
    "fail if the path is empty" in {
      Resource.mapListPathData("") must throwA[Throwable]
    }
    
    "fail if the path contains an odd-number of components" in {
      Resource.mapListPathData("foo") must throwA[Throwable]
      Resource.mapListPathData("/foo") must throwA[Throwable]
      Resource.mapListPathData("foo/") must throwA[Throwable]
      Resource.mapListPathData("/foo/") must throwA[Throwable]
      Resource.mapListPathData("/foo/bar/baz") must throwA[Throwable]
    }
    
    "fail if the path contains more than 4 components" in {
      Resource.mapListPathData("/foo/bar/baz/qux/thud") must throwA[Throwable]
      Resource.mapListPathData("/foo/bar/baz/qux/quux/corge/grault/garply/waldo") must throwA[Throwable]
    }
  }

  "findFqon" should {

    "create an Org" in {
      val gfOrg = createInstance(ResourceIds.Org, "galacticfog", id = UUID.randomUUID(), owner = owner, org = dummyRootOrgId, properties = Some(Map("fqon" -> "galacticfog")), parent = Some(dummyRootOrgId))
      gfOrg must beSuccessfulTry
    }

    "find an Org when given a valid FQON" in {
      val org = Resource.findFqon("galacticfog")
      org must beSome[GestaltResourceInstance]
      org.get.properties.get("fqon") === "galacticfog"
    }

    "return None when given an invalid FQON" in {
      Resource.findFqon("INVALID_FQON") must beNone
    }
  }

//  "isSubTypeOf" should {
//
//    "return TRUE if targetType is a sub-type of baseType" in {
//      Resource.isSubTypeOf(ResourceIds.Provider, ResourceIds.DcosProvider) must beTrue
//      Resource.isSubTypeOf(ResourceIds.Provider, ResourceIds.GatewayManager) must beTrue
//
//      Resource.isSubTypeOf(ResourceIds.Rule, ResourceIds.RuleEvent) must beTrue
//      Resource.isSubTypeOf(ResourceIds.Rule, ResourceIds.RuleLimit) must beTrue
//    }
//
//    "return FALSE if the targetType is NOT a sub-type of baseType" in {
//
//      Resource.isSubTypeOf(ResourceIds.DcosProvider, ResourceIds.Provider) must beFalse
//      Resource.isSubTypeOf(ResourceIds.GatewayManager, ResourceIds.Provider) must beFalse
//      Resource.isSubTypeOf(ResourceIds.RuleLimit, ResourceIds.Rule) must beFalse
//
//      // random false
//      Resource.isSubTypeOf(ResourceIds.Provider, ResourceIds.Workspace) must beFalse
//      Resource.isSubTypeOf(ResourceIds.Rule, ResourceIds.Policy) must beFalse
//      Resource.isSubTypeOf(ResourceIds.Org, ResourceIds.Environment) must beFalse
//      Resource.isSubTypeOf(ResourceIds.Provider, ResourceIds.Workspace) must beFalse
//    }
//  }

  "fromPath" should {

    "find a valid Org by FQON" in {
      val org = Resource.fromPath("/galacticfog")
      org must beSome
      org.get.name === "galacticfog"
    }

    "return None when the Org does not exist" in {
      Resource.fromPath("invalid_org") must beNone
    }

    "find a valid top-level resource (i.e. /{fqon}/environments/{id})" in {
      val org = Resource.fromPath("galacticfog")
      org must beSome

      val (wid, eid) = createWorkspaceEnvironment(org.get.id)

      val wrk = findById(ResourceIds.Workspace, wid)
      wrk must beSome

      findById(ResourceIds.Environment, eid) must beSome

      val fqon = org.get.properties.get("fqon")
      val path1 = "/%s/workspaces/%s".format(fqon, wid)
      val path2 = "/%s/environments/%s".format(fqon, eid)

      // Find the Workspace by path
      val test1 = Resource.fromPath(path1)
      test1 must beSome
      test1.get.id === wid

      // Find the Environment by path
      val test2 = Resource.fromPath(path2)
      test2 must beSome
      test2.get.id === eid
    }

    "find a valid second-level resource (i.e. /{fqon}/workspaces/{id}/environments/{id})" in {
      val org = Resource.fromPath("galacticfog")
      org must beSome

      val (wid, eid) = createWorkspaceEnvironment(org.get.id)

      val wrk = findById(ResourceIds.Workspace, wid)
      wrk must beSome

      findById(ResourceIds.Environment, eid) must beSome

      val fqon = org.get.properties.get("fqon")

      // Find the environment
      val path1 = "/%s/workspaces/%s/environments/%s".format(fqon, wid, eid)
      val test1 = Resource.fromPath(path1)

      test1 must beSome
      test1.get.id === eid
    }

    "throw an exception when an invalid resouce type name is given" in {
      Resource.fromPath(s"/galacticfog/invalid_resource/${uuid().toString}") must throwA[BadRequestException]
      Resource.fromPath(s"/galacticfog/workspaces/${uuid.toString}/invalid_resource/${uuid.toString}") must throwA[BadRequestException]
    }

    "throw an exception when the path contains an invalid UUID" in {
      Resource.fromPath(s"/galacticfog/invalid_resource/${uuid.toString}") must throwAn[BadRequestException]
      Resource.fromPath(s"/galacticfog/workspaces/${uuid.toString}/environments/INVALID_UUID") must throwAn[BadRequestException]
    }

    "CONDITION: typeName == 'providers" should {

      "find the resource if it is a valid subtype of Provider" in {
        val org = Resource.fromPath("galacticfog")
        org must beSome

        val (wid, eid) = createWorkspaceEnvironment(org.get.id)
        val g1 = createDummyGateway(eid, org = org.get.id)

        findById(ResourceIds.GatewayManager, g1.get.id) must beSome

        Resource.lookupResource("providers", g1.get.id) must beSome
      }

      "return None if the resource is NOT a valid subtype of Provider" in {
        val org = Resource.fromPath("galacticfog")
        org must beSome

        val (wid, eid) = createWorkspaceEnvironment(org.get.id)

        // UUID does not exist.
        Resource.lookupResource("providers", uuid()) must beNone

        // UUID names valid resource of wrong type.
        Resource.lookupResource("providers", eid) must beNone
      }
    }

    "CONDITION: typeName == 'rules" should {

      "find the resource if it is a valid subtype of Rule" in {
        val org = Resource.fromPath("galacticfog")
        org must beSome

        val (wid, eid) = createWorkspaceEnvironment(org.get.id)
        val (pol, rul) = createPolicyRule(eid, ruleType = ResourceIds.RuleLimit, org = org.get.id)

        findById(ResourceIds.Policy, pol) must beSome
        findById(ResourceIds.RuleLimit, rul) must beSome

        Resource.lookupResource("rules", rul) must beSome
      }

      "return None if the resource is NOT a valid subtype of Rule]" in {
        val org = Resource.fromPath("galacticfog")
        org must beSome

        val (wid, eid) = createWorkspaceEnvironment(org.get.id)

        Resource.lookupResource("rules", uuid()) must beNone
        Resource.lookupResource("rules", wid) must beNone
      }
    }
  }

  "findFirstLevelList" should {

    "find first-level resources of the given type" in {
      val org = Resource.fromPath("galacticfog")
      org must beSome

      val (wid, eid) = createWorkspaceEnvironment(org.get.id)
      val w = ResourceFactory.findById(ResourceIds.Workspace, wid)
      w must beSome

      val l1 = Resource.findFirstLevelList(Map(
        Resource.Fqon -> "galacticfog",
        Resource.TargetType -> "workspaces"))

      //l1.size === 1
      l1(0).typeId === ResourceIds.Workspace

      val e = ResourceFactory.findById(ResourceIds.Environment, eid)
      e must beSome

      val l2 = Resource.findFirstLevelList(Map(
        Resource.Fqon -> "galacticfog",
        Resource.TargetType -> "environments"))
      //l2.size === 1
      l2(0).typeId === ResourceIds.Environment
    }

    "find nothing when there are no resources of the given type" in {
      failure
    }.pendingUntilFixed("Need to work against clean database")

    "throw an exception when the Org is invalid" in {
      Resource.findFirstLevelList(Map(
        Resource.Fqon -> ("INVALID-" + uuid.toString),
        Resource.TargetType -> "environments")) must throwA[Throwable]
    }

    "throw an exception when the type-name is invalid" in {
      Resource.findFirstLevelList(Map(
        Resource.Fqon -> "galacticfog",
        Resource.TargetType -> ("INVALID-" + uuid.toString))) must throwA[Throwable]
    }
  }

  "findSecondLevelList" should {

    "find second-level resources of the given type" in {
      val org = Resource.fromPath("galacticfog")
      org must beSome

      val (wid, eid) = createWorkspaceEnvironment(org.get.id)

      val l1 = Resource.findSecondLevelList(Map(
        Resource.Fqon -> "galacticfog",
        Resource.ParentType -> "workspaces",
        Resource.ParentId -> wid.toString,
        Resource.TargetType -> "environments"))

      println("L1 : " + l1)
      println("L1.SIZE : " + l1.size)

      (l1.size > 0) === true
      l1(0).typeId === ResourceIds.Environment
    }

    "find nothing when there are no resources of the given type" in {
      failure
    }.pendingUntilFixed("Need to work against a clean database")

    "throw an exception when the given Org is invalid" in {
      val org = Resource.fromPath("galacticfog")
      org must beSome
      val (wid, eid) = createWorkspaceEnvironment(org.get.id)

      Resource.findSecondLevelList(Map(
        Resource.Fqon -> s"INVALID-${uuid.toString}",
        Resource.ParentType -> "workspaces",
        Resource.ParentId -> wid.toString,
        Resource.TargetType -> "environments")) must throwA[Throwable]
    }

    "throw an exception when the given parent-type (first-level) is invalid" in {
      val org = Resource.fromPath("galacticfog")
      org must beSome
      val (wid, eid) = createWorkspaceEnvironment(org.get.id)

      Resource.findSecondLevelList(Map(
        Resource.Fqon -> "galacticfog",
        Resource.ParentType -> s"INVALID-${uuid.toString}",
        Resource.ParentId -> wid.toString,
        Resource.TargetType -> "environments")) must throwA[Throwable]
    }

    "throw an exception when the given target-type (second-level) is invalid" in {
      val org = Resource.fromPath("galacticfog")
      org must beSome
      val (wid, eid) = createWorkspaceEnvironment(org.get.id)

      Resource.findSecondLevelList(Map(
        Resource.Fqon -> "galacticfog",
        Resource.ParentType -> "workspaces",
        Resource.ParentId -> wid.toString,
        Resource.TargetType -> s"INVALID-${uuid.toString}")) must throwA[Throwable]
    }
  }

  abstract class TestApplication extends WithApplication(containerApp()) {
    val testController = app.injector.instanceOf[ResourceController]  
  }

  def fakeRequest(secure: Boolean) = FakeRequest("GET", "/", headers = FakeHeaders(), body = AnyContentAsEmpty, secure = secure)

  "meta resource HREFs" should {

    "abide header.secure in the absence of proxy headers" in new TestApplication {
      testController.META_URL(
        fakeRequest(secure = false).withHeaders(HOST -> "meta.company.co")) must_==("http://meta.company.co")
      testController.META_URL(
        fakeRequest(secure = true).withHeaders(HOST -> "meta.company.co")) must_==("https://meta.company.co")
    }

    "abide X-FORWARDED-PROTO regardless of header.secure" in new TestApplication {
      testController.META_URL(
        fakeRequest(secure = false).withHeaders(HOST -> "meta.company.co", X_FORWARDED_PROTO -> "https")) must_==("https://meta.company.co")
      testController.META_URL(
        fakeRequest(secure = true).withHeaders(HOST -> "meta.company.co", X_FORWARDED_PROTO -> "https")) must_==("https://meta.company.co")
      testController.META_URL(
        fakeRequest(secure = false).withHeaders(HOST -> "meta.company.co", X_FORWARDED_PROTO -> "http")) must_==("http://meta.company.co")
      testController.META_URL(
        fakeRequest(secure = true).withHeaders(HOST -> "meta.company.co", X_FORWARDED_PROTO -> "http")) must_==("http://meta.company.co")
    }

    "lower case X-FORWARDED-PROTO" in new TestApplication {
      testController.META_URL(
        fakeRequest(secure = false).withHeaders(HOST -> "meta.company.co", X_FORWARDED_PROTO -> "HTTPS")) must_==("https://meta.company.co")
      testController.META_URL(
        fakeRequest(secure = false).withHeaders(HOST -> "meta.company.co", X_FORWARDED_PROTO -> "HTTP")) must_==("http://meta.company.co")
    }

    "prefer X-FORWARDED-HOST over request.host" in new TestApplication {
      testController.META_URL(
        fakeRequest(secure = false).withHeaders(HOST -> "some-node.company.co", X_FORWARDED_HOST -> "meta.company.co")) must_==("http://meta.company.co")
    }

  }

}