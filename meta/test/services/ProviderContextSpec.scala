package services


//import com.galacticfog.gestalt.data._
//import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification


class ProviderContextSpec extends PlaySpecification with MetaRepositoryOps {

  "mapUri" should {
    
    "map a URI when parent is included" >> {
      val fqon = "org"
      val parentType = "environments"
      val parentId = uuid.toString
      val targetType = "containers"
      val targetId = uuid.toString
      
      val uri = s"$fqon/$parentType/$parentId/$targetType/$targetId"
      val pc = new ProviderContext(FakeURI(uri), uuid, None)
      
      val m = pc.mapUri(uri)
      m.size === 5
      m.get("fqon") must beSome(fqon)
      m.get("parentType") must beSome(parentType)
      m.get("parentId") must beSome(parentId)
      m.get("targetType") must beSome(targetType)
      m.get("targetId") must beSome(targetId)
    }
    
    
    "inject parent data when parent is omitted from URI" >> {
      
      val (wrk, env) = createWorkspaceEnvironment()
      val container = newDummyContainer(env)
      container must beSuccessfulTry
      
      val fqon = "org"
      val parentType = "environments"
      val parentId = env.toString
      val targetType = "containers"
      val targetId = container.get.id.toString
      
      val uri = s"$fqon/$targetType/$targetId"
      val pc = new ProviderContext(FakeURI(uri), uuid, None)
      
      val m = pc.mapUri(uri)

      m.size === 5
      m.get("fqon") must beSome(fqon)
      m.get("parentType") must beSome(parentType)
      m.get("parentId") must beSome(parentId)
      m.get("targetType") must beSome(targetType)
      m.get("targetId") must beSome(targetId)
    }    
    
  }
  
}
