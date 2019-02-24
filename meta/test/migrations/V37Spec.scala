package migrations

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.sdk.ResourceIds
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V37Spec extends PlaySpecification with MetaRepositoryOps {
  
  "V37" should {

    "add environment_type 'system' to the enumeration" >> {
      val oldtypes = ReferenceFactory.findAllGlobal(ResourceIds.EnvironmentType)
      oldtypes.exists(_.name == "system") === false
      
      val oldCount = oldtypes.size
      oldCount must be_>(0)
      
      val v37 = new migrations.V37()
      val user1 = createNewUser()      
      v37.migrate(user1.id, None) must beRight
      
      val newtypes = ReferenceFactory.findAllGlobal(ResourceIds.EnvironmentType)
      val newCount = newtypes.size
      
      newCount === (oldCount + 1)
      newtypes.exists(_.name == "system") === true
    }
    
    "run multiple times without failure" >> {
      val v37 = new migrations.V37()
      val user1 = createNewUser()      
      
      v37.migrate(user1.id, None) must beRight
      v37.migrate(user1.id, None) must beRight
      v37.migrate(user1.id, None) must beRight
    }
    
  }
  
}