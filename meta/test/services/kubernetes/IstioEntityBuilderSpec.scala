package services.kubernetes

import play.api.test.PlaySpecification
import org.specs2.specification.{BeforeAfterEach, BeforeAll}
import com.galacticfog.gestalt.meta.test.ResourceScope

class IstioEntityBuilderSpec extends PlaySpecification with ResourceScope with BeforeAll with BeforeAfterEach {
  override def beforeAll(): Unit = pristineDatabase()

  override def before: Unit = scalikejdbc.config.DBs.setupAll()

  override def after: Unit = scalikejdbc.config.DBs.closeAll()

  sequential
    
  lazy val ieb = new IstioEntityBuilder { }

  "IstioEntityBuilder" should {
    "1" in {
      val _ = ieb.mkGateway(null, null)
      1 must_== 1
    }
    "2" in {
      val _ = ieb.mkVirtualService(null, null)
      1 must_== 1
    }
  }
}