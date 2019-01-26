package migrations

import com.galacticfog.gestalt.data._
// import com.galacticfog.gestalt.data.models.GestaltTypeProperty
import com.galacticfog.gestalt.meta.test.MetaRepositoryOps
import play.api.test.PlaySpecification

class V28Spec extends PlaySpecification with MetaRepositoryOps {
  "V28" >> {

    "Delete Gestalt::Configuration::Provider::AWSLambda" >> {
      val tpe = TypeFactory.findByName("Gestalt::Configuration::Provider::AWSLambda")
      tpe must beNone
    }

    "Do not delete Gestalt::Configuration::Provider::Lambda::AWS" >> {
      val tpe = TypeFactory.findByName("Gestalt::Configuration::Provider::Lambda::AWS")
      tpe must beSome
    }

    "Delete Gestalt::Configuration::Provider::AWSAPIGateway" >> {
      val tpe = TypeFactory.findByName("Gestalt::Configuration::Provider::AWSAPIGateway")
      tpe must beNone
    }

    "Do not delete Gestalt::Configuration::Provider::GatewayManager::AWS" >> {
      val tpe = TypeFactory.findByName("Gestalt::Configuration::Provider::GatewayManager::AWS")
      tpe must beSome
    }
  }
}