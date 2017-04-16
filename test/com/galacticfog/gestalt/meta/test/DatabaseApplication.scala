package com.galacticfog.gestalt.meta.test

import scala.reflect.ClassTag
import org.specs2.specification.BeforeAll
import play.api.Application
import play.api.mvc.Controller
import play.api.test.WithApplication
import controllers.util._

abstract class ControllerApplication[A <: Controller : ClassTag](application: Application) extends WithApplication(application) {
  val controller: A = app.injector.instanceOf[A]
}

abstract class WithDatabase[A <: Controller : ClassTag](application: Application) extends ControllerApplication[A](application) {
  def setup() = scalikejdbc.config.DBs.setupAll()
}

trait MetaRepositoryOps extends GestaltProviderMocking with ResourceScope with BeforeAll {
  override def beforeAll(): Unit = pristineDatabase()
}