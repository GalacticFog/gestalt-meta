package com.galacticfog.gestalt.meta.test

import scala.reflect.ClassTag
import org.specs2.specification.BeforeAll
import play.api.Application
import play.api.mvc.Controller
import play.api.test.WithApplication
import controllers.util._
import org.specs2.execute.{AsResult,Result}


/**
 * Provides a Play application with a single controller constructed.
 */
abstract class ControllerApplication[A <: Controller : ClassTag](application: Application) extends WithApplication(application) {
  val controller: A = app.injector.instanceOf[A]
}


abstract class WithDb(application: Application) extends WithApplication(application) {
  override def around[T: AsResult](t: => T): Result = super.around {
    scalikejdbc.config.DBs.closeAll()
    scalikejdbc.config.DBs.setupAll()
    t
  }  
}

abstract class WithDbController[A <: Controller : ClassTag](application: Application) extends ControllerApplication[A](application) {
  override def around[T: AsResult](t: => T): Result = super.around {
    scalikejdbc.config.DBs.closeAll()
    scalikejdbc.config.DBs.setupAll()
    t
  }
}

/**
 * This pulls several traits together for easier test class declaration
 */
trait MetaRepositoryOps extends GestaltProviderMocking with ResourceScope with BeforeAll {
  override def beforeAll(): Unit = pristineDatabase()
}


