package com.galacticfog.gestalt.meta.test

import scala.reflect.ClassTag
import org.specs2.specification.{AfterEach, BeforeAll}
import play.api.Application
import play.api.mvc.Controller
import play.api.test.WithApplication
import controllers.util._
import org.specs2.execute.{AsResult, Result}
import play.api.db.Database
import play.api.inject.Injector


/**
 * Provides a Play application with a single controller constructed.
 */
abstract class ControllerApplication[A <: Controller : ClassTag](application: Application) extends WithApplication(application) {
  val controller: A = app.injector.instanceOf[A]
}


abstract class WithDb(val application: Application) extends WithApplication(application) {
  override def around[T: AsResult](t: => T): Result = super.around {
    scalikejdbc.config.DBs.setupAll()
    try AsResult(t)
    finally {
      scalikejdbc.config.DBs.closeAll()
      application.stop()
    }
  }  
}

abstract class WithDbController[A <: Controller : ClassTag](application: Application) extends ControllerApplication[A](application) {
  override def around[T: AsResult](t: => T): Result = super.around {
    scalikejdbc.config.DBs.setupAll()
    try AsResult(t)
    finally scalikejdbc.config.DBs.closeAll()
  }
}

trait DbShutdown extends AfterEach {
  //TODO: find a way to get rid of this ugly solution.
  //We need either to access example scope from a spec-level aroundEach or
  //implement example-level aroundEach - this approach leads to a weird reflection error
  private var injectorOpt : Option[Injector] = None

  def setInjector(injector : Injector) : Unit = injectorOpt = Some(injector)

  override def after(): Unit = {
    injectorOpt match {
      case Some(v) => {
        val database = v.instanceOf(classOf[Database])
        database.shutdown()
      }
      case None =>
    }
  }
}

/**
 * This pulls several traits together for easier test class declaration
 */
trait MetaRepositoryOps extends GestaltProviderMocking with ResourceScope with BeforeAll {
  override def beforeAll(): Unit = pristineDatabase().get
}


