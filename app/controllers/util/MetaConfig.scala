package controllers.util

import java.util.UUID
import play.api.libs.json._
import scala.util.{Try,Success,Failure}
import com.galacticfog.gestalt.data.models.GestaltResourceInstance


trait MetaConfig[A] {
  
  def initRootUser(user: Option[A]): Try[A]
  def setRootUser(user: A): Try[Unit]
  def getRootUser(): Try[A]
  def getRootUserId(): Option[UUID]
  def isRootUser(identity: UUID): Boolean
  
}


/**
 * This is a dummy object that holds the root identity locally
 * (and ephemerally). Useful for testing the concept and interface.
 */
object DummyMetaConfig extends MetaConfig[GestaltResourceInstance] {

  private var rootuser: GestaltResourceInstance = null
  
  
  def initRootUser(user: Option[GestaltResourceInstance]): Try[GestaltResourceInstance] = {
    Try {
      user.fold {
        if (rootuser == null) {
          throw new RuntimeException("Could not initialize root user.")
        } else rootuser
      }{ newRoot => 
        rootuser = newRoot
        newRoot
      }
    }
  }
  
  def setRootUser(user: GestaltResourceInstance): Try[Unit] = {
    rootuser = user
    Try(())
  }
  
  def getRootUser(): Try[GestaltResourceInstance] = {
    Try(rootuser)  
  }
  
  def getRootUserId(): Option[UUID] = {
    if (rootuser != null) Some(rootuser.id) else None
  }
  
  def isRootUser(identity: UUID): Boolean = {
    getRootUserId.fold(false)(identity == _)
  }

}

