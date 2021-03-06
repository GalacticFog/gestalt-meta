package controllers.util


import java.util.UUID

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.meta.api.patch._
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.patch._
import com.galacticfog.gestalt.security.api.{GestaltAccountUpdate, GestaltGroupUpdate, GestaltPasswordCredential, GestaltSecurityClient, ResourceLink => SecurityLink}
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import javax.inject.Inject
import play.api.Logger
import play.api.mvc.RequestHeader

import scala.concurrent.Future
import scala.util.{Either, Left, Right, Try}

class GroupMethods @Inject()( security: Security ) {
  
  private[this] val log = Logger(this.getClass)
  
  private[controllers] def groupPatch( 
     resource: GestaltResourceInstance,
     patch: PatchDocument,
     user: AuthAccountWithCreds,
     request: RequestHeader )
       (implicit client: GestaltSecurityClient): Future[GestaltResourceInstance] = Future.fromTry {
    
    /* Patch users if we have any ops for it.
     * patchGroupMembership returns a list of the groups current members (as links)
     * we don't use it currently.
     */
    val (userops, ops) = patch.ops partition { _.path.trim.endsWith("/users") }

    for {
      _       <- patchGroupMembership(resource.id, PatchDocument(userops:_*))
      updated <- {
                   log.info("Applying patch to Group in Meta.")
                   PatchInstance.applyPatch(resource, PatchDocument(ops:_*))
                 }
      secGroup = asSecurityGroup(updated.asInstanceOf[GestaltResourceInstance])
      _       <- {
                   log.info("Updating Group in gestalt-security")
                   val result = security.updateGroup(updated.id, user, secGroup)
                   log.info("gestalt-security result: " + result)
                   result
                 }
    } yield updated.asInstanceOf[GestaltResourceInstance]
    
}

  
  private[controllers] def userPatch(
    metaUser: GestaltResourceInstance,
    patch: PatchDocument,
    secAccount: AuthAccountWithCreds,
    request: RequestHeader)
      (implicit client: GestaltSecurityClient): Future[GestaltResourceInstance] = Future.fromTry {

    for {
      u <- {
        log.debug("Applying PATCH ops to resource...")
        PatchInstance.applyPatch(metaUser, patch)
      }      
      a <- {
        log.info("Updating user in gestalt-security...")
        val secUpdate = user2AccountUpdate(u.asInstanceOf[GestaltResourceInstance])
        security.updateAccount(metaUser.id, secAccount, secUpdate)
      }
      o <- Try(u.asInstanceOf[GestaltResourceInstance])
    } yield o
  }

  
  private def asSecurityGroup(group: GestaltResourceInstance) = {
    GestaltGroupUpdate(Some(group.name), group.description)
  }
  
  private def user2AccountUpdate(user: GestaltResourceInstance): GestaltAccountUpdate = {
    val props = user.properties.get.get _
    val maybePassword = props("password") map { GestaltPasswordCredential(_) }
    GestaltAccountUpdate(
      username    = Some(user.name),
      description = user.description,
      email       = props("email"),
      phoneNumber = props("phoneNumber"),
      credential  = maybePassword,
      firstName   = props("firstName"),
      lastName    = props("lastName")
    )
  }

  private[controllers] def patchGroupMembership(group: UUID, patch: PatchDocument)(
      implicit client: GestaltSecurityClient) :Try[Seq[SecurityLink]] = {
    
    val opmap = opsToMap(patch.ops, allowed = Seq(PatchOps.Add, PatchOps.Remove, PatchOps.Replace))
    val ids   = patch.ops map { i => UUID.fromString(i.value.get.as[String]) }
    
    // Make sure all the users given in the patch exist in Meta
    val users = getValidatedUsers(ids) match {
      case Right(users) => users
      case Left(errs)   => 
        throw new BadRequestException("The following user(s) not found: " + errs.mkString(","))
    }
    
    // Apply the Patch ops
    applyGroupUserOps(group, opmap map { o => 
      (o._1 -> (o._2 map ( p => UUID.fromString(p.value.get.as[String]) ) ) )
    })
  }
  
  /**
   * Apply Patch ops against group.properties.user - currently supports Add and Remove
   */
  private[controllers] def applyGroupUserOps(
      group: UUID, 
      opmap: Map[String,Seq[UUID]])(implicit client: GestaltSecurityClient): Try[Seq[SecurityLink]] = {
    
    def go(opNames: Seq[String], result: Seq[SecurityLink]): Try[Seq[SecurityLink]] = {
      opNames match {
        case Nil => Try(result)
        case op :: t => op match {
          case PatchOps.Add => 
            go(t, security.addAccountsToGroup(group, opmap(PatchOps.Add)).get)
          case PatchOps.Remove =>
            go(t, security.removeAccountsFromGroup(group, opmap(PatchOps.Remove)).get)
        }
      }
    }
    go(opmap.keys.toSeq, Seq.empty)
  }
  
  
  private[util] def getValidatedUsers(ids: Seq[UUID]): Either[Seq[UUID],Seq[GestaltResourceInstance]] = {
    // Make sure all the users exist in Meta
    val users = ResourceFactory.findAllIn(ResourceIds.User, ids)
    val found = users map { _.id }
    
    // Ensure we got all the users we asked for
    val delta = ids.diff(found)
    if (delta.isEmpty) Right(users) else Left(delta)
  }

  /**
   * Convert a list of patch ops to a map keyed by op name.
   */
  private[util] def opsToMap(given: Seq[PatchOp], allowed: Seq[String] = Seq.empty): Map[String,Seq[PatchOp]] = {
    val opmap = given groupBy { o => o.op }
    
    if (allowed.isEmpty) opmap
    else {
      val invalidOps = opmap.keys.toSeq diff allowed 
  
      if (invalidOps.isEmpty) opmap else
        throw new BadRequestException(
          s"Invalid Patch ops found. allowed: ${allowed.mkString(",")}")
    }
  }
}