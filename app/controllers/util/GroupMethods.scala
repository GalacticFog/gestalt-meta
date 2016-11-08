package controllers.util


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.ResourcePath
import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.api.GestaltSecurityClient

import controllers.ResourceController.findResource
import controllers.util._

import com.galacticfog.gestalt.meta.api.patch._


import com.galacticfog.gestalt.patch._

import scala.util.{Try,Success,Failure}

import play.api.libs.json._

import java.util.UUID

import com.galacticfog.gestalt.data.EnvironmentType

import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._

import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

import com.galacticfog.gestalt.data.ResourceFactory.update

import com.galacticfog.gestalt.security.api.json.JsonImports.linkFormat
import scala.util.{Either,Right,Left}
import com.galacticfog.gestalt.security.api.{ResourceLink => SecurityLink}
import play.api.Logger


/**
 * @tparam A type of securityClient
 * @tparam B type of Account
 */
class GroupMethods[A,B](secProvider: SecurityProvider[A,B]) {
  
  private val log = Logger(this.getClass)
  
  private[controllers] def groupPatch(
        resource: GestaltResourceInstance, 
        patch: PatchDocument, 
        user: AuthAccountWithCreds)(implicit client: GestaltSecurityClient): Try[GestaltResourceInstance] = Try {

    /* Patch users if we have any ops for it.
     * patchGroupMembership returns a list of the groups current members (as links)
     * we don't use it currently.
     */
    val (userops, ops) = patch.ops partition { _.path.trim.endsWith("/users") }
    
    if (userops.nonEmpty) patchGroupMembership(
        resource.id, PatchDocument(userops:_*)).get
    
    // Handle patching other attributes
    ResourcePatch.applyPatch(resource, PatchDocument(ops:_*)).get.asInstanceOf[GestaltResourceInstance]
  }
  
  private[controllers] def patchGroupMembership(group: UUID, patch: PatchDocument)(implicit client: GestaltSecurityClient) :Try[Seq[SecurityLink]] = {
    
    val opmap = opsToMap(patch.ops, allowed = Seq(PatchOps.Add, PatchOps.Remove))
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
            go(t, Security.addAccountsToGroup(group, opmap(PatchOps.Add)).get)
          case PatchOps.Remove =>
            go(t, Security.removeAccountsFromGroup(group, opmap(PatchOps.Remove)).get)
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