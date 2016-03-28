package controllers


import java.util.UUID
import java.net.URL
import play.api.http.HttpVerbs
import play.api.libs.ws.WS
import play.api.Play.current

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.data.Hstore
import com.galacticfog.gestalt.data.PropertyValidator
import com.galacticfog.gestalt.data.ResourceFactory
import com.galacticfog.gestalt.data.ResourceType
import com.galacticfog.gestalt.data.illegal
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk.ResourceOwnerLink
import com.galacticfog.gestalt.data.uuid2string
import com.galacticfog.gestalt.meta.api.{ PatchOp, PatchDocument, PatchHandler }
import com.galacticfog.gestalt.meta.api.output._
import com.galacticfog.gestalt.meta.api.errors._
import com.galacticfog.gestalt.security.api.GestaltAccount
import com.galacticfog.gestalt.security.api.GestaltOrg
import com.galacticfog.gestalt.security.api.{ GestaltResource => SecurityResource }
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecuredController
import com.galacticfog.gestalt.tasks.play.io.NonLoggingTaskEvents
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util._
import controllers.util.JsonUtil._
import controllers.util.db._
import controllers.util.MetaController
import controllers.util.Security
import play.api.{ Logger => log }
import play.api.libs.json._
import com.galacticfog.gestalt.data.ResourceState
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.meta.api.errors._
import controllers.util.stringmap
import controllers.util.trace
import com.galacticfog.gestalt.meta.api._
import play.api.mvc.Result
import com.galacticfog.gestalt.laser._

object MarathonController extends GestaltFrameworkSecuredController[DummyAuthenticator]
  with MetaController with SecurityResources {

  
  def test(marathon: String) = Authenticate() { implicit request =>
    val buf = new StringBuilder
    
    buf append "request.path : " + request.path + "\n"
    buf append "request.uri  : " + request.uri + "\n"
    buf append "marathon     : " + marathon + "\n"
    buf append "querystring  : " + request.queryString + "\n"
    
    val meta_uri = request.path.replaceAll(marathon, "")
    buf append "meta_uri     : " + meta_uri + "\n"
    
    Ok(buf toString)
  }
  
  def proxyProvider(fqon: String, parentType: String, envId: UUID, providerId: UUID, proxyUri: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    // TODO: fill in the blanks!
    lazy val ___ = ???
    val marathonClient = MarathonClient(WS.client,___)
    (request.method,proxyUri) match {
      case (HttpVerbs.GET, "v2/apps") =>
        marathonClient.listApplicationsInEnvironment_marathon_v2(fqon = fqon, wrkName = ___, envName = ___)
          .map {Ok(_)}
          .recover {case e: Throwable => BadRequest(e.getMessage)}
      case (HttpVerbs.POST,"v2/apps") =>
        marathonClient.launchContainer_marathon_v2(fqon = fqon, wrkName = ___, envName = ___, marPayload = request.request.body.as[JsObject])
          .map {Created(_)}
          .recover {case e: Throwable => BadRequest(e.getMessage)}
      case _ => ___ // endpoints that we don't care about yet
    }
  }  
  
}



