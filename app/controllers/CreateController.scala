package controllers

import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.{AuthAccountWithCreds, GestaltSecurityEnvironment}
import com.google.inject.Inject
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.util.SecureController
import play.api.i18n.MessagesApi

// TODO: remove this class?
class CreateController @Inject()( messagesApi: MessagesApi,
                                  env: GestaltSecurityEnvironment[AuthAccountWithCreds,DummyAuthenticator])
  extends SecureController(messagesApi = messagesApi, env = env) with Authorization {

  /*
   * 
   * 1 - Get JSON from request payload
   * 2 - Transform it if necessary
   * 3 - Create the resource
   * 4 - Render resource as JSON response.
   * 
   */
  
  def create(fqon: String, typeId: String) = Authenticate(fqon).async(parse.json) { implicit request =>
    
    val json = request.body
    
    
    
    ???
  }
  
  
}


