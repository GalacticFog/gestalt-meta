package controllers

import com.galacticfog.gestalt.meta.auth.Authorization

// TODO: remove this class?
object CreateController extends Authorization {

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


