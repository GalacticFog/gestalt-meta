package com.galacticfog.gestalt.meta.providers


import scala.collection.JavaConverters._ 
   import play.api.libs.json._

import org.jtwig.JtwigModel;
import org.jtwig.JtwigTemplate;

  
package object ui {
  
  private[this] lazy val log = play.api.Logger(this.getClass)
  
  object Ascii {
    
    import java.util.Base64
    import java.nio.charset.Charset
    import scala.util.matching.Regex
    
    val DEFAULT_CHARSET: Charset = Charset.forName("UTF-8")
    
    private val base64Match = 
        """^\s*(([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{4}|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{2}==))\s*$""".r
        
    def encode64(s: String, charset: Charset = DEFAULT_CHARSET): String =
      Base64.getEncoder.encodeToString(s.getBytes(charset))
  
    def decode64(s: String): String = if (!isBase64(s))
      throw new IllegalArgumentException("Argument is not Base64 encoded.")
    else new String(Base64.getDecoder.decode(s))
    
    /**
     * Determine if a String is Base64 encoded.
     * 
     * TODO: The test in the function is very weak. It really checks if the given
     * String "could be" base64 encoded or not. Any string where str.length % 4 == 0
     * will pass (accounting for 1 or 2 equal signs padding the string).
     */
    def isBase64(s: String): Boolean = 
      base64Match.pattern.matcher(s).matches
  
    def asStream(s: String, charset: Charset = DEFAULT_CHARSET) = 
      new java.io.ByteArrayInputStream(s.getBytes(charset))
  }
  
  object SimpleProcessor {
    
    def render(template: String, vars: (String, String)*): String = {
      render(template, vars.toMap)
    }
    
    def render(template: String, vars: Map[String,String]): String = {
      val templ = mkTemplate(template)
      
      val jvars = (vars map { case (k, v) => (k -> v.asInstanceOf[Object]) }).asJava
      val model = JtwigModel.newModel(jvars)
      templ.render(model)
    }
    
    private def mkTemplate(s: String): JtwigTemplate = {
      JtwigTemplate.inlineTemplate(s)  
    }    
  }
  
  object TemplateKeys {
    val GESTALT_ACTION_CONTEXT = "GESTALT_ACTION_CONTEXT"
    val GESTALT_USER_CONTENT = "GESTALT_USER_CONTENT"
  }
  
  import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
  import com.galacticfog.gestalt.data._
  import com.galacticfog.gestalt.data.models._
  
  def buildActionContext(
      r: GestaltResourceInstance, 
      user: AuthAccountWithCreds, 
      action: GestaltResourceInstance): JsObject = {
    
    //get token replacer from caas-kube
    
    /*
     * TODO: Temporary - this needs to come from the TypeCache
     */
    val tpe = TypeFactory.findById(r.typeId).get
    val jsonUser = Json.obj(
      "id" -> user.account.id,
      "name" -> user.account.name,
      "email" -> user.account.email,
      "phone" -> user.account.phoneNumber)
    
    val jsonRes = Json.obj(
      "id" -> r.id,
      "typeName" -> tpe.name,
      "name" -> r.name)
    
    Json.obj("user" -> jsonUser, "resource" -> jsonRes)
  }
  

  def buildUiTemplate(content: String, context: String): String = {
     SimpleProcessor.render(
         Templates.UI_MATERIALIZE_1, 
         TemplateKeys.GESTALT_USER_CONTENT -> content,
         TemplateKeys.GESTALT_ACTION_CONTEXT -> context)
  }  
  
  
  
  def assembleActionUi(
        action: GestaltResourceInstance, 
        target: GestaltResourceInstance, 
        user: AuthAccountWithCreds): String = {
    log.debug("assembleActionUi(_,_,_)")
    log.debug("Loading Action Spec...")
    val spec = ProviderActionSpec.fromResource(action)
    
    log.debug("Decoding user-content...")
    val userContent = Ascii.decode64(spec.implementation.input.get.data.get)
    
    log.debug("Building Action Context...")
    val actionContext = buildActionContext(target, user, action)
    
    log.debug("Constructing UI Template...")
    buildUiTemplate(userContent, Json.prettyPrint(actionContext))    
  }
  
  
  object Templates {
  
  val UI_MATERIALIZE_1 = s"""
  |<!DOCTYPE html>
  |<html>
    <head>
    	<meta charset="utf-8">
      <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
      <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/css/materialize.min.css">
      <style>
        .card {
  	      background: #fff;
  	      border-radius: 2px;
  	      position: relative;
  	      padding-bottom: 0px;
  	      margin-bottom: 0px;
  	    }
        .card-1 {
  	      box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
  	      transition: all 0.3s cubic-bezier(.25,.8,.25,1);
        }
        .card-1:hover {
        	box-shadow: 0 14px 28px rgba(0,0,0,0.25), 0 10px 10px rgba(0,0,0,0.22);
        }	
        .info {
        	padding: 5px 5px 0px 5px;
        }
        .info tr {
  	      height: 10px;
  	      line-height: 10px;		
        }
        td.header {
  	      color: #0d47a1;
  	      font-weight: bold;
  	      padding-left: 10px;
        }
        nav {
  	      height: 45px;
  	      line-height: 50px;
        }
        nav .brand-logo { 
        	font-size: 1.6rem; 
        }
        .check-blue-dark-2[type="checkbox"].filled-in:checked + label:after {
  	      border: 2px solid #1976d2; 
  	      background-color: #1976d2;
        }
        #container {
          padding-bottom: 35px;
        }
        .actions-footer {
          position: fixed;
          bottom: 0;
          right: 0;
          height: 35px;
        }      
      </style>
      <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
    </head>
    <body onload="render()">
      <div id="target">Loading...</div>
      
      <script id="template" type="x-tmpl-mustache">
      {{ ${TemplateKeys.GESTALT_USER_CONTENT} }}
      </script>
      
      <!-- Used by gestalt-ui -->
      <div id="container-root"></div>
      
      <!--Import jQuery before materialize.js-->
      <script type="text/javascript" src="https://code.jquery.com/jquery-3.2.1.min.js"></script>
      <script src="https://cdnjs.cloudflare.com/ajax/libs/mustache.js/2.3.0/mustache.min.js"></script>
      <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/js/materialize.min.js"></script>
      
      <script type="text/javascript">
        function render() {
        	var template = $$('#template').html();
          var rendered = Mustache.render(template, {  
          _ctx: {{ ${TemplateKeys.GESTALT_ACTION_CONTEXT} }}
         });
         $$('#target').html(rendered);
        }
        
        function closeParent() {
        	window.parent.document.getElementById('close-parent-modal').click();
        }
      </script>    
    </body>
  |</html>""".stripMargin
  
  }
}