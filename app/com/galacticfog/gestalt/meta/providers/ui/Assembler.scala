package com.galacticfog.gestalt.meta.providers.ui


import scala.collection.JavaConverters._
import play.api.libs.json._

import org.jtwig.JtwigModel;
import org.jtwig.JtwigTemplate;
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.providers.ProviderActionSpec
import scalatags.Text.TypedTag
import scalatags.Text.all._
import scala.xml._
import scala.xml.XML._
import scalatags.Text.tags2.style


object Assembler {
  
  private[this] lazy val log = play.api.Logger(this.getClass)  
  
  /*
   * These 'theme' color designations are used to control the color (or theme)
   * of system-generated UI elements.  Currently that is the header at the top
   * and the cancel/execute buttons at the bottom of the modal. 
   */
  private val SYSTEM_COLOR_DARK_BLUE = "blue darken-2"
  private val SYSTEM_COLOR_DEFAULT = SYSTEM_COLOR_DARK_BLUE
  private val SYSTEM_COLOR_ORANGE = "orange accent-3"
  
  /* 
   * This controls the display of the colored block with the action name
   * at the top of the action-ui modal.
   */
  private val SYSTEM_HEADER_DEFAULT = true
  /*
   * This is the .id of the form where we get the payload data-fields for
   * the POST to /invoke.  The .id of each 'input' element in the form is
   * used to collect the values.
   */
  private val SYSTEM_UI_FORM_ID = "gestalt-action-form"
  
  private val DOCTYPE_DECLARATION = "<!DOCTYPE html>"
  
  
  def prettyScalaTags(tt: TypedTag[String], maxWidth: Int = 150, indent: Int = 2, withDocType: Boolean = true): String = {
    prettyHtmlString(tt.toString, maxWidth, indent)
  }
  
  def prettyHtmlString(html: String, maxWidth: Int = 150, indent: Int = 2, withDocType: Boolean = true): String = {
    val printer = new PrettyPrinter(maxWidth, indent)
    val decl = if (withDocType) DOCTYPE_DECLARATION else "" 
    decl + "\n" + printer.format(XML.loadString(html))    
  }  
  
  def ActionHeader(metaUrl: String) = {
    tag("nav")(id:="gf-action-header", attr("style"):="display:none")(
      div(`class`:="nav-wrapper gf-blue darken-2")(
        a(href:="#", `class`:="brand-logo center")(
          img(`class`:="logo-img", src:=Assets.Image(metaUrl, LOGO_3)),
          span(id:="gf-header-title", attr("style"):="display:inline")
         )
      )
    )    
  }
 
  def buildContentArea(userHtml: String, metaUrl: String) = {
    ContentWrapper(
      ActionHeader(metaUrl),
      ResourceInfo(),
      InstructionCard(),
      InputForm(userHtml)
    )
  }

  /**
   * Generate the 'Execute' and 'Cancel' buttons at the bottom of the Action-UI
   */
  def StandardControls(): Frag = {
    val toolTipDelay = 200.toString
    
    div(`class`:="section", attr("style"):="text-align:right")(
      div(`class`:="row")(
          
        button(
          id:="gestalt-cancel",
          `class`:=s"waves-effect waves-light btn gf-blue tooltipped",
          `type`:="button", // <- this keeps button from running 'submit' function
          onclick:="closeParent()",
          attr("data-position"):="top",
          attr("data-delay"):=toolTipDelay,
          attr("data-tooltip"):="Close form with no changes")("Cancel"),

        button(
          id:="gestalt-execute",
          `class`:=s"waves-effect waves-light btn gf-blue tooltipped", 
          `type`:="submit",
          attr("data-position"):="top",
          attr("data-delay"):=toolTipDelay,
          attr("data-tooltip"):="Invoke this action")("Execute")
      )
    )
  }

  
  val ContentWrapper = {
    div(cls:="row", attr("style"):="padding: 0.75em")
  }
  
  val ResourceInfo = {

    ul(id:="resource-data", cls:="collapsible", 
        attr("dataCollapsible"):="expandable", attr("materialize"):="collapsible",
        attr("style"):="display:none")(
      li(
        div(cls:="collapsible-header active resource-info-header",
            attr("style"):="font-weight: bold; background-color: rgba(58, 173, 240, 0.3)")(
                i(cls:="material-icons")("navigate_next")
        ),
        div(cls:="collapsible-body", 
          attr("style"):="padding: 0px; margin-bottom: 0px;")(
            div(`class`:="row", id:="dynamic-table", attr("style"):="margin-bottom: 0px;")
        )
      )
    )
  }

  
  /*

  Set type = 'button' to all non-submit buttons.
  Check why 'Dismiss' buttons are weird (error and splash)
  Enable static content from Meta (images, javascript, css)
  Dry-run endpoint to get UI templates
  build context: add full resource, and action JSON.
  
  */

  val InstructionCard = {
    div(id:="instruction-card", `class`:="row instruction-card", attr("style"):="display:none")(
      div(`class`:="card-panel", attr("style"):="border: 2px solid rgba(58, 173, 240, 0.3)")(
        span(`class`:="blue-text text-darken-2", id:="action-description")
      )
    )
  }
  
  def SplashScreen(metaUrl: String) = {
    div(id:="gf-splash-screen",
      `class`:="modal modal-fixed-footer instructions-modal valign-wrapper")(
       div(`class`:="modal-content")(
         div(`class`:="splash-title")(
           img(src:=Assets.Image(metaUrl, LOGO_2), 
               attr("style"):="width: 25px; height: 25px; margin-right: 5px; transform: translateY(20%)"),
           h5(id:="gf-splash-title", `class`:="blue-text text-darken-2", attr("style"):="display:inline;")
         ),
         p(id:="gf-splash-body"),
         div(`class`:="modal-footer")(
           a(href:="#!", `class`:="modal-action modal-close waves-effect waves-green btn-flat")("Dismiss")
         )
       )
     )    
  }
  
  def ErrorScreen() = {
   div(id:="gestalt-error", 
     `class`:="modal bottom-sheet", attr("style"):="border: 4px solid #e53935")(
   div(`class`:="modal-content")(
     div(cls:="red-text text-darken-1")(
       i(cls:="small material-icons")("error"),
       span(id:="error-title")
     ),
     p(id:="error-body")
   ),
   div(`class`:="modal-footer red darken-1 white-text")(
     a(href:="#!", 
       cls:="modal-action modal-close waves-effect waves-green btn-flat white-text")("Dismiss")
     )
   )
  }

  def InputForm(userHtml: String) = {
    form(`class`:="col s12 input-form", id:=SYSTEM_UI_FORM_ID)(
      fieldset(cls:="card-1", 
        attr("style"):="border: 2px solid rgba(58, 173, 240, 0.3)")(
        legend(id:="form-title", 
          attr("style"):="font-weight: regular; color: #1c1f2f;"),
        userHtml
      ),
      StandardControls
    )
  }
  
  
  /**
   * Generate the main part of the input form from the user-supplied html.
   * IMPORTANT - the user-supplied code MUST NOT contain the 'form' element. It should
   * be just the form internals. We don't have a reliable to validate/enforce that right now.
   */
  def UserContent(content: String, metaUrl: String): Frag = {
    
    div(`class`:="row")(
      ResourceInfo,
      InstructionCard,
      
      form(`class`:="col s12", id:=SYSTEM_UI_FORM_ID)(
        content,
        StandardControls
      ),
      
      div(id:="gf-splash-screen",
        `class`:="modal modal-fixed-footer instructions-modal valign-wrapper")(
         div(`class`:="modal-content")(
           div(`class`:="splash-title")(
             img(src:=Assets.Image(metaUrl, LOGO_2), 
                 attr("style"):="width: 25px; height: 25px; margin-right: 5px; transform: translateY(20%)"),
             h5(id:="gf-splash-title", `class`:="blue-text text-darken-2", attr("style"):="display:inline;")
           ),
           p(id:="gf-splash-body"),
           div(`class`:="modal-footer")(
             a(href:="#!", `class`:="modal-action modal-close waves-effect waves-green btn-flat")("Dismiss")
           )
         )
       ),
       
       div(id:="gestalt-error", 
           `class`:="modal bottom-sheet", attr("style"):="border: 4px solid #e53935")(
         div(`class`:="modal-content")(
           div(cls:="red-text text-darken-1")(
             i(cls:="small material-icons")("error"),
             span(id:="error-title")
           ),
           p(id:="error-body")
         ),
         div(`class`:="modal-footer red darken-1 white-text")(
           a(href:="#!", 
             cls:="modal-action modal-close waves-effect waves-green btn-flat white-text")("Dismiss")
         )
       )
     )
  }

  private[ui] def safeAppendAll[A, B](m: Map[A, B], tups: Seq[(A, Option[B])], default: Option[B] = None): Map[A, B] = {
    tups.foldLeft(m) { (acc, next) =>
      acc ++ safeAppend(m, next)
    }
  }
  
   private[ui]def safeAppend[A, B](m: Map[A, B], tup: (A, Option[B]), default: Option[B] = None): Map[A, B] = {
    if (tup._2.isEmpty) {
      if (default.isEmpty) m else (m ++ Map(tup._1 -> default.get))
    } else (m ++ Map(tup._1 -> tup._2.get))
  }

  /*
   * This plugs in any user-supplied CSS before the system-generated styles.
   */   
  def ConsolidatedStyles() = {
    style(s"{{ ${TemplateKeys.GESTALT_USER_CSS} }}" /*,navStyles*/)
  }

  /*
   * TODO: This script should probably be stored in an external file that we just link in.
   */
   def SystemFunctions(user: AuthAccountWithCreds) = {
  
      script(`type` := "text/javascript")(
        raw(s"""
          const getToken = () => '$${user.creds.headerValue}';
          const _ctx =  {{ ${TemplateKeys.GESTALT_ACTION_CONTEXT} }};
          const _url = '{{ ${TemplateKeys.GESTALT_ACTION_URL} }}';
          const res = _ctx.resource;
        """))
    }
   
    def getUiTemplate(
        user: AuthAccountWithCreds,
        metaUrl: String) = {
  
      val dom = {
        html(
            head(
              meta(charset:="utf-8"),
              meta(name:="viewport", content:="width=device-width, initial-scale=1.0"),
              link(rel:="stylesheet", href:="https://fonts.googleapis.com/icon?family=Material+Icons"),
              link(rel:="stylesheet", href:="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/css/materialize.min.css"),
              link(rel:="stylesheet", href:=cssUrl(metaUrl, "meta-actions.css")),
              ConsolidatedStyles()
            ),
            body(
    
              buildContentArea(s"{{ ${TemplateKeys.GESTALT_USER_CONTENT} }}", metaUrl),
              SplashScreen(metaUrl),
              ErrorScreen(),
              div(id:="container-root"),
              script(`type`:="text/javascript", src:="https://code.jquery.com/jquery-3.2.1.min.js")(" "),
              script(`type`:="text/javascript", src:="https://cdnjs.cloudflare.com/ajax/libs/mustache.js/2.3.0/mustache.min.js")(" "),
              script(`type`:="text/javascript", src:="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/js/materialize.min.js")(" "),
              script(`type`:="text/javascript", src:=scriptUrl(metaUrl, "meta-actions-ui.js"))(""),
              SystemFunctions(user),
              script(`type`:="text/javascript")(s"\n{{ ${TemplateKeys.GESTALT_USER_SCRIPT} }}")
  
            )
         )
      }
      /*
       * Processing the output through the XML printer causes script and style blocks to be conerted to text-nodes,
       * which escapes the text making them invalid in the browser.  For now, we're relying on the default rendering
       * of the scalatags - it's not pretty-printed but works in initial testing. 
       */
      //prettyScalaTags(dom)
      dom.render
    }   
 
    /**
     * 
     */
    def envelope(
        fqon: String,
        meta: String,
        context: Option[JsValue] = None,
        user: AuthAccountWithCreds): String = {
      
      log.info("Building Action Context...")
      val actionContext = context getOrElse {
        Json.obj("action" -> Json.obj(), "user" -> Json.obj(), "resource" -> Json.obj())
      }
      
      log.info("Constructing UI Template...")

      val actionUrl = "%s/%s/actions/%s".format(meta, fqon, "example")
      log.info("Action-URL generated: " + actionUrl)

      processUiTemplate(user,
          content = "<!-- YOUR CONTENT HERE -->",
          actionContext.toString, 
          style = Some("<!-- YOUR CSS HERE -->"),
          script = Some("/* YOUR JAVASCRIPT HERE */"), 
          metaUrl = meta, 
          actionUrl = actionUrl)
    }
    
    def assemble(
        fqon: String,
        meta: String,
        action: ProviderActionSpec,
        target: Option[GestaltResourceInstance] = None,
        context: Option[JsValue] = None,
        user: AuthAccountWithCreds): String = {      
      
      log.info("Getting input spec...")
      val input = action.implementation.input.get
  
      log.info("Decoding html...")
      val userContent = Ascii.decode64(input.data.get)
  
      log.info("Decoding user style data...")
      val userStyle = Ascii.decode64Opt(input.style)
  
      log.info("Decoding user script data...")
      val userScript = Ascii.decode64Opt(input.script)
      
      log.info("Building Action Context...")
      val actionContext = context getOrElse {
        Json.obj("action" -> Json.obj(), "user" -> Json.obj(), "resource" -> Json.obj())
      }

      log.info("Constructing UI Template...")

      val actionUrl = "%s/%s/actions/%s/invoke".format(meta, fqon, "example")
      log.info("Action-URL generated: " + actionUrl)

      processUiTemplate(user,
          userContent, 
          actionContext.toString, 
          userStyle, 
          userScript, 
          metaUrl = meta, 
          actionUrl = actionUrl)
    }
    
    
    def assemble(
      fqon: String,
      meta: String,
      action: GestaltResourceInstance,
      target: GestaltResourceInstance,
      user: AuthAccountWithCreds): String = {
      
      val spec = ProviderActionSpec.fromResource(action)
  
      log.info("Getting input spec...")
      val input = spec.implementation.input.get
  
      log.info("Decoding html...")
      val userContent = Ascii.decode64(input.data.get)
  
      log.info("Decoding user style data...")
      val userStyle = Ascii.decode64Opt(input.style)
  
      log.info("Decoding user script data...")
      val userScript = Ascii.decode64Opt(input.script)
  
      log.info("Building Action Context...")
      val actionContext = buildActionContext(target, user, action, meta)

      log.info("Constructing UI Template...")
      val context = Json.prettyPrint(actionContext)
      
      val actionUrl = "%s/%s/actions/%s/invoke".format(meta, fqon, action.id.toString)
      log.info("Action-URL generated: " + actionUrl)
  
      processUiTemplate(user,
          userContent, 
          context, 
          userStyle, 
          userScript, 
          metaUrl = meta, 
          actionUrl = actionUrl)
    }
    
    /*
     * TODO: Wrap these up in an AssetPaths class or similar...
     */
    def styleDir(meta: String) = "%s/assets/css".format(meta)
    def scriptDir(meta: String) = "%s/assets/javascripts".format(meta)
    def cssUrl(meta: String, file: String) = "%s/%s".format(styleDir(meta), file)
    def scriptUrl(meta: String, file: String) = "%s/%s".format(scriptDir(meta), file)
    
    private lazy val LOGO_1 = "favicon.png"  // hexagonal 'G' logo
    private lazy val LOGO_2 = "gf-logo2.svg" // black wireframe sphere
    private lazy val LOGO_3 = "gf-logo4.svg" // white wireframe sphere
    
    object Assets {
      
      private val base = "assets"
      private val images = "images/icons"
      private val scripts = "javascripts"
      private val styles = "css"

      def Image(meta: String, name: String): String = {
        mkurl(meta, images, name)
      }
      
      def Script(meta: String, name: String): String = {
        mkurl(meta, scripts, name)
      }
      
      def Style(meta: String, name: String): String = {
        mkurl(meta, styles, name)
      }
      
      def Asset(meta: String, uri: String): String = {
        "%s/%s".format(meta, uri)
      }
      
      private def mkurl(meta: String, fileType: String, fileName: String): String = {
        "%s/%s/%s/%s".format(meta, base, fileType, fileName)
      }      
    }
    
    /*
     * TODO: Parameter list is growing too large here (and will probably grow larger). Change signature to
     * accept a map of parsed values.
     */
    def processUiTemplate(
        user: AuthAccountWithCreds,
        content: String, 
        context: String,
        style: Option[String] = None, 
        script: Option[String] = None,
        actionUrl: String,
        metaUrl: String,
        template: Option[String] = None): String = {
      
      val requiredArgs = Map(
        TemplateKeys.GESTALT_USER_CONTENT -> content,
        TemplateKeys.GESTALT_ACTION_CONTEXT -> context,
        TemplateKeys.GESTALT_ACTION_URL -> actionUrl)
      
      val optionalArgs = Seq(
        TemplateKeys.GESTALT_USER_CSS -> style,
        TemplateKeys.GESTALT_USER_SCRIPT -> script)
      
      val templateArguments = safeAppendAll(requiredArgs, optionalArgs, default = Some(""))
      
      if (log.isDebugEnabled) {
        val debugmap = templateArguments.map{ case (k,v) =>
          val dat = {
            if (v.size <= 10)  "\"%s\"".format(v) 
            else "\"%s...\"".format(v.substring(0, 10))
          }
          (k, dat)
        }
        log.debug("Parsed template arguments:")
        log.debug(debugmap.toString)
      }
      
      val uiTemplate = template getOrElse getUiTemplate(user, metaUrl)
      
      SimpleProcessor.render(uiTemplate, templateArguments)
    }  
  
}