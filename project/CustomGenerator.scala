object CustomGenerator {
  object ModifiedInjectedRoutesGenerator extends play.routes.compiler.RoutesGenerator {
    import play.routes.compiler._
    import play.routes.compiler.RoutesCompiler.RoutesCompilerTask

    def generate(task: RoutesCompilerTask, namespace: Option[String], rules: List[Rule]): Seq[(String, String)] = {
      play.routes.compiler.InjectedRoutesGenerator.generate(task, namespace, rules) map { case(key, value) =>
        var v = value
        if(key.endsWith("/ReverseRoutes.scala")) {
          v = v.replace("import ReverseRouteContext.empty", "implicit val empty = ReverseRouteContext(Map())")
          v = v.replace("import play.core.routing.{ HandlerDef, ReverseRouteContext, queryString, dynamicString }", "import play.core.routing.{ ReverseRouteContext, queryString }")
          v = v.replace("import play.api.mvc.{ QueryStringBindable, PathBindable, Call, JavascriptLiteral }", "import play.api.mvc.{ QueryStringBindable, Call }")
        }
        if(key.endsWith("migrations/ReverseRoutes.scala")) {
          v = v.replace("import play.api.mvc.{ QueryStringBindable, Call }", "import play.api.mvc.{ Call }")
          v = v.replace("import play.core.routing.{ ReverseRouteContext, queryString }", "import play.core.routing.{ ReverseRouteContext }")
        }
        if(key.endsWith("/JavaScriptReverseRoutes.scala")) {
          v = v.replace("import ReverseRouteContext.empty", "")
          v = v.replace("import play.api.mvc.{ QueryStringBindable, PathBindable, Call, JavascriptLiteral }", "import play.api.mvc.{ QueryStringBindable, JavascriptLiteral }")
          v = v.replace("import play.core.routing.{ HandlerDef, ReverseRouteContext, queryString, dynamicString }", "")
        }
        if(key.endsWith("migrations/javascript/JavaScriptReverseRoutes.scala")) {
          v = v.replace("import play.api.mvc.{ QueryStringBindable, JavascriptLiteral }", "")
        }
        if(key.endsWith("/Routes.scala")) {
          v = v.replace("import play.core.routing.HandlerInvokerFactory._", "")
          v = v.replace("import play.core.j._", "")
          v = v.replace("import ReverseRouteContext.empty", "implicit val empty = ReverseRouteContext(Map())")
        }
        (key, v)
      }
    }

    def id: String = "injected+"
  }
}