package com.galacticfog.gestalt.meta.providers.ui


import org.specs2.mutable.Specification


class AssemblerSpec extends Specification {
val control1 = s"""
  |<!DOCTYPE html>
  |<html>
  |  <head>
  |  	<meta charset="utf-8">
  |    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
  |    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/css/materialize.min.css">
  |    <style>
  |    {{ ${TemplateKeys.GESTALT_USER_CSS} }}
  |    </style>
  |    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  |  </head>
  |  <body onload="render()">
  |    <div id="target">Loading...</div>
  |    
  |    <script id="template" type="x-tmpl-mustache">
  |    {{ ${TemplateKeys.GESTALT_USER_CONTENT} }}
  |    </script>
  |    
  |    <!-- Used by gestalt-ui -->
  |    <div id="container-root"></div>
  |    
  |    <!--Import jQuery before materialize.js-->
  |    <script type="text/javascript" src="https://code.jquery.com/jquery-3.2.1.min.js"></script>
  |    <script src="https://cdnjs.cloudflare.com/ajax/libs/mustache.js/2.3.0/mustache.min.js"></script>
  |    <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/js/materialize.min.js"></script>
  |    
  |    <script type="text/javascript">
  |      function render() {
  |      	var template = $$('#template').html();
  |        var rendered = Mustache.render(template, {  
  |        _ctx: {{ ${TemplateKeys.GESTALT_ACTION_CONTEXT} }}
  |       });
  |       $$('#target').html(rendered);
  |      }
  |      
  |      function closeParent() {
  |      	window.parent.document.getElementById('close-parent-modal').click();
  |      }
  |    </script>    
  |  </body>
  |</html>""".trim.stripMargin  
  
val template1 = s"""
  |<!DOCTYPE html>
  |<html>
  |  <head>
  |  	<meta charset="utf-8">
  |    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
  |    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/css/materialize.min.css">
  |    <style>
  |    {{ ${TemplateKeys.GESTALT_USER_CSS} }}
  |    </style>
  |    <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
  |  </head>
  |  <body onload="render()">
  |    <div id="target">Loading...</div>
  |    
  |    <script id="template" type="x-tmpl-mustache">
  |    {{ ${TemplateKeys.GESTALT_USER_CONTENT} }}
  |    </script>
  |    
  |    <!-- Used by gestalt-ui -->
  |    <div id="container-root"></div>
  |    
  |    <!--Import jQuery before materialize.js-->
  |    <script type="text/javascript" src="https://code.jquery.com/jquery-3.2.1.min.js"></script>
  |    <script src="https://cdnjs.cloudflare.com/ajax/libs/mustache.js/2.3.0/mustache.min.js"></script>
  |    <script src="https://cdnjs.cloudflare.com/ajax/libs/materialize/0.100.1/js/materialize.min.js"></script>
  |    
  |    <script type="text/javascript">
  |      function render() {
  |      	var template = $$('#template').html();
  |        var rendered = Mustache.render(template, {  
  |        _ctx: {{ ${TemplateKeys.GESTALT_ACTION_CONTEXT} }}
  |       });
  |       $$('#target').html(rendered);
  |      }
  |      
  |      function closeParent() {
  |      	window.parent.document.getElementById('close-parent-modal').click();
  |      }
  |    </script>    
  |  </body>
  |</html>""".trim.stripMargin
  
  
}