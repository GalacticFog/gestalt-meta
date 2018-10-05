package com.galacticfog.gestalt.meta.providers.ui

import org.specs2.mutable.Specification

class SimpleProcessorSpec extends Specification {
  
  "render" should {
    
    "replace a single token in a string" in {
      
      val control1 = "One if by land, and two if by sea;"
      val template1 = "{{TOKEN1}} if by land, and two if by sea;"      
      val result = SimpleProcessor.render(template1, ("TOKEN1" -> "One"))

      result === control1
    }
    
    "replace multiple tokens from a sequence of tuples" in {
      val control1 = "One if by land, and two if by sea;"
      val template1 = "{{TOKEN1}} if by land, and {{TOKEN2}} if by sea;"
      val result = SimpleProcessor.render(template1, ("TOKEN1" -> "One"), ("TOKEN2" -> "two"))

      result === control1
    }
    
    "replace multiple tokens from a Map[String, String]" >> {
      val control1 = "One if by land, and two if by sea; And I on the opposite shore will be..."
      val template1 = "{{TOKEN1}} if by land, and {{TOKEN2}} if by sea; And I on the opposite {{TOKEN3}} will be..."
      val result = SimpleProcessor.render(template1, Map("TOKEN1" -> "One","TOKEN2" -> "two", "TOKEN3" -> "shore"))
      
      result === control1
    }
    
    "handle complex, structured content with multiple complex tokens" >> {
      val control1 = """
        |<html>
        |<head>
        |<style>
        |td.header {
  	    |  color: #0d47a1;
  	    |  font-weight: bold;
  	    |  padding-left: 10px;
        |}
        |</style>
        |</head>
        |<body>
        |<h1>Hello, World!</h1>
        |</body>
        |</html>
        """.trim.stripMargin
      
      val style = """
        |td.header {
  	    |  color: #0d47a1;
  	    |  font-weight: bold;
  	    |  padding-left: 10px;
        |}
        """.trim.stripMargin
      
      val html = "<h1>Hello, World!</h1>"
      
      val template1 = s"""
        |<html>
        |<head>
        |<style>
        |{{STYLE}}
        |</style>
        |</head>
        |<body>
        |{{HTML}}
        |</body>
        |</html>
        """.trim.stripMargin      
      
      val result = SimpleProcessor.render(template1, Map("STYLE" -> style, "HTML" -> html))
      
      println("results :\n" + result)
      
      result === control1
    }
  }
  
}