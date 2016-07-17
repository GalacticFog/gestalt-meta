package com.galacticfog.gestalt.meta.auth

object Actions {

  object Org {
    private val prefix = "org"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }

  object User {
    private val prefix = "user"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }
  
  object Group {
    private val prefix = "group"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }
  
  object Workspace {
    private val prefix = "workspace"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }

  object Environment {
    private val prefix = "environment"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }

  object Lambda {
    private val prefix = "lambda"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }

  object Container {
    private val prefix = "container"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }
  
  object License {
    private val prefix = "license"
    val Create = s"$prefix.create"
    val View   = s"$prefix.view"
    val Update = s"$prefix.update"
    val Delete = s"$prefix.delete"
  }  
}