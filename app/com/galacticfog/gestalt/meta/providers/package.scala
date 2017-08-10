package com.galacticfog.gestalt.meta

import play.api.libs.json._
import com.galacticfog.gestalt.data._
import com.galacticfog.gestalt.data.models._
import com.galacticfog.gestalt.meta.api.sdk._

import java.util.UUID


package object providers {

  
  implicit lazy val uiLocationFormat = Json.format[UiLocation]
  implicit lazy val actionInputFormat = Json.format[ActionInput]
  implicit lazy val actionImplSpecFormat = Json.format[ActionImplSpec]
  implicit lazy val providerActionFormat = Json.format[ProviderActionSpec]
  

}