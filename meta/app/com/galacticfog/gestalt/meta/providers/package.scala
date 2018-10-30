package com.galacticfog.gestalt.meta

import play.api.libs.json._



package object providers {

  
  implicit lazy val uiLocationFormat = Json.format[UiLocation]
  implicit lazy val actionInputFormat = Json.format[ActionInput]
  implicit lazy val actionImplSpecFormat = Json.format[ActionImplSpec]
  implicit lazy val providerActionFormat = Json.format[ProviderActionSpec]
  

}