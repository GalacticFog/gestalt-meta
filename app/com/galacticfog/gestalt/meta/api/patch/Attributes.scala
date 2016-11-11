package com.galacticfog.gestalt.meta.api.patch

object Attributes {
  val Id = "/id"
  val Org = "/org"
  val Name = "/name"
  val Description = "/description"
  val ResourceType = "/resource_type"
  val ResourceState = "/state"
  val Owner = "/owner"
  val Created = "/created"
  val Modified = "/modified"
  val Tags = "/tags"
  val Variables = "/variables"
  
  protected[this] case class AttributeInfo(optional: Boolean, editable: Boolean)
  
  val attrs = Map(
      Id            -> AttributeInfo(false, false),
      Org           -> AttributeInfo(false, false),  // TEMPORARY: Cannot move to different Org.
      Name          -> AttributeInfo(false, true),
      Description   -> AttributeInfo(true, true),
      ResourceType  -> AttributeInfo(false, false),
      ResourceState -> AttributeInfo(false, true),
      Owner         -> AttributeInfo(false, false),  // TEMPORARY: Cannot transfer ownership.
      Created       -> AttributeInfo(false, false),
      Modified      -> AttributeInfo(false, false),
      Tags          -> AttributeInfo(true, true),
      Variables     -> AttributeInfo(true, true) )
   
  lazy val allowed = Attributes.attrs.keys.toSeq
}  