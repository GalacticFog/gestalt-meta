package com.galacticfog.gestalt.meta.auth

import java.util.UUID
import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.json.Js
import play.api.libs.json._


case class IdentityChange(old: GestaltResourceInstance, update: GestaltResourceInstance) {
  lazy val oldIdentities = IdentityChange.getIdentities(old)
  lazy val newIdentities = IdentityChange.getIdentities(update)
  
  lazy val deleted = oldIdentities.diff(newIdentities)
  lazy val added = newIdentities.diff(oldIdentities)    
}
object IdentityChange {
  def getIdentities(ent: GestaltResourceInstance): Seq[UUID] = {
    ent.properties.get.get("identities") map { ids =>
      Js.parse[Seq[UUID]](Json.parse(ids)).get
    } getOrElse Seq.empty
  }
}