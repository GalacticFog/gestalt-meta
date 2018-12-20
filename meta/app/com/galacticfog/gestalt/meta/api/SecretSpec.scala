package com.galacticfog.gestalt.meta.api

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk._
import play.api.Logger
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

import scala.util.{Failure, Try}

case class SecretSpec( name: String,
                       description: Option[String] = None,
                       provider: ContainerSpec.InputProvider,
                       items: Seq[SecretSpec.Item] = Seq.empty,
                       external_id: Option[String] = None
                     )

case object SecretSpec {

  val secretNameRegex = "[a-z0-9]([-a-z0-9]*[a-z0-9])?(\\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*".r
  val secretItemKeyRegex = "[-._a-zA-Z0-9]+".r

  val secretItemReads: Reads[SecretSpec.Item] = (
    (__ \ "key").read[String](Reads.pattern(secretItemKeyRegex) keepAnd Reads.minLength[String](1) keepAnd Reads.maxLength[String](253)) and
      (__ \ "value").readNullable[String]
    )(SecretSpec.Item.apply _)
  val secretItemWrites = Json.writes[SecretSpec.Item]

  implicit val secretItemFmt = Format(secretItemReads, secretItemWrites)

  val secretSpecReads: Reads[SecretSpec] = (
    (__ \ "name").read[String](Reads.pattern(secretNameRegex) keepAnd Reads.minLength[String](1) keepAnd Reads.maxLength[String](253)) and
      (__ \ "description").readNullable[String] and
      (__ \ "provider").read[ContainerSpec.InputProvider] and
      (__ \ "items").read[Seq[SecretSpec.Item]]
    )(SecretSpec.apply(_,_,_,_))
  val secretSpecWrites = Json.writes[SecretSpec]

  implicit val metaContainerSpec = Format(secretSpecReads, secretSpecWrites)


  case class Item(key: String, value: Option[String])

  val log = Logger(this.getClass)

  def toResourcePrototype(spec: SecretSpec): GestaltResourceInput = GestaltResourceInput(
    name = spec.name,
    resource_type = Some(ResourceIds.Secret),
    description = spec.description,
    resource_state = None,
    properties = Some(Map[String,JsValue](
      "provider" -> Json.toJson(spec.provider),
      "items" -> Json.toJson(spec.items.map(_.copy(value = None))) // values do not get persisted to meta
    ))
  )

  def fromResourceInstance(metaSecretSpec: GestaltResourceInstance): Try[SecretSpec] = {
    if (metaSecretSpec.typeId != ResourceIds.Secret) return Failure(new RuntimeException("cannot convert non-Secret resource into SecretSpec"))
    val attempt = for {
      props <- Try{metaSecretSpec.properties.get}
      provider <- Try{props("provider")} map {json => Json.parse(json).as[ContainerSpec.InputProvider]}
      items = props.get("items") map {json => Json.parse(json).as[Seq[SecretSpec.Item]]}
    } yield SecretSpec(
      name = metaSecretSpec.name,
      description = metaSecretSpec.description,
      provider = provider,
      items = items.getOrElse(Seq.empty)
    )
    attempt.recoverWith {
      case e: Throwable => Failure(new RuntimeException(s"Could not convert GestaltResourceInstance into SecretSpec: ${e.getMessage}"))
    }
  }

}