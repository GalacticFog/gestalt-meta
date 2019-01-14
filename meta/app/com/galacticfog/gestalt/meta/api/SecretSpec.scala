package com.galacticfog.gestalt.meta.api

import com.galacticfog.gestalt.data.models.GestaltResourceInstance
import com.galacticfog.gestalt.meta.api.sdk._
import com.galacticfog.gestalt.util.ResourceSerde
import com.galacticfog.gestalt.util.EitherWithErrors._
import cats.syntax.either._
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

  def toResourcePrototype(spec: SecretSpec) = {
    val prunedSpec = spec.copy(items=spec.items map { item =>
      item.copy(value=None) // values do not get persisted to meta
      // shouldn't this happen somewhere outside serialization-deserialization code?
    })
    val properties = Json.toJson(prunedSpec).as[Map[String,JsValue]] -- Seq("name", "description")

    GestaltResourceInput(
      name = spec.name,
      resource_type = Some(ResourceIds.Secret),
      description = spec.description,
      resource_state = None,
      properties = Some(properties)
    )
  }

  def fromResourceInstance(metaSecretSpec: GestaltResourceInstance): Try[SecretSpec] = {
    if (metaSecretSpec.typeId != ResourceIds.Secret) return Failure(new RuntimeException("cannot convert non-Secret resource into SecretSpec"))
    val mssWithName = metaSecretSpec.copy(
      properties=metaSecretSpec.properties map { props =>
        props ++ Map("name" -> metaSecretSpec.name)
      }
    )
    ResourceSerde.deserialize[SecretSpec](mssWithName).liftTo[Try] map { secretSpec =>
      secretSpec.copy(
        name=metaSecretSpec.name,
        description=metaSecretSpec.description
      )
    } recoverWith { case e: Throwable =>
      Failure(new RuntimeException(s"Could not convert GestaltResourceInstance into SecretSpec: ${e.getMessage}"))
    }
  }

}