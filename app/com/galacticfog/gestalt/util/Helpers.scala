package com.galacticfog.gestalt.util

import org.joda.time.DateTime
import play.api.libs.json._

object Helpers {

  object JodaJsonFormats {
    implicit val jodaDateWrites: Writes[DateTime] = new Writes[DateTime] {
      def writes(d: DateTime): JsValue = JsString(d.toString())
    }
    implicit val jodaDateReads = Reads[DateTime](js =>
      js.validate[String].map[DateTime](dtString =>
        DateTime.parse(dtString)
      )
    )
    implicit val formatJodaDate = Format(jodaDateReads, jodaDateWrites)
  }

  object OptionDefaults {
    implicit val defaultString =  (o : Option[String]) => o.getOrElse("")
  }

  implicit class OptionWithDefaults[T](o: Option[T]){

    def getOrDefault()(implicit defaultFunction: Option[T] => T) : T = {
      defaultFunction(o)
    }
  }
}
