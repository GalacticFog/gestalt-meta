package controllers

import com.galacticfog.gestalt.meta.auth.Authorization
import com.galacticfog.gestalt.security.play.silhouette.GestaltFrameworkSecurity
import com.galacticfog.tracking.{Credit, CreditProvider}
import com.google.inject.Inject
import controllers.util.SecureController
import javax.inject.Singleton
import play.api.i18n.MessagesApi
import play.api.libs.json.Json

import scala.concurrent.Future

@Singleton
class TrackingController @Inject()(
    messagesApi: MessagesApi,
    sec: GestaltFrameworkSecurity,
    creditProvider: CreditProvider)
  extends SecureController(messagesApi = messagesApi, sec = sec) with Authorization {
  

  def postCredit() = AsyncAudited() { implicit request =>
    request.body.validate[Credit].fold({
      errs =>
        val details = errs.map({
          case (path, e) => path.toString -> e.mkString(",")
        }).toMap
        Future.successful(BadRequest(Json.obj(
          "message" -> "invalid payload",
          "details" -> details
        )))
    },{ credit =>
      val result = creditProvider.upsertCredit(credit)
      Future.successful{
        result match {
          case Some(true) => Ok
          case Some(false) => InternalServerError
          case _ => NotImplemented
        }
      }
    })
  }

  def getCredit(name: String) = AsyncAuditedAny() { implicit request =>
    val result = creditProvider.getCredit(name)
    Future.successful{
      result match {
        case Some(credit) => Ok(Json.toJson(credit))
        case _ => NotFound
      }
    }
  }

  def getCredits() = AsyncAuditedAny() { implicit request =>
    val result = creditProvider.getCredits()
    Future.successful{
      result match {
        case Some(credits) => Ok(Json.toJson(credits))
        case _ => NotImplemented
      }
    }
  }

  def deleteCredit(name: String) = AsyncAuditedAny() { implicit request =>
    val result = creditProvider.deleteCredit(name)
    Future.successful{
      result match {
        case Some(1l) => Ok
        case Some(0) => NotFound
        case _ => NotImplemented
      }
    }
  }
  
}
