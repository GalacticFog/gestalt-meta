package controllers.util

import scala.language.postfixOps
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.meta.api.errors.BadRequestException

class QueryString(qs: Map[String,Seq[String]]) {
  
}

object QueryString {
  
  def single[V](qs: Map[String,Seq[V]], param: String): Option[V] = {
    qs.get(param).map { buf =>
      if (buf.size > 1) {
        throw new BadRequestException(s"Too many values found for '$param'. Expected: 1, Found: ${buf.size}")
      } else buf.head
    }
  }
  
  def list[V](qs: Map[String,Seq[V]], param: String): Seq[V] = {
    qs.getOrElse(param, Seq.empty[V])
  }
  
  def singleBoolean[V](qs: Map[String, Seq[V]], param: String): Boolean = {
    single(qs, param).fold(false) { p =>
      Try(p.toString.toBoolean) match {
        case Success(b) => b == true
        case Failure(_) => 
          throw new BadRequestException(s"Value of '$param' parameter must be true or false. found: $p")
      }
    }
  }
  
  
  def singleParamBoolean(qs: Map[String,Seq[String]], param: String) = {
    if (!qs.contains(param)) false
    else {
      val bp = qs(param)
      Try {
        bp.mkString.toBoolean
      } match {
        case Success(b) => b == true
        case Failure(_) => throw new BadRequestException(s"Value of '$param' parameter must be true or false. found: $bp")
      }
    }
  }
}

