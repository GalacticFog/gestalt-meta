package controllers.util

import scala.language.postfixOps
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import com.galacticfog.gestalt.meta.api.errors.BadRequestException


object QueryString {
  
  /**
   * Get a single parameter value from the querystring Map. Throws an exception if
   * the parameter is given more than once.
   * 
   * @param qs Map from which to extract the param value
   * @param param Key of the param value to extract
   * @param strict when true an error will be thrown if the param is present but no value is given
   */
  def single[V](qs: Map[String,Seq[V]], param: String, strict: Boolean = false): Option[V] = {
    qs.get(param).flatMap { buf =>
      
      buf.size match {
        case 1 => 
          Some(buf.head)
        case e if e > 1 => 
          throw badRequest(s"Too many values found for '$param'. Expected: 1, Found: ${buf.size}")
        case e if e == 0 => {
          if (strict) errorNoValue(param) else Option.empty[V]
        }
      }
    }
  }
  
  /**
   * Get a list of one or more values identified by key from the querystring Map. 
   * 
   * @param qs Map from which to extract param values
   * @param param key of the param values to extract
   * @param strict when true an error will be thrown if the param is present but no value is given
   */
  def list[V](qs: Map[String,Seq[V]], param: String, strict: Boolean = false): Seq[V] = {
    val results = qs.getOrElse(param, Seq.empty[V])
    if (results.isEmpty && strict) errorNoValue(param) else results
  }

  /**
   * Get a single Boolean value from the querystring Map.
   */
  def singleBoolean[V](qs: Map[String, Seq[V]], param: String, strict: Boolean = false): Boolean = {
    single(qs, param).fold {
      if (strict) errorNoValue(param) else false
    }{ p =>
      Try(p.toString.toBoolean) match {
        case Success(b) => b == true
        case Failure(_) => 
          throw new BadRequestException(s"Value of '$param' parameter must be true or false. found: $p")
      }
    }
  }
  
  private def badRequest(message: String) = new BadRequestException(message)
  
  private def errorNoValue(param: String) =
    throw badRequest(s"Given param '$param' must have a value")
  
}
