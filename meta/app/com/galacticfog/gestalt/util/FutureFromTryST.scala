package com.galacticfog.gestalt.util

import scala.language.implicitConversions
import scala.concurrent.Future
import scala.util.{Failure,Try}

object FutureFromTryST {
  private[this] def logStackTrace[A](t: Try[A]): Try[A] = {
    t recoverWith { case throwable => 
      throwable.printStackTrace()
      Failure(throwable)
    }
  }

  implicit def futureFromTryST(x: Future.type) = new {
    def fromTryST[A](t: Try[A]): Future[A] = Future.fromTry(logStackTrace(t))
  }
}