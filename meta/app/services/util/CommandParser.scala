package services.util

import scala.language.postfixOps

object CommandParser {

  import java.util.{StringTokenizer, Vector}

  val NORMAL = 0
  val IN_QUOTE = 1
  val IN_DOUBLE_QUOTE = 2

  def translate(toProcess: String): List[String] = {

    if (toProcess == null || toProcess.isEmpty) {
      Array[String]()
    }

    var state = NORMAL;

    val tok = new StringTokenizer(toProcess, "\"\' ", true)
    val v = new Vector[String]
    var current = new StringBuffer()

    var lastTokenHasBeenQuoted = false;

    while (tok.hasMoreTokens()) {

      val nextTok = tok.nextToken();

      state match {
        case IN_QUOTE => {
          if ("\'".equals( nextTok )) {
            lastTokenHasBeenQuoted = true
            state = NORMAL
          } else {
            current.append( nextTok )
          }
        }
        case IN_DOUBLE_QUOTE => {
          if ("\"".equals( nextTok )) {
            lastTokenHasBeenQuoted = true
            state = NORMAL
          } else {
            current.append( nextTok )
          }
        }
        case _ => {
          if ("\'".equals(nextTok))      state = IN_QUOTE;
          else if ("\"".equals(nextTok)) state = IN_DOUBLE_QUOTE;
          else if (" ".equals(nextTok)) {
            if (lastTokenHasBeenQuoted || current.length() != 0) {
              v.addElement(current.toString());
              current = new StringBuffer();
            }
          }
          else {
            current.append(nextTok);
          }
          lastTokenHasBeenQuoted = false;
        }
      }
    } // while

    if (lastTokenHasBeenQuoted || current.length() != 0) {
      v.addElement(current.toString());
    }

    if (state == IN_QUOTE || state == IN_DOUBLE_QUOTE) {
      throw new IllegalArgumentException("unbalanced quotes in " + toProcess);
    }

    val args = new Array[Object]( v.size );
    v.copyInto(args);
    args map { a => a.toString } toList;
  }
}
