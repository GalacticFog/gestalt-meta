package controllers.util.db

import com.galacticfog.gestalt.data.util.JdbcConnectionInfo

trait JdbcConfiguration {
  def isValid(): Boolean
  def getConnection: JdbcConnectionInfo
}