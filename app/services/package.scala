
import java.util.UUID
import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.play.silhouette.AuthAccountWithCreds

package object services {
  
  def uuid() = UUID.randomUUID()
  
  /*
  private[this] def ownerFromAccount(account: AuthAccountWithCreds): ResourceOwnerLink = {
    toOwnerLink(
      ResourceIds.User,
      account.account.id, 
      name = Some(account.account.name), 
      orgId = account.account.directory.orgId )  
  }
   */
  
  val uinfo = Map(
      "id" -> "",
      "username" -> "")
      //id: UUID, name: String, description: Option[String], orgId: UUID
  
      val dinfo = Map(
      "org" -> ""
  )
  
  def dummyAuthAccountWithCreds(
      userInfo: Map[String,String] = Map.empty,
      directoryInfo: Map[String,String] = Map.empty,
      authHeader: Option[String] = None): AuthAccountWithCreds = {
    
    val defaultStr  = "foo"
    val header      = authHeader getOrElse s"Bearer ${uuid()}"
    val credentials = GestaltAPICredentials.getCredentials(header).get    
    val directory   = GestaltDirectory(
      id = uuid(),
      name = defaultStr,
      description = None,
      orgId = directoryInfo.get("org") map (UUID.fromString(_)) getOrElse uuid(),
      directoryType = DIRECTORY_TYPE_INTERNAL.label
    )

    val account = GestaltAccount(
        userInfo.get("id") map (UUID.fromString(_)) getOrElse uuid(),
        userInfo.getOrElse("username",  defaultStr), 
        userInfo.getOrElse("firstName", defaultStr),
        userInfo.getOrElse("lastName",  defaultStr),
        userInfo.get("description") orElse Option(defaultStr),
        userInfo.get("email")       orElse Option(defaultStr),
        userInfo.get("phoneNumber") orElse Option(defaultStr),
        directory)
        
    val t = AuthAccountWithCreds(account, Seq(), Seq(), credentials, uuid())
    val u = t.account
    
    t
  }  
  
  
}