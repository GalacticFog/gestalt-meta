package controllers.util

import java.util.UUID

import com.galacticfog.gestalt.security.api._
import com.galacticfog.gestalt.security.play.silhouette.test.FakeGestaltSecurityEnvironment
import com.mohiva.play.silhouette.impl.authenticators.DummyAuthenticator
import controllers.{ResourceController, DeleteController, ContainerController}
import org.specs2.mock.Mockito
import play.api.test.PlaySpecification

trait GestaltSecurityMocking extends Mockito {

  private[this] def uuid() = UUID.randomUUID()

  class TestContainerController(val cs: ContainerService,
                                override val env: FakeGestaltSecurityEnvironment[DummyAuthenticator],
                                override val securityClient: GestaltSecurityClient) extends ContainerController(cs)

  class TestDeleteController(val cs: ContainerService,
                             override val env: FakeGestaltSecurityEnvironment[DummyAuthenticator],
                             override val securityClient: GestaltSecurityClient) extends DeleteController(cs)

  class TestResourceController(val cs: ContainerService,
                               override val env: FakeGestaltSecurityEnvironment[DummyAuthenticator],
                               override val securityClient: GestaltSecurityClient) extends ResourceController(cs)

  def dummyAuthAccount(userInfo: Map[String,String] = Map(), groups: Seq[ResourceLink] = Seq(), orgId: UUID = uuid()): GestaltAuthResponse = {
    val defaultStr = "foo"
    val directory = GestaltDirectory(uuid(), defaultStr, None, uuid())
    val account = GestaltAccount(
      userInfo.get("id") map (UUID.fromString(_)) getOrElse uuid(),
      userInfo.getOrElse("username", defaultStr),
      userInfo.getOrElse("firstName", defaultStr),
      userInfo.getOrElse("lastName", defaultStr),
      userInfo.get("description") orElse Option(defaultStr),
      userInfo.get("email") orElse Option(defaultStr),
      userInfo.get("phoneNumber") orElse Option(defaultStr),
      directory
    )
    GestaltAuthResponse(
      account = account,
      groups = groups,
      rights = Seq(),
      orgId = orgId
    )
  }

  def dummyCreds(authHeader: Option[String] = None): GestaltAPICredentials = {
    val header = authHeader getOrElse s"Bearer ${uuid()}"
    GestaltAPICredentials.getCredentials(header).get
  }

}

