import sbt._

object Dependencies {

  object Library {

    object Play {
      val version = play.core.PlayVersion.current
      val ws = "com.typesafe.play" %% "play-ws" % version
      val json = "com.typesafe.play" %% "play-json" % version
      val test = "com.typesafe.play" %% "play-test" % version
      val specs2 = "com.typesafe.play" %% "play-specs2" % version
    }

    object Gestalt {
      val securitySdk = "com.galacticfog" %% "gestalt-security-sdk-scala" % "2.3.0-SNAPSHOT"
    }
    
    object Mohiva {
      val version = "3.0.5"
      val silhouette        = "com.mohiva" %% "play-silhouette"          % version
      val silhouetteTestkit = "com.mohiva" %% "play-silhouette-testkit"  % version
    }

    object Specs2 {
      private val version = "3.6"
      val matcherExtra = "org.specs2" %% "specs2-matcher-extra" % version
    }

    val jbcrypt = "org.mindrot" % "jbcrypt" % "0.3m"
    val jwtCore = "com.atlassian.jwt" % "jwt-core" % "1.2.4"
    val jwtApi = "com.atlassian.jwt" % "jwt-api" % "1.2.4"
    val mockito = "org.mockito" % "mockito-core" % "1.10.19"
    val scalaGuice = "net.codingwell" %% "scala-guice" % "4.0.0"
    val akkaTestkit = "com.typesafe.akka" %% "akka-testkit" % "2.3.10"
  }
}


