name := """gestalt-meta"""

version := "0.1.0"

lazy val root = (project in file(".")).enablePlugins(PlayScala)


scalaVersion := "2.11.7"

maintainer in Docker := "Chris Baker <chris@galacticfog.com>"

dockerUpdateLatest := true

dockerRepository := Some("galacticfog.artifactoryonline.com")



//scalacOptions ++= Seq(
  //"-deprecation", 		// Emit warning and location for usages of deprecated APIs.
  //"-feature", 			// Emit warning and location for usages of features that should be imported explicitly.
  //"-unchecked" 		// Enable additional warnings where generated code depends on assumptions.
  // "-Xfatal-warnings", 	// Fail the compilation if there are any warnings.
  //"-Xlint", 				// Enable recommended additional warnings.
  // "-Ywarn-adapted-args", 	// Warn if an argument list is modified to match the receiver.
  // "-Ywarn-dead-code", 	// Warn when dead code is identified.
  //"-Ywarn-inaccessible", 	// Warn about inaccessible types in method signatures.
  //"-Ywarn-nullary-override" // Warn when non-nullary overrides nullary, e.g. def foo() over def foo.
  // "-Ywarn-numeric-widen" // Warn when numerics are widened.
//)


resolvers ++= Seq(
  "gestalt"   at "http://galacticfog.artifactoryonline.com/galacticfog/libs-snapshots-local",
  "snapshots" at "http://scala-tools.org/repo-snapshots",
  "releases"  at "http://scala-tools.org/repo-releases",
  "Atlassian Releases" at "https://maven.atlassian.com/public/"
)

credentials ++= {
  (for {
    realm 	 	    <- sys.env.get("GESTALT_RESOLVER_REALM")
    username 		<- sys.env.get("GESTALT_RESOLVER_USERNAME")
    resolverUrlStr  <- sys.env.get("GESTALT_RESOLVER_URL")
    resolverUrl 	<- scala.util.Try{url(resolverUrlStr)}.toOption
    password 		<- sys.env.get("GESTALT_RESOLVER_PASSWORD")
  } yield {
    Seq(Credentials(realm, resolverUrl.getHost, username, password))
  }) getOrElse(Seq())
}

resolvers ++= {
  sys.env.get("GESTALT_RESOLVER_URL") map {
    url => Seq("gestalt-resolver" at url)
  } getOrElse(Seq())
}

scalikejdbcSettings


libraryDependencies ++= Seq(
	jdbc,
	cache,
	ws,
	"com.galacticfog" % "gestalt-task-io_2.11" 	 	% "0.2.2",
	"com.galacticfog" % "gestalt-task-play_2.11" 	% "0.2.2",	
	"com.galacticfog" % "gestalt-streaming-io_2.11" % "0.1.3",
	"com.galacticfog" %% "gestalt-meta-repository" % "0.1.0",
	"com.galacticfog" %% "gestalt-security-play" % "1.2.0" withSources()
)


libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.10"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1102-jdbc4"

libraryDependencies += "org.scalikejdbc" % "scalikejdbc_2.11" % "2.2.6"

libraryDependencies += "org.scalikejdbc" % "scalikejdbc-test_2.11"   % "2.2.6"   % "test"

libraryDependencies += "org.scalikejdbc" % "scalikejdbc-config_2.11" % "2.2.6" % "test"

libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.4.0-M2"

