import com.typesafe.sbt.packager.docker._

name := """gestalt-meta"""

organization := "com.galacticfog"

version := "0.4.6"

maintainer in Docker := "Chris Baker <chris@galacticfog.com>"

dockerBaseImage := "java:8-jre-alpine"

dockerCommands := dockerCommands.value.flatMap {
  case cmd@Cmd("FROM",_) => List(
    cmd,
    Cmd("RUN", "apk add --update bash && rm -rf /var/cache/apk/*")     
  )
  case other => List(other)
}

lazy val root = (project in file(".")).
  enablePlugins(PlayScala,SbtNativePackager).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](
      name, version, scalaVersion, sbtVersion,
      "builtBy" -> System.getProperty("user.name"),
      "gitHash" -> new java.lang.Object(){
              override def toString(): String = {
                      try { 
                    val extracted = new java.io.InputStreamReader(
                              java.lang.Runtime.getRuntime().exec("git rev-parse HEAD").getInputStream())                         
                    (new java.io.BufferedReader(extracted)).readLine()
                      } catch {      case t: Throwable => "get git hash failed"    }
              }}.toString()
    ),
    buildInfoPackage := "com.galacticfog.gestalt.meta.api"
  )

buildInfoOptions += BuildInfoOption.BuildTime

buildInfoOptions += BuildInfoOption.ToMap

buildInfoOptions += BuildInfoOption.ToJson


EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

scalaVersion := "2.11.8"


scalacOptions ++= Seq(
  "-deprecation", 	// Emit warning and location for usages of deprecated APIs.
  "-feature", 			// Emit warning and location for usages of features that should be imported explicitly.
  "-unchecked") 		// Enable additional warnings where generated code depends on assumptions.
  
  //"-Xlint" 				// Enable recommended additional warnings.
  // "-Ywarn-adapted-args", 	// Warn if an argument list is modified to match the receiver.
  // "-Ywarn-dead-code", 	// Warn when dead code is identified.
  //"-Ywarn-inaccessible", 	// Warn about inaccessible types in method signatures.
  //"-Ywarn-nullary-override" // Warn when non-nullary overrides nullary, e.g. def foo() over def foo.
  // "-Ywarn-numeric-widen" // Warn when numerics are widened.
  // "-Xfatal-warnings", 	// Fail the compilation if there are any warnings.
//)


resolvers ++= Seq(
  "gestalt-snapshots" at "https://galacticfog.artifactoryonline.com/galacticfog/libs-snapshots-local",
  "gestalt-releases" at "https://galacticfog.artifactoryonline.com/galacticfog/libs-releases-local",
  "snapshots" at "http://scala-tools.org/repo-snapshots",
  "releases"  at "http://scala-tools.org/repo-releases",
  "Atlassian Releases" at "https://maven.atlassian.com/public/"
)

scalikejdbcSettings

libraryDependencies ++= Seq(
	jdbc,
	cache,
	ws,
	"com.galacticfog" %% "gestalt-meta-repository" 		% "0.4.7" withSources(),
	"com.galacticfog" %% "gestalt-meta-sdk-scala" 		% "0.4.0" withSources(),
	"com.galacticfog" %% "gestalt-play-json" 			% "0.1.1" withSources(),
	"com.galacticfog" %% "gestalt-security-play" 		% "2.2.5-SNAPSHOT" withSources(),
	"com.galacticfog" %% "gestalt-security-sdk-scala" 	% "2.2.6-SNAPSHOT" withSources(),
	"com.galacticfog" % "gestalt-license-keymgr" 		% "1.1.0-SNAPSHOT"
)


libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.2.2"



// MockWS for testing
libraryDependencies += "de.leanovate.play-mockws" %% "play-mockws" % "2.3.0" % "test" withSources()



libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.10"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1102-jdbc4"

libraryDependencies += "org.scalikejdbc" % "scalikejdbc_2.11" % "2.2.6"

libraryDependencies += "org.scalikejdbc" % "scalikejdbc-test_2.11"   % "2.2.6"   % "test"

libraryDependencies += "org.scalikejdbc" % "scalikejdbc-config_2.11" % "2.2.6" % "test"

libraryDependencies += "com.typesafe.play" % "play-json_2.11" % "2.4.0-M2"

libraryDependencies += "com.rabbitmq" % "amqp-client" % "3.6.1"


libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.4" % "test")

libraryDependencies ++= Seq("org.specs2" %% "specs2-matcher-extra" % "3.8.4" % "test")

libraryDependencies ++= Seq("org.specs2" %% "specs2-mock" % "3.8.4" % "test")

scalacOptions in Test ++= Seq("-Yrangepos")

