import Dependencies._

import com.typesafe.sbt.packager.docker._

name := """gestalt-meta"""

organization := "com.galacticfog"

version := "0.5.2"

maintainer in Docker := "Chris Baker <chris@galacticfog.com>"

resolvers ++= Seq(
    "snapshots" at "http://scala-tools.org/repo-snapshots",
    "releases"  at "http://scala-tools.org/repo-releases",
    "Atlassian Releases" at "https://maven.atlassian.com/public/",
    "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases",
    "gestalt-snapshots" at "https://galacticfog.artifactoryonline.com/galacticfog/libs-snapshots-local",
    "gestalt-releases" at  "https://galacticfog.artifactoryonline.com/galacticfog/libs-releases-local"
)


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
  "-feature", 		// Emit warning and location for usages of features that should be imported explicitly.
  "-unchecked") 	// Enable additional warnings where generated code depends on assumptions.
  
  //"-Xlint" 		// Enable recommended additional warnings.
//)

//scalikejdbcSettings

libraryDependencies ++= Seq(

	"com.galacticfog" %% "gestalt-meta-repository" 		 % "0.4.11" withSources(),
	"com.galacticfog" %% "gestalt-meta-sdk-scala" 		 % "0.5.0" withSources(),
	"com.galacticfog" %% "gestalt-play-json" 			 % "0.3.0" withSources(),
	"com.galacticfog" %% "gestalt-security-play" 		 % "3.0.1" withSources(),
	"com.galacticfog" %% "gestalt-security-play-testkit" % "3.0.1" withSources(),
	"com.galacticfog"  % "gestalt-license-keymgr" 		 % "1.1.0-SNAPSHOT",
	
	"org.slf4j" 	   % "slf4j-api" 		% "1.7.21",
	"ch.qos.logback"   % "logback-classic" 	% "1.1.2",
	"org.postgresql"   % "postgresql" 		% "9.3-1102-jdbc4",
	"com.rabbitmq"     % "amqp-client" 		% "3.6.1",	
	
	Library.Play.specs2          % Test,
	Library.Specs2.matcherExtra  % Test,
	Library.mockito              % Test,
	Library.akkaTestkit          % Test,
	
  	jdbc,
	cache,
	ws
)


//libraryDependencies += "org.slf4j" % "slf4j-api" % "1.7.21"

//libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.2"

//libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1102-jdbc4"

//libraryDependencies += "com.rabbitmq" % "amqp-client" % "3.6.1"







//libraryDependencies += "com.typesafe.play" % "play-test_2.11" % "2.4.8" % "test"

//libraryDependencies += "org.specs2" % "specs2-junit_2.11" % "2.4.17" % "test"

//libraryDependencies += "org.specs2" % "specs2-core_2.11" % "2.4.17" % "test"

//libraryDependencies += "org.specs2" % "specs2-matcher-extra_2.11" % "2.4.17" % "test"

//libraryDependencies += "de.leanovate.play-mockws" %% "play-mockws" % "2.4.2" % "test" withSources()

//libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.2.2"



scalacOptions in Test ++= Seq("-Yrangepos")


routesGenerator := InjectedRoutesGenerator
