import Dependencies._

import com.typesafe.sbt.packager.docker._

name := """gestalt-meta"""

organization := "com.galacticfog"


version := "0.6.2"


maintainer in Docker := "Chris Baker <chris@galacticfog.com>"

resolvers ++= Seq(
    "gestalt-snapshots" at "https://galacticfog.artifactoryonline.com/galacticfog/libs-snapshots-local",
    "gestalt-releases" at  "https://galacticfog.artifactoryonline.com/galacticfog/libs-releases-local",
    "snapshots" at "http://scala-tools.org/repo-snapshots",
    "releases"  at "http://scala-tools.org/repo-releases",
    "Atlassian Releases" at "https://maven.atlassian.com/public/",
    "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
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

javaOptions in Universal ++= Seq(
        "-Djava.util.prefs.systemRoot=/tmp/.java",
        "-Djava.util.prefs.userRoot=/tmp/.userPrefs"
)

scalaVersion := "2.11.8"


scalacOptions ++= Seq(
  "-deprecation", 	// Emit warning and location for usages of deprecated APIs.
  "-feature", 		// Emit warning and location for usages of features that should be imported explicitly.
  "-unchecked") 	// Enable additional warnings where generated code depends on assumptions.
  
  //"-Xlint" 		// Enable recommended additional warnings.
//)

//scalikejdbcSettings


libraryDependencies ++= Seq(
	"com.galacticfog" %% "gestalt-meta-repository" 		 % "0.6.5" withSources(),
	"com.galacticfog" %% "gestalt-play-json" 			 % "0.3.0" withSources(),
	"com.galacticfog" %% "gestalt-security-play" 		 % "3.0.3" withSources(),
	"com.galacticfog"  % "gestalt-license-keymgr" 		 % "1.2.2-SNAPSHOT",
	"com.galacticfog" %% "gestalt-caas-kube" 			 % "0.1.0" withSources(),
    "net.codingwell"  %% "scala-guice" % "4.1.0",

    "org.slf4j" 	   % "slf4j-api" 		% "1.7.21",
	"ch.qos.logback"   % "logback-classic" 	% "1.1.2",
	"org.postgresql"   % "postgresql" 		% "9.3-1102-jdbc4",
	"com.rabbitmq"     % "amqp-client" 		% "3.6.1",	

	"com.galacticfog" %% "gestalt-security-play-testkit" % "3.0.3" withSources(),

	Library.Play.specs2          % Test,
	Library.Specs2.matcherExtra  % Test,
	
	Library.mockito              % Test,
	Library.akkaTestkit          % Test,
	
  	jdbc,
	cache,
	ws,
    filters
)

scalacOptions in Test ++= Seq("-Yrangepos")

routesGenerator := InjectedRoutesGenerator
