import Dependencies._

import com.typesafe.sbt.packager.docker._

name := """gestalt-meta"""

organization := "com.galacticfog"

version := "0.6.101"

maintainer in Docker := "Chris Baker <chris@galacticfog.com>"

resolvers ++= Seq(
    "jwtig" at  "https://jcenter.bintray.com/",
    "gestalt-snapshots" at "https://galacticfog.artifactoryonline.com/galacticfog/libs-snapshots-local",
    "gestalt-releases" at  "https://galacticfog.artifactoryonline.com/galacticfog/libs-releases-local",
    "snapshots" at "http://scala-tools.org/repo-snapshots",
    "releases"  at "http://scala-tools.org/repo-releases",
    "Atlassian Releases" at "https://maven.atlassian.com/public/",
    "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
)

dockerBaseImage := "openjdk:8-jre-alpine"

dockerCommands := dockerCommands.value.flatMap {
  case cmd@Cmd("FROM",_) => List(
    cmd,
    Cmd("RUN", "apk add --update bash && rm -rf /var/cache/apk/*")     
  )
  case other => List(other)
}

parallelExecution in Test := false

lazy val root = (project in file(".")).
  enablePlugins(PlayScala,SbtNativePackager).
  enablePlugins(BuildInfoPlugin).
  settings(
    buildInfoKeys := Seq[BuildInfoKey](
      name, version, scalaVersion, sbtVersion,
      "builtBy" -> System.getProperty("user.name"),
      "gitHash" -> new java.lang.Object() {
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


javaOptions in Test += "-Dconfig.file=test/resources/application.test.conf"

libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.6.7"

libraryDependencies ++= Seq(

    "org.clapper" %% "scalasti" % "3.0.1",
	"org.jtwig" 	   	 % "jtwig-core" 					 			 		% "5.86.0.RELEASE",
	"com.galacticfog" %% "gestalt-meta-repository" 		 		% "0.6.77" withSources(),
	"com.galacticfog" %% "gestalt-play-json" 			 		 		% "0.3.0" withSources(),
	"com.galacticfog" %% "gestalt-security-sdk-scala"  		% "2.3.4" withSources(),
	"com.galacticfog" %% "gestalt-security-play" 		 	 		% "3.0.5" withSources(),
	"com.galacticfog" %% "gestalt-security-play-testkit" 	% "3.0.5" withSources(),
	"com.galacticfog"  % "gestalt-license-keymgr" 		 		% "1.2.2-SNAPSHOT",
	"com.galacticfog" %% "gestalt-caas-kube" 			 				% "0.2.5" withSources(),
  "net.codingwell"  %% "scala-guice" 					 					% "4.1.0",
  "org.slf4j" 	   	 % "slf4j-api" 											% "1.7.21",
	"ch.qos.logback"   % "logback-classic" 								% "1.1.7",
	"org.postgresql"   % "postgresql" 										% "9.4.1208.jre7",
	"com.rabbitmq"     % "amqp-client" 										% "3.6.6",
  "io.jsonwebtoken"  % "jjwt"            		 						% "0.7.0",
  "com.spotify" 	   % "docker-client" 									% "8.7.1",

  "org.scalikejdbc" %% "scalikejdbc-config"           	% "2.5.1",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer" 	% "2.5.1",

	Library.Play.specs2          % Test,
	Library.Specs2.matcherExtra  % Test,
	
	Library.mockito              % Test,
	Library.akkaTestkit          % Test,
	
    "de.leanovate.play-mockws" %% "play-mockws" % "2.4.2" % Test,

  	jdbc,
	cache,
	ws,
    filters
)

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.11.8"

libraryDependencies += "org.scala-lang" % "scala-compiler" % "2.11.8"

libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.1.12"

//libraryDependencies += "com.internetitem" % "logback-elasticsearch-appender" % "1.6"

scalacOptions in Test ++= Seq("-Yrangepos")

routesGenerator := InjectedRoutesGenerator
