import Dependencies._

import com.typesafe.sbt.packager.docker._

name := """gestalt-meta"""

organization := "com.galacticfog"

version := "0.7.0"

maintainer in Docker := "Chris Baker <chris@galacticfog.com>"

testFrameworks := Seq(TestFrameworks.Specs2)

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
  "-deprecation",   // Emit warning and location for usages of deprecated APIs.
  "-feature",       // Emit warning and location for usages of features that should be imported explicitly.
  "-unchecked"      // Enable additional warnings where generated code depends on assumptions.
  //"-Xlint"        // Enable recommended additional warnings.
)

javaOptions in Test ++= Seq("-Dconfig.file=test/resources/application.test.conf", 
                            "-Dlogger.file=test/resources/logback-test.xml")

scalacOptions in Test ++= Seq("-Yrangepos")

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  filters,

  "org.clapper"     %% "scalasti"                      % "3.0.1",
  "org.jtwig"        % "jtwig-core"                    % "5.86.0.RELEASE",
  "com.galacticfog" %% "gestalt-meta-repository"       % "0.8.1" withSources(),
  "com.galacticfog" %% "gestalt-security-sdk-scala"    % "2.4.5-SNAPSHOT" withSources(),
  "com.galacticfog" %% "gestalt-security-play"         % "4.0.1-SNAPSHOT" withSources(),
  "com.galacticfog" %% "gestalt-security-play-testkit" % "4.0.1-SNAPSHOT" withSources(),
  "com.galacticfog"  % "gestalt-license-keymgr"        % "1.2.2-SNAPSHOT",
  "com.galacticfog" %% "gestalt-caas-kube"             % "0.3.3" withSources(),
  "com.galacticfog" %% "gestalt-play-json"             % "0.5.0",
  "net.codingwell"  %% "scala-guice"                   % "4.2.1",
  "org.slf4j"        % "slf4j-api"                     % "1.7.21",
  "ch.qos.logback"   % "logback-classic"               % "1.1.7",
  "org.postgresql"   % "postgresql"                    % "9.4.1208.jre7",
  "com.rabbitmq"     % "amqp-client"                   % "3.6.6",
  "io.jsonwebtoken"  % "jjwt"                          % "0.7.0",
  "com.spotify"      % "docker-client"                 % "8.7.1",

  "com.lihaoyi"     %% "scalatags"                     % "0.6.7",
  "org.scala-lang"   % "scala-reflect"                 % "2.11.8",
  "org.scala-lang"   % "scala-compiler"                % "2.11.8",
  "org.scalaz"      %% "scalaz-core"                   % "7.1.12",

  "org.scalikejdbc" %% "scalikejdbc-config"            % "2.5.1",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer"  % "2.5.1",

  "de.leanovate.play-mockws"  %% "play-mockws" % "2.5.1" % Test,
  Library.Play.specs2          % Test,
  Library.Specs2.matcherExtra  % Test,
  Library.mockito              % Test,
  Library.akkaTestkit          % Test
)

routesGenerator := InjectedRoutesGenerator
