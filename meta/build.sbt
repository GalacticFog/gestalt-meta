import sbt._

import com.typesafe.sbt.packager.docker._

name := """gestalt-meta"""

version := "0.7.3"

maintainer in Docker := "Chris Baker <chris@galacticfog.com>"

testFrameworks := Seq(TestFrameworks.Specs2)

dockerBaseImage := "openjdk:8-jre-alpine"

dockerCommands := dockerCommands.value.flatMap {
  case cmd@Cmd("FROM",_) => List(
    cmd,
    Cmd("RUN", "apk --no-cache add bash curl python py-crcmod bash libc6-compat openssh-client git gnupg && ln -s /lib /lib64 && rm -rf /var/cache/apk/*")
  )
  case other => List(other)
}

parallelExecution in Test := false

buildInfoOptions += BuildInfoOption.BuildTime

buildInfoOptions += BuildInfoOption.ToMap

buildInfoOptions += BuildInfoOption.ToJson

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Managed

javaOptions in Universal ++= Seq(
        "-Djava.util.prefs.systemRoot=/tmp/.java",
        "-Djava.util.prefs.userRoot=/tmp/.userPrefs"
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
  "com.galacticfog" %% "gestalt-meta-repository"       % "0.8.5" withSources(),
  "com.galacticfog" %% "gestalt-security-sdk-scala"    % "2.4.5-SNAPSHOT" withSources(),
  "com.galacticfog" %% "gestalt-security-play"         % "4.1.0" withSources(),
  "com.galacticfog" %% "gestalt-security-play-testkit" % "4.1.0" withSources(),
  "com.galacticfog"  % "gestalt-license-keymgr"        % "1.2.2-SNAPSHOT",
  "com.galacticfog" %% "gestalt-caas-kube"             % "0.3.6" withSources(),
  "com.galacticfog" %% "gestalt-play-json"             % "0.5.0",
  "net.codingwell"  %% "scala-guice"                   % "4.2.1",
  "org.slf4j"        % "slf4j-api"                     % "1.7.21",
  "ch.qos.logback"   % "logback-classic"               % "1.1.7",
  "org.postgresql"   % "postgresql"                    % "9.4.1208.jre7",
  "com.rabbitmq"     % "amqp-client"                   % "3.6.6",
  "io.jsonwebtoken"  % "jjwt"                          % "0.7.0",
  "com.spotify"      % "docker-client"                 % "8.7.1",
  "com.amazonaws"    % "aws-java-sdk-ecs"              % "1.11.410",

  "com.lihaoyi"     %% "scalatags"                     % "0.6.7",
  "org.scala-lang"   % "scala-reflect"                 % "2.11.8",
  "org.scala-lang"   % "scala-compiler"                % "2.11.8",
  "org.scalaz"      %% "scalaz-core"                   % "7.1.12",

  "org.scalikejdbc" %% "scalikejdbc-config"            % "2.5.1",
  "org.scalikejdbc" %% "scalikejdbc-play-initializer"  % "2.5.1",

  "de.leanovate.play-mockws"  %% "play-mockws"          % "2.5.1"       % Test,
  "com.typesafe.play"         %% "play-specs2"          % play.core.PlayVersion.current  % Test,
  "org.specs2"                %% "specs2-matcher-extra" % "3.6.6"       % Test,
  "org.mockito"                % "mockito-core"         % "1.10.19"     % Test,
  "com.typesafe.akka"         %% "akka-testkit"         % "2.3.10"      % Test
)

routesGenerator := InjectedRoutesGenerator
