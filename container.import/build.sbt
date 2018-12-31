import com.typesafe.sbt.SbtGit._

versionWithGit

name := """container.import"""

libraryDependencies ++= Seq(
  "com.galacticfog"   %% "gestalt-caas-kube"    % "0.3.6" withSources(),
  "com.galacticfog" %% "gestalt-meta-repository" % "0.8.5" withSources(),
  "com.galacticfog" %% "gestalt-security-sdk-scala" % "2.4.5-SNAPSHOT" withSources(),
  "com.typesafe.play" %% "play-json" % "2.5.12",
  "com.typesafe.play" %% "play-ws" % "2.5.12",
  "io.jsonwebtoken" % "jjwt" % "0.7.0",
  "com.amazonaws" % "aws-java-sdk-ecs" % "1.11.410",
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.specs2" %% "specs2-core" % "4.3.4" % Test,
  "de.leanovate.play-mockws" %% "play-mockws" % "2.5.1" % Test
)
excludeDependencies += "commons-logging" % "commons-logging"

scalacOptions ++= Seq(
  "-language:implicitConversions",
  "-feature",
  "-Xfatal-warnings",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import"
)
scalacOptions in Test ++= Seq("-Yrangepos")

testFrameworks := Seq(TestFrameworks.Specs2)

parallelExecution in Test := true

javaOptions in Test ++= Seq("-Dconfig.file=src/test/resources/application.test.conf", 
                            "-Dlogger.file=src/test/resources/logback-test.xml")

fork in Test := true

// version in ThisBuild := "0.1.0-SNAPSHOT"

publishTo in ThisBuild <<= version { (v: String) =>
  val ao = "https://galacticfog.artifactoryonline.com/galacticfog/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("publish-gf-snapshots" at ao + "libs-snapshots-local;build.timestamp=" + new java.util.Date().getTime)
  else
    Some("publish-gf-releases"  at ao + "libs-releases-local")
}
// enablePlugins(GitVersioning)

git.baseVersion := "0.1.0"
git.useGitDescribe := true

isSnapshot := true
publishMavenStyle := true
credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")