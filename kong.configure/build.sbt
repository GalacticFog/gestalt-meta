import com.typesafe.sbt.SbtGit._

versionWithGit

name := """container.import"""

libraryDependencies ++= Seq(
  "com.typesafe.play" %% "play-json" % "2.5.12",
  "com.typesafe.play" %% "play-ws" % "2.5.12",
  "io.jsonwebtoken" % "jjwt" % "0.7.0",
  "com.amazonaws" % "aws-java-sdk-ecs" % "1.11.410",
  "org.typelevel" %% "cats-core" % "1.0.1"
)

scalacOptions ++= Seq(
  "-language:implicitConversions",
  "-feature",
  "-Xfatal-warnings",
  "-Ywarn-value-discard",
  "-Ywarn-unused-import"
)

version in ThisBuild := "0.1.0-SNAPSHOT"

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