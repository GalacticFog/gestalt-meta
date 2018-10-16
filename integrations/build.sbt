import com.typesafe.sbt.SbtGit._

versionWithGit

name := """integrations"""

val awsJavaSdkVersion = "1.11.410"

libraryDependencies ++= Seq(
  "com.galacticfog" %% "gestalt-meta-repository" % "0.8.5" withSources(),
  "com.galacticfog" %% "gestalt-security-sdk-scala" % "2.4.5-SNAPSHOT" withSources(),
  "com.typesafe.play" %% "play-json" % "2.5.12",
  "com.typesafe.play" %% "play-ws" % "2.5.12",
  "io.jsonwebtoken" % "jjwt" % "0.7.0",
  "com.amazonaws" % "aws-java-sdk-ecs" % awsJavaSdkVersion,
  "com.amazonaws" % "aws-java-sdk-elasticloadbalancingv2" % awsJavaSdkVersion,
  "com.amazonaws" % "aws-java-sdk-ec2" % awsJavaSdkVersion,
  "org.typelevel" %% "cats-core" % "1.0.1",
  "org.specs2" %% "specs2-core" % "4.3.4" % Test,
  "de.leanovate.play-mockws" %% "play-mockws" % "2.5.1" % Test
)

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

fork in Test := true

// version in ThisBuild := "0.1.0-SNAPSHOT"

git.baseVersion := "0.1.0"
git.useGitDescribe := true