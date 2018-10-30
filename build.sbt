import com.typesafe.sbt.packager.docker._

// looks like these don't take effect in any subproject as of now – commenting out to make this fact explicit
// scalacOptions ++= Seq(
//   "-deprecation",   // Emit warning and location for usages of deprecated APIs.
//   "-feature",       // Emit warning and location for usages of features that should be imported explicitly.
//   "-unchecked"      // Enable additional warnings where generated code depends on assumptions.
//   //"-Xlint"        // Enable recommended additional warnings.,
// )

resolvers in ThisBuild ++= Seq(
  "jwtig" at  "https://jcenter.bintray.com/",
  "gestalt-snapshots" at "https://galacticfog.artifactoryonline.com/galacticfog/libs-snapshots-local",
  "gestalt-releases" at  "https://galacticfog.artifactoryonline.com/galacticfog/libs-releases-local",
  "snapshots" at "http://scala-tools.org/repo-snapshots",
  "releases"  at "http://scala-tools.org/repo-releases",
  "Atlassian Releases" at "https://maven.atlassian.com/public/",
  "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
)

lazy val commonSettings = Seq(
  organization := "com.galacticfog",
  scalaVersion := "2.11.12"
)

lazy val meta = (project in file("meta")).
  enablePlugins(PlayScala,SbtNativePackager).
  enablePlugins(BuildInfoPlugin).
  settings(commonSettings: _*).
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
    buildInfoPackage := "com.galacticfog.gestalt.meta.api"//,
    // sources in (Compile,doc) := Seq.empty
  ).dependsOn(integrations)

lazy val containerImport = (project in file("container.import")).
  settings(commonSettings: _*).
  settings(
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    },
    assemblyShadeRules in assembly := Seq(      // until aws library is removed from laser jvm executor
      ShadeRule.rename("com.amazonaws.**" -> "shadeaws.@1").inAll
    )
  ).
  settings(
    artifact in (Compile, assembly) ~= { art =>
      art.copy(`classifier` = Some("assembly"))
    }
  ).
  settings(addArtifact(artifact in (Compile, assembly), assembly).settings: _*).
  dependsOn(meta % "test->test").dependsOn(integrations)

lazy val kongConfigure = (project in file("kong.configure")).
  settings(commonSettings: _*).
  settings(
    assemblyMergeStrategy in assembly := {
      case PathList("META-INF", xs @ _*) => MergeStrategy.discard
      case _ => MergeStrategy.first
    },
    assemblyShadeRules in assembly := Seq(      // until aws library is removed from laser jvm executor
      ShadeRule.rename("com.amazonaws.**" -> "shadeaws.@1").inAll
    )
  ).
  settings(
    artifact in (Compile, assembly) ~= { art =>
      art.copy(`classifier` = Some("assembly"))
    }
  ).
  settings(addArtifact(artifact in (Compile, assembly), assembly).settings: _*)

lazy val integrations = (project in file("integrations")).
  settings(commonSettings: _*)

lazy val root = (project in file(".")).
  aggregate(meta).
  aggregate(containerImport).
  aggregate(kongConfigure).
  aggregate(integrations).
  settings(commonSettings: _*).
  settings(
    publish := {},
    publishLocal := {},
    assemblyMergeStrategy in assembly := {
        case _  => MergeStrategy.discard
    }
  )