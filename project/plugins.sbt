// Comment to get more information during initialization
//logLevel := Level.Warn

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")

// The Play plugin
addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.8")

// sbt-ecplise plugin
addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "3.0.0")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.6.1")

// web plugins

// addSbtPlugin("com.typesafe.sbt" % "sbt-coffeescript" % "1.0.0")

// addSbtPlugin("com.typesafe.sbt" % "sbt-less" % "1.1.0")

// addSbtPlugin("com.typesafe.sbt" % "sbt-jshint" % "1.0.3")

// addSbtPlugin("com.typesafe.sbt" % "sbt-rjs" % "1.0.7")

// addSbtPlugin("com.typesafe.sbt" % "sbt-digest" % "1.1.0")

// addSbtPlugin("com.typesafe.sbt" % "sbt-mocha" % "1.1.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.1.5")

// Driver needed here for scalike mapper.

//addSbtPlugin("org.scalikejdbc" %% "scalikejdbc-mapper-generator" % "2.2.6")

//libraryDependencies += "org.postgresql" % "postgresql" % "9.3-1102-jdbc4"

