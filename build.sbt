ThisBuild / scalaVersion := "3.2.2"
ThisBuild / organization := "com.jarrahtechnology"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / githubOwner := "jarrahtech"
ThisBuild / githubRepository := "hex"

lazy val root = project.in(file(".")).
  aggregate(hex.js, hex.jvm).
  settings(
    publish := {},
    publishLocal := {},   
  )

lazy val hex = crossProject(JSPlatform, JVMPlatform).in(file(".")).
  settings(
    name := "hex",
    version := "0.3.2",

    resolvers ++= Resolver.sonatypeOssRepos("public"),
    resolvers += Resolver.githubPackages("jarrahtech"),

    libraryDependencies += "com.jarrahtechnology" %%% "jarrah-util" % "0.6.0",

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.15" % "test",
    libraryDependencies += "org.scalatest" %%% "scalatest-funsuite" % "3.2.15" % "test",

    wartremoverErrors ++= Warts.unsafe,

    Test / logBuffered := false,  
  ).
  jvmSettings(
    scalacOptions ++= Seq(
      "-encoding", "utf8",
      "-Xfatal-warnings",
      "-deprecation",
    ),
    libraryDependencies += "org.scala-js" %% "scalajs-stubs" % "1.1.0" % "provided",
    
    Test / testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-fW", "./target/scalatest.txt"),  
  ).
  jsSettings(
    // Add JS-specific settings here
  )

lazy val hexJS = hex.js
lazy val hexJVM = hex.jvm
