ThisBuild / scalaVersion := "3.2.0"
ThisBuild / organization := "com.jarrahtechnology"
ThisBuild / versionScheme := Some("early-semver")

lazy val root = (project in file("."))
  .settings(
    name := "hex",
    version := "0.1.0",

    scalacOptions ++= Seq(
      "-encoding", "utf8", // Option and arguments on same line
      "-Xfatal-warnings",  // New lines for each options
      "-deprecation",
    ),

    githubOwner := "jarrahtech",
    githubRepository := "hex",

    resolvers ++= Resolver.sonatypeOssRepos("public"),
    resolvers += Resolver.githubPackages("jarrahtech"),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    wartremoverErrors ++= Warts.unsafe
  )
