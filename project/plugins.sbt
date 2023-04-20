
// https://www.scala-sbt.org/1.x/docs/Plugins.html

//conflictManager := ConflictManager.latestRevision
//dependencyOverrides += "org.scala-lang.modules" %% "scala-xml" % "2.1.0"

addSbtPlugin("com.codecommit" % "sbt-github-packages" % "0.5.3")
//addSbtPlugin("com.orrsella" % "sbt-stats" % "1.0.7")
//addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0") // https://github.com/sbt/sbt-buildinfo
//addSbtPlugin("de.heikoseeberger" % "sbt-header" % "5.6.0") // https://github.com/sbt/sbt-header
//addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.0.0")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "3.1.0")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.13.1")
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.3.0")