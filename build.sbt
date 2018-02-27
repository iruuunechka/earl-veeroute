lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  scalaVersion := "2.12.4"
)

lazy val earlVeeRoute = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "earl-veeroute", version := "0.0")
  .settings(libraryDependencies ++= Seq(
    "org.scalaj" %% "scalaj-http" % "2.3.0",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test"
  ))
