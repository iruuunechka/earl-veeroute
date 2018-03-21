val circeVersion = "0.9.1"

lazy val earlVeeRoute = project
  .in(file("."))
  .settings(organization := "ru.ifmo", scalaVersion := "2.12.4")
  .settings(name := "earl-veeroute", version := "0.0")
  .settings(libraryDependencies ++= Seq(
    "org.scalaj" %% "scalaj-http" % "2.3.0",
    "org.scalatest" %% "scalatest" % "3.0.4" % "test"
  ))
  .settings(libraryDependencies ++= Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion))
