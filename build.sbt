lazy val commonSettings = Seq(
  organization := "ru.ifmo",
  scalaVersion := "2.12.4"
)

lazy val earlVeeRoute = project
  .in(file("."))
  .settings(commonSettings :_*)
  .settings(name := "earl-veeroute", version := "0.0")
        
