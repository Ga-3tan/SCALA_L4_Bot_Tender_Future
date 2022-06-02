val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Bot-tender",
    version := "0.2.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= List(
        "com.lihaoyi" %% "scalatags" % "0.11.1",
        "com.lihaoyi" %% "cask" % "0.8.0",
    ),
  )
