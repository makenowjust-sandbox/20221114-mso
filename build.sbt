Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "mso",
    console / initialCommands := "import codes.quine.labs.mso._",
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
  )
