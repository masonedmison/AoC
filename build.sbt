ThisBuild / scalaVersion := "2.13.7"

lazy val root = project
  .in(file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.3.0"
    )
  )
