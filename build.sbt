ThisBuild / scalaVersion := "3.8.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "meta-programming",
    libraryDependencies ++= Seq(
      "org.postgresql" % "postgresql" % "42.7.11"
    ),
    scalacOptions ++= Seq(
      "-Vprint:postInlining",
      "-Xprint-suspension",
      // "-Xmax-inlines:100000",
      "-deprecation"
    )
  )
