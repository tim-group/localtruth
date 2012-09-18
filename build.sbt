name := "localtruth"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies ++=  Seq(
    // Test-only Dependencies
    "junit" % "junit" % "4.10" % "test",
    "org.specs2" %% "specs2" % "1.11" % "test",
    "org.scalacheck" %% "scalacheck" % "1.9" % "test"
)
