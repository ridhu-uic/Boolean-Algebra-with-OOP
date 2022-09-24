ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.3"

lazy val root = (project in file("."))
  .settings(
    name := "Assignment 1"
  )

"org.scalatest" %% "scalatest" % scalacticVersion % Test,
"org.scalatest" %% "scalatest-featurespec" % scalacticVersion % Test,


