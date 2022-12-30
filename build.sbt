name := "scala-match-deconstruct"

version := "0.0.1"

ThisBuild / scalaVersion := "3.2.1"

scalacOptions ++= Seq(
  "-Xcheck-macros",
)

libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "0.7.29",
)

testFrameworks += new TestFramework("munit.Framework")
