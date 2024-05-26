import Dependencies._

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "scala-sbt",
    libraryDependencies += "com.lihaoyi" %% "requests" % "0.8.2",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "3.3.1",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.wiremock" % "wiremock" % "3.6.0" % "test"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
