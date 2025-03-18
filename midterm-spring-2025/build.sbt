ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
    .settings(
        name := "midterm-spring-2025"
    )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.2" % "test"