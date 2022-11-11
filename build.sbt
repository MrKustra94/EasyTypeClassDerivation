ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "TypeclassDerivation"
  ).dependsOn(`encoder`)

lazy val `encoder` = (project in file("./encoder"))
  .settings(name := "encoder")
