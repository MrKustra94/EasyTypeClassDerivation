ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .settings(
    name := "TypeclassDerivation"
  ).dependsOn(
    `json-encoder`,
    `encoder-macro`
  )

lazy val `json-encoder` = (project in file("./encoder"))
  .settings(
    name := "encoder"
  )

lazy val `encoder-macro` = (project in file("./encoder-macros"))
  .settings(name := "encoder-macro")
  .dependsOn(`json-encoder`)
