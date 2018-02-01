name := "radoc-core"

version := "0.1"

scalaVersion := "2.12.4"
val scalaTestVersion = "3.0.4"
val circeVersion = "0.9.1"
val scalaXmlVersion = "1.0.6"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
  "org.scalactic" %% "scalactic" % scalaTestVersion,
  "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion,
  "io.circe" %% "circe-parser" % circeVersion
)
