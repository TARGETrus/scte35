name := "scte35"

libraryDependencies ++= Seq(
  "commons-codec" % "commons-codec" % "1.10",
  // tests
  "org.scalatest" %% "scalatest" % "3.0.5" % Test
)

version := "0.1"

scalaVersion := "2.12.8"