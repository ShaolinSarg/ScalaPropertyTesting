val scalaTest = "org.scalatest" %% "scalatest" % "2.2.5" % Test
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.12.6" % Test
val playJson = "com.typesafe.play" %% "play-json" % "2.3.0"

resolvers += "Typesafe Repo" at "http://repo.typesafe.com/typesafe/releases/"

lazy val commonSettings = Seq(
  organization := "com.meetup.fpne",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "FPNE Scala seed project",
    libraryDependencies ++= Seq(scalaTest, scalaCheck, playJson)
  )