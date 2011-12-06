name := "Nemo"

version := "1.0"

scalaVersion := "2.9.1"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.1"

scalacOptions ++= Seq("-deprecation")

mainClass in (Compile, run) := Some("ChessUI")
