name := "Nemo"

version := "1.0"

scalaVersion := "2.9.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.1"

libraryDependencies += "se.scalablesolutions.akka" % "akka-actor" % "1.2"

scalacOptions ++= Seq("-deprecation")

mainClass in (Compile, run) := Some("BasicNemoTest")

// mainClass in (Compile, run) := Some("GameOfLife")

fork in run := true

javaOptions in run += "-Xmx1G"
