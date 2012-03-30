import AssemblyKeys._ // put this at the top of the file

seq(assemblySettings: _*)

//seq(ScctPlugin.scctSettings: _*)

name := "Nemo"

version := "1.0"

scalaVersion := "2.9.1"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

libraryDependencies += "org.scala-lang" % "scala-swing" % "2.9.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.7.1" % "test"
)

// libraryDependencies += "ch.craven" %% "scct-plugin" % "0.2"

// libraryDependencies += "se.scalablesolutions.akka" % "akka-actor" % "1.2"

scalacOptions ++= Seq("-deprecation")

mainClass in (Compile, run) := Some("NemoApp")

// mainClass in (Compile, run) := Some("GameOfLife")

mainClass := Some("NemoApp")

fork in run := true

javaOptions in run ++= Seq("-Xms64M", "-Xmx1536M", "-Xss1M", "-XX:MaxPermSize=384M")
