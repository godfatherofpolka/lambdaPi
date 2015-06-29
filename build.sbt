name := """lambdaPi"""

version := "1.0"

scalaVersion := "2.11.6"

mainClass in (Compile, run) := Some("simply.TestSimply")

// Change this to another test framework if you prefer
libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

// Uncomment to use Akka
//libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.3.11"



fork in run := true
