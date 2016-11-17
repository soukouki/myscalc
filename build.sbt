
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

scalaVersion := "2.12.0"

libraryDependencies ++= Seq(
	"org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
	"org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

assemblyJarName in assembly := "myscalc.jar"
