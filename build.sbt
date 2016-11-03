
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

assemblyJarName in assembly := "myscalc.jar"
