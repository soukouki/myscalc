
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

libraryDependencies ++= Seq(
	"org.scalatest" %% "scalatest" % "3.0.0" % "test"
)

// 次の行はスクリプトで編集されるので、フォーマットを変更しないこと
val myscalcVersion = "0.0.0"

assemblyJarName in assembly := "myscalc-" + myscalcVersion + ".jar"
