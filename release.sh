
sbt assembly

cp "$(find "target" -type f -name "myscalc.jar")" myscalc.jar

zip myscalc.zip myscalc.jar Readme.md License

rm myscalc.jar
