
num="[0-9]*"
read newVersion

sed -i "s/val myscalcVersion = \"$num.$num.$num\"/val myscalcVersion = \"$newVersion\"/" build.sbt
