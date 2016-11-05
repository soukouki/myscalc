
# 変数の設定
read newVersion
directory="myscalc-$newVersion"

sbt assembly

# zipファイルの作成
mkdir $directory
cp $(ls target/scala-*/myscalc.jar) Readme.md License $directory
echo 'java -jar (join-path (pwd) myscalc.jar) $ARGS' > $directory/myscalc.ps1
zip -r "myscalc-$newVersion.zip" $directory
rm -r $directory
