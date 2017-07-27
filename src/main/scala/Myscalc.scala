package myscalc

import scala.io.StdIn._

import myscalc.variables.Variables


/** myscalcの引数の処理とか */
object Myscalc {
	
	val version = "v2.0.2"
	
	def main(args: Array[String]) = {
		if(args.find(_ == "-h").nonEmpty || args.find(_ == "--help").nonEmpty) println(helpMesseage)
		else if (args.find(_ == "-v").nonEmpty || args.find(_ == "--version").nonEmpty) println(versionMesseage)
		else if(args.nonEmpty) println(calcArgs(args))
		else loop(Calc(Variables(Map())))
	}
	private def calcArgs(args: Seq[String]): String = {
		val (_, res) = args.foldLeft((Calc(Variables(Map())), List[String]())){(tup, inF) => {
			val (ca, res) = tup
			val (nca, addres) = ca.calc(inF)
			(nca, res ++ addres)
		}}
		res.mkString("\n")
	}
	private def loop(ca: Calc): Unit = {
		Option(readLine()).foreach{s =>
			if(s.length >= 1 && s.charAt(0) == '\u0004') return
			val (nca, ss) = ca.calc(s)
			println(ss.mkString("\n"))
			loop(nca)
		}
	}
	
	val helpMesseage: String = """
myscalc help message

-h | --help	| ヘルプメッセージを表示
-v | --version	| バージョンメッセージを表示

その他の引数はその値で計算され、終了します。

インタプリタモード
	引数がないとこれになります。
	ctrl+d を実行すると終了します。
"""
	val versionMesseage = "myscalc " + version
}
