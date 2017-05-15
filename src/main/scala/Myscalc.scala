package myscalc

import scala.io.StdIn._

import myscalc.variables.Variables
import myscalc.Calc


/** myscalcの引数の処理とか */
object Myscalc {
	
	val versionMesseage = "v2.0.0"
	
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
		val in = readLine()
		if(in.length >= 1 && in.charAt(0) == '\u0004') return
		val (nca, ss) = ca.calc(in)
		println(ss.mkString("\n"))
		loop(nca)
	}
	
	val helpMesseage: String = """
myscalc help message

-h | --help  ヘルプメッセージを表示

その他の引数は実行され、終了します。

インタプリタモード
	引数がないとこれになります。
	ctrl+d で終了します。
"""
}
