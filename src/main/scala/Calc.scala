package myscalc

import scala.io.StdIn._

import myscalc.calc.Base
import myscalc.variables.Variables

/** myscalcの引数の処理とか */
object Myscalc {
	def main(args: Array[String]) = {
		if(args.find(_ == "-h").nonEmpty || args.find(_ == "--help").nonEmpty)
			println(helpMesseage)
		else loop(Variables(Map()))
	}
	private def loop(va: Variables): Unit = {
		val in = readLine()
		if(in.charAt(0) == '\u0004') return
		val (nva, ss) = Calc(va).calc(in)
		println(ss)
		loop(va)
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

/** [[myscalc.calc]]の処理を汎用化したもの */
case class Calc(va: Variables) {
	def calc(f: String): (Calc, List[String]) = {
		Parse(f) match {
			case Left(msg) => (this, List(msg))
			case Right(tree) => {
				val (lastVa, ss) = calci(tree, va, List())
				(Calc(lastVa), ss.reverse)
			}
		}
	}
	private def calci(tree: Base, va: Variables, ss: List[String]): (Variables, List[String]) = {
		tree.hasFinished(va) match {
			case true => (va, tree.string :: ss)
			case false => {
				val (ntree, nva) = tree.advance(va)
				calci(ntree, va, tree.string :: ss)
			}
		}
	}
}
