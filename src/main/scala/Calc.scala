package myscalc

import scala.io.StdIn._

import myscalc.calc.Base
import myscalc.variables.Variables

/** [[myscalc.calc]]の処理を汎用化したもの */
case class Calc(va: Variables) {
	def calc(f: String): (Calc, List[String]) = {
		Parse(f) match {
			case Left(msg) => (this, List(msg))
			case Right(tree) => {
				val (lastVa, ss) = calci(tree, va, List())
				(Calc(lastVa), ss.reverse.distinct)
			}
		}
	}
	private def calci(tree: Base, va: Variables, ss: List[String]): (Variables, List[String]) = {
		tree.hasFinished(va) match {
			case true => (va, tree.string :: ss)
			case false => {
				val (ntree, nva) = tree.advance(va)
				calci(ntree, nva, tree.string :: ss)
			}
		}
	}
}
