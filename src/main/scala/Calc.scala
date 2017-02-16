package myscalc

import scala.io.StdIn._

import myscalc.calc.Base
import myscalc.variables.Variables

object Calc {
	def main(args: Array[String]) = {
		val in = readLine()
		Parse(in) match {
			case Left(msg) => println("parse error:" + msg)
			case Right(tree) => calc(tree, Variables(Map()), "")
		}
	}
	private def calc(tree: Base, va: Variables, old: String): Unit = {
		if(tree.string != old) println(tree.string)
		if(!tree.hasFinished(va)) {
			val (new_tree, new_va) = tree.advance(va)
			calc(new_tree, new_va, tree.string)
		}
	}
}
