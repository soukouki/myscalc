package myscalc

import scala.io.StdIn._
import myscalc.calc.Base

object Calc {
	def main(args: Array[String]) = {
		val in = readLine()
		val tree = Parse(in)
		calc(tree, "")
	}
	def calc(tree: Base, old: String): Unit = {
		if(tree.string != old) println(tree.string)
		if(tree.isContinue) calc(tree.result, tree.string)
	}
}
