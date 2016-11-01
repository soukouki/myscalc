package myscalc

import myscalc.calc.Base

object Calc {
	def main(args: Array[String]) = {
		val in = readLine()
		val tree = Parse(in)
		println(tree)
		calc(tree)
	}
	def calc(tree: Base): Unit = {
		val newTree = tree.result
		println(newTree)
		if(newTree.isContinue) {calc(newTree)}
	}
}
