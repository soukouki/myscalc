package myscalc

import scala.io.StdIn._
import myscalc.calc.Base

object Calc {
	def main(args: Array[String]) = {
		val in = readLine()
		Parse(in) match {
			case Left(msg) => println("parse error:" + msg)
			case Right(tree) => calc(tree, "")
		}
	}
	private def calc(tree: Base, old: String): Unit = {
		if(tree.string != old) println(tree.string)
		if(tree.hasFinished) calc(tree.advance, tree.string)
	}
}
