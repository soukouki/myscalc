package myscalc.calc

import myscalc.variables.Variables
import myscalc.variables.Key

/** 変数 */
case class Variable(key: Key) extends Base {
	override def advance(va: Variables): (Base, Variables) = (va.value(key), va)
	override def hasFinished(va: Variables): Boolean = !va.existing(key)
	override def string: String = key.toString
}
