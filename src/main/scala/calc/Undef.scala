package myscalc.calc

import myscalc.variables.Variables

case class Undef() extends Base {
	override def advance(va: Variables) = sys.error("未定義にadvanceされた")
	override def hasFinished(va: Variables): Boolean = true
	override def string: String = "Undef"
}
