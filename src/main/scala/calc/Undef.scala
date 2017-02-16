package myscalc.calc

import myscalc.variables.Variables

case class Undef() extends Base {
	def advance(v: Variables) = ???
	def hasFinished(v: Variables) = true
	def string = "Undef"
}
