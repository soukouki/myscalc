package myscalc.calc

import myscalc.variables.Variables

sealed abstract class CalcError(val msg: String) extends Base {
	override def advance(v: Variables) = ???
	override def hasFinished(v: Variables) = true
	override def string = "ERROR: " + msg
}

package error {
	case class SubstitutionError() extends CalcError("すでに代入されている変数に、違う値を代入しようとしています")
}
