package myscalc.calc

import myscalc.variables.Variables
import myscalc.calc.Undef
import myscalc.calc.error.SubstitutionError

/**
	簡易実装なのでEqualSpec要確認
*/
case class Equal(left: Variable, right: Base) extends Base {
	override def advance(va: Variables): (Base, Variables) =
		if(va.existing(left.key) && va.value(left.key) != right) {
			(SubstitutionError(), va)
		}
		else {
			(Undef(), va.substitution(left.key, right))
		}
	override def hasFinished(va: Variables) = false
	override def string = s"${left.string}=${right.string}"
}
