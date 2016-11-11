package myscalc.calc.operatorbase
import myscalc.calc._
import myscalc.calc.operator.Operator

/**
[[Mul]][[Div]]よりも優先順位が低い演算子のtrait
*/
private[calc] trait AddSubOperator {
	protected def stringBase(symbol: Char, left: Base, right: Base): String = (left, right) match {
		case (_, _: AddSubOperator) => s"${left.string}$symbol(${right.string})"
		case _ => s"${left.string}$symbol${right.string}"
	}
}
private[calc] object AddSubOperator {
	def unapply(ope: AddSubOperator): Option[(Base, Base)] = {
		val Operator(l, r) = ope
		Option((l, r))
	}
}

/**
[[Add]][[Sub]]よりも優先順位が高い演算子のtrait
*/
private[calc] trait MulDivOperator {
	protected def stringBase(symbol: Char, left: Base, right: Base): String = right match {
		case _: MulDivOperator => s"${putParentheses(left)}$symbol(${right.string})"
		case _ => s"${putParentheses(left)}$symbol${putParentheses(right)}"
	}
	private def putParentheses(b: Base): String = b match {
		case _: AddSubOperator => s"(${b.string})"
		case _ => b.string
	}
}
private[calc] object MulDivOperator {
	def unapply(ope: MulDivOperator): Option[(Base, Base)] = {
		val Operator(l, r) = ope
		Option((l, r))
	}
}
