package myscalc.calc.operatorbase
/**
	[[Rational]]でも使われるため、[[operator]]から独立したパッケージになっている
	今はほぼ[[#string]]の処理のためにある
*/

import myscalc.calc._
import myscalc.calc.operator.Operator

/** [[Mul]][[Div]]よりも優先順位が低い演算子のtrait */
private[calc] trait AddSubOperator {
	protected def stringBase(left: Base, symbol: String, right: Base): String = (left, right) match {
		case (_, _) if right.isRightmostMinus => putParenthesesString(left, symbol, right)
		case (_, _: AddSubOperator) => putParenthesesString(left, symbol, right)
		case _ => s"${left.string}$symbol${right.string}"
	}
	
	private def putParenthesesString(l: Base, s: String, r: Base): String = s"${l.string}$s(${r.string})"
}

/** [[Add]][[Sub]]よりも優先順位が高い演算子のtrait */
private[calc] trait MulDivOperator {
	protected def stringBase(left: Base, symbol: String, right: Base): String = right match {
		case _: MulDivOperator => s"${putParentheses(left)}$symbol(${right.string})"
		case _ => s"${putParentheses(left)}$symbol${putParentheses(right)}"
	}
	private def putParentheses(b: Base): String = b match {
		case _: AddSubOperator => s"(${b.string})"
		case _ => b.string
	}
}
