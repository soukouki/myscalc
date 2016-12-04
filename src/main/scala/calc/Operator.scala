package myscalc.calc.operator

import myscalc.calc._
import myscalc.calc.operatorbase._

sealed trait Operator extends Base {
	override def isContinue = true
	def left: Base
	def right: Base
	/**
		[[Add]][[Sub]][[Mul]][[Div]]のresultをまとめたメソッド
		
		@param create コンストラクタを設定
		@param solve Numの演算子を設定
	*/
	protected def resultBase(
			l: Base, r: Base,
			create: (Base, Base) => Operator,
			solve: (Num, Num) => Base
		): Base = {
		(l, r) match {
			case (_: Operator, _: Operator) | (_: Operator, _: Num) // (1+2)+(3+4) | (1+2)+3
				=> create(l.result, r)
			case (_: Num, _: Operator) // 1+2+3
				=> create(l, r.result)
			case (nl: Num, nr: Num) // 1+2
				=> solve(nl, nr)
		}
	}
}
object Operator {
	def unapply(ope: Operator): Option[(Base, Base)] = Option((ope.left, ope.right))
}

case class Add(left: Base, right: Base) extends Operator with AddSubOperator {
	override def result: Base = resultBase(left, right, Add(_, _), _ + _)
	override def string: String = stringBase(left, "+", right)
}

case class Sub(left: Base, right: Base) extends Operator with AddSubOperator {
	override def result: Base = resultBase(left, right, Sub(_, _), _ - _)
	override def string: String = stringBase(left, "-", right)
}

case class Mul(left: Base, right: Base) extends Operator with MulDivOperator {
	override def result: Base = resultBase(left, right, Mul(_, _), _ * _)
	override def string: String = stringBase(left, "*", right)
}

case class Div(left: Base, right: Base) extends Operator with MulDivOperator {
	override def result: Base = resultBase(left, right, Div(_, _), _ / _)
	override def string: String = stringBase(left, "/", right)
}
