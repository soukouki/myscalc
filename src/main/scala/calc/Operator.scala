package myscalc.calc

abstract sealed class Operator extends Base {
	override def isContinue = true
	def left: Base
	def right: Base
	/**
		[[Add]][[Sub]][[Mul]][[Div]]のresultをまとめたメソッド
		
		@param create コンストラクタを設定
		@param solve Numの演算子を設定
	*/
	protected def resultBase(
			create: (Base, Base) => Operator,
			solve: (Num, Num) => Num
		): Base = {
		(left, right) match {
			case (Operator(_, _), Operator(_, _)) | (Operator(_, _), Num()) // (1+2)+(3+4)
				=> create(left.result, right)
			case (Num(), Operator(_, _)) // 1+2+3
				=> create(left, right.result)
			case (l @ Num(), r @ Num()) // 1+2
				=> solve(l, r)
		}
	}
}
object Operator {
	def unapply(ope: Operator): Option[(Base, Base)] = Option((ope.left, ope.right))
}

sealed trait AddSubOperator extends Operator
object AddSubOperator {
	def unapply(ope: AddSubOperator): Option[(Base, Base)] = Option((ope.left, ope.right))
}

case class Add(left: Base, right: Base) extends AddSubOperator {
	override def result: Base = resultBase(Add(_, _), _ + _)
	override def string: String = (left, right) match {
		case (MulDivOperator(_, _), _) => s"(${left.string})+${right.string}"
		case _ => left.string + "+" + right.string
	}
}

case class Sub(left: Base, right: Base) extends AddSubOperator {
	override def result: Base = resultBase(Sub(_, _), _ - _)
	override def string: String = left.string + "-" + right.string
}

sealed trait MulDivOperator extends Operator
object MulDivOperator {
	def unapply(ope: MulDivOperator): Option[(Base, Base)] = Option((ope.left, ope.right))
}

case class Mul(left: Base, right: Base) extends MulDivOperator {
	override def result: Base = resultBase(Mul(_, _), _ * _)
	override def string: String = left.string + "*" + right.string
}

case class Div(left: Base, right: Base) extends MulDivOperator {
	override def result: Base = resultBase(Div(_, _), _ / _)
	override def string: String = left.string + "/" + right.string
}
