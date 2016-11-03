package myscalc.calc

abstract sealed class Operator(left: Base, right: Base) extends Base {
	override def isNum = false
	override def isContinue = true
	/**
		[[Add]][[Sub]][[Mul]][[Div]]のresultをまとめたメソッド
		
		@param create コンストラクタを設定
		@param solve Numの演算子を設定
	*/
	protected def resultBase(
			create: (Base, Base) => Operator,
			solve: (Num, Num) => Num
		): Base = {
		(left.isNum, right.isNum) match {
			case (false, false) | (false, true) // (1+2)+(3+4)
				=> create(left.result, right)
			case (true, false) // 1+2+3
				=> create(left, right.result)
			case (true, true) // 1+2
				=> solve(left.asInstanceOf[Num], right.asInstanceOf[Num])
		}
	}
}

case class Add(left: Base, right: Base) extends Operator(left, right) {
	override def result: Base = resultBase(Add(_, _), _ + _)
	override def string: String = left.string + "+" + right.string
}

case class Sub(left: Base, right: Base) extends Operator(left, right) {
	override def result: Base = resultBase(Sub(_, _), _ - _)
	override def string: String = left.string + "-" + right.string
}

case class Mul(left: Base, right: Base) extends Operator(left, right) {
	override def result: Base = resultBase(Mul(_, _), _ * _)
	override def string: String = left.string + "*" + right.string
}

case class Div(left: Base, right: Base) extends Operator(left, right) {
	override def result: Base = resultBase(Div(_, _), _ / _)
	override def string: String = left.string + "/" + right.string
}
