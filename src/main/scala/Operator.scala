package myscalc.calc

abstract sealed class Operator(left: Base, right: Base) extends Base {
	override def isNum: Boolean = false
}

case class Add(left: Base, right: Base) extends Operator(left, right) {
	override def result: Base = (left.isNum, right.isNum) match {
		case (false, false) | (false, true) // (1+2)+(3+4)
			=> Add(left.result, right)
		case (true, false) // 1+2+3
			=> Add(left, right.result)
		case (true, true) // 1+2
			=> left.asInstanceOf[Num] + right.asInstanceOf[Num]
	}
}

case class Sub(left: Num, right: Num) extends Operator(left, right) {
	override def result: Num = {
		left - right
	}
}

case class Mul(left: Num, right: Num) extends Operator(left, right) {
	override def result: Num = {
		left * right
	}
}

case class Div(left: Num, right: Num) extends Operator(left, right) {
	override def result: Num = {
		left / right
	}
}
