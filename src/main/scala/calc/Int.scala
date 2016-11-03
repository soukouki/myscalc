package myscalc.calc

import scala.{Int => ScalaInt}

case class Int(value: ScalaInt) extends Num {
	override def + (pair: Num): Num = pair match {
		case Int(n) => Int(value + n)
		case _ => throw new RuntimeException(pair.getClass + "には対応していません")
	}
	override def - (pair: Num): Num = pair match {
		case Int(n) => Int(value - n)
		case _ => throw new RuntimeException(pair.getClass + "には対応していません")
	}
	override def * (pair: Num): Num = pair match {
		case Int(n) => Int(value * n)
		case _ => throw new RuntimeException(pair.getClass + "には対応していません")
	}
	override def / (pair: Num): Num = {
		pair match {
			case Int(n) => {
				if(n == 0) {
					return Inf()
				}
				Int(value / n)
			}
			case _ => throw new RuntimeException(pair.getClass + "には対応していません")
		}
	}
	override def string: String = value.toString
}
