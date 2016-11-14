package myscalc.calc

import scala.{Int => ScalaInt}

sealed trait Num extends Base {
	override def isContinue = false
	override def result: Num = ???
	def + (pair: Num): Num
	def - (pair: Num): Num
	def * (pair: Num): Num
	def / (pair: Num): Num
}
object Num {
	def unapply(n: Num) = Option(())
}

package num {
	case class Int(value: ScalaInt) extends Num {
		override def + (pair: Num): Num = pair match {
			case Int(n) => Int(value + n)
			case _: Inf => Inf()
		}
		override def - (pair: Num): Num = pair match {
			case Int(n) => Int(value - n)
			case _: Inf => Inf()
		}
		override def * (pair: Num): Num = pair match {
			case Int(n) => Int(value * n)
			case _: Inf => Inf()
		}
		override def / (pair: Num): Num = pair match {
			case ipair @ Int(pairValue) => {
				if(pairValue == 0) {return Inf()}
				if(value % pairValue != 0) {return Rational(this, ipair)}
				Int(value / pairValue)
			}
			case _: Inf => Inf()
		}
		override def string: String = value.toString
	}
	
	case class Rational(numerator: Int, denominator: Int) extends Num {
		override def + (pair: Num): Num = ???
		override def - (pair: Num): Num = ???
		override def * (pair: Num): Num = ???
		override def / (pair: Num): Num = ???
		override def string: String = ???
	}
	
	case class Inf() extends Num {
		override def + (pair: Num): Num = Inf()
		override def - (pair: Num): Num = Inf()
		override def * (pair: Num): Num = Inf()
		override def / (pair: Num): Num = Inf()
		override def string: String = "Inf"
	}
}
