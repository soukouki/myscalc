package myscalc.calc

import scala.{Int => ScalaInt}

trait Num extends Base {
	override def isContinue = false
	override def result: Num = this
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
			case Inf() => Inf()
		}
		override def - (pair: Num): Num = pair match {
			case Int(n) => Int(value - n)
			case Inf() => Inf()
		}
		override def * (pair: Num): Num = pair match {
			case Int(n) => Int(value * n)
			case Inf() => Inf()
		}
		override def / (pair: Num): Num = pair match {
			case Int(n) => {
				if(n == 0) {return Inf()}
				Int(value / n)
			}
			case Inf() => Inf()
		}
		override def string: String = value.toString
	}
	
	case class Inf() extends Num {
		override def + (pair: Num) = Inf()
		override def - (pair: Num) = Inf()
		override def * (pair: Num) = Inf()
		override def / (pair: Num) = Inf()
		override def string: String = "Inf"
	}
}
