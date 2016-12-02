package myscalc.calc

import scala.{Int => ScalaInt}
import myscalc.calc.operatorbase.MulDivOperator
import myscalc.calc.operator._

sealed trait Num extends Base {
	def + (pair: Num): Base
	def - (pair: Num): Base
	def * (pair: Num): Base
	def / (pair: Num): Base
}
object Num {
	def unapply(n: Num) = Option(())
}

package num {
	/**resultできない[[Num]]*/
	sealed trait SimpleNum extends Num {
		override def isContinue = false
		override def result: Num = throw new RuntimeException("isContinueがfalseなのでresultは実行されてはいけない")
	}
	
	case class Int(value: ScalaInt) extends SimpleNum {
		override def + (pair: Num): Base = pair match {
			case Int(n) => Int(value + n)
			case _: Inf => Inf()
			case _: Rational => pair + this
		}
		override def - (pair: Num): Base = pair match {
			case Int(n) => Int(value - n)
			case _: Inf => Inf()
			case Rational(pn, pd) => Div(Sub(pn, Mul(pd, this)), pd)
		}
		override def * (pair: Num): Base = pair match {
			case Int(n) => Int(value * n)
			case _: Inf => Inf()
			case _: Rational => pair * this
		}
		override def / (pair: Num): Base = pair match {
			case ipair @ Int(pairValue) => {
				if(pairValue == 0) {return Inf()}
				if(value % pairValue != 0) {return Rational(this, ipair)}
				Int(value / pairValue)
			}
			case _: Inf => Inf()
			case Rational(pn, pd) => Div(Mul(this, pd), pn)
		}
		override def string: String = value.toString
		/**
			最大公約数
			{{{a gcd b}}}
		*/
		private[num] def gcd(pair: Int): Int = {
			def gcdi(a: ScalaInt, b: ScalaInt): ScalaInt = {
				if(b==0) a
				else gcdi(b, a % b)
			}
			Int(gcdi(value, pair.value).abs)
		}
		private[num] def minimumCommonDivisorExcept1(pair: Int): Int = {
			def mcde1(i: ScalaInt, a: ScalaInt, b: ScalaInt): ScalaInt = {
				if(a % i == 0 && b % i == 0) i
				else mcde1(i + 1, a, b)
			}
			Int(mcde1(2, value, pair.value))
		}
		/**[[Rational]]の方で、公約数で割る処理のため*/
		private[num] def intdiv(pair: Int): Int = Int(value / pair.value)
		private[num] def isMinus: Boolean = value < 0
		private[num] def uminus: Int = Int(0 - value)
	}
	
	case class Inf() extends SimpleNum {
		override def + (pair: Num): Num = Inf()
		override def - (pair: Num): Num = Inf()
		override def * (pair: Num): Num = Inf()
		override def / (pair: Num): Num = Inf()
		override def string: String = "Inf"
	}
	
	case class Rational(numerator: Int, denominator: Int) extends Num with MulDivOperator {
		override def isContinue: Boolean = {
			canReduce | denominator.isMinus
		}
		override def result: Rational = {
			if (denominator.isMinus) {
				Rational(numerator.uminus, denominator.uminus)
			} else if(canReduce) {
				val mcde1, minimumCommonDivisorExcept1 = numerator minimumCommonDivisorExcept1 denominator
				Rational(numerator.intdiv(mcde1), denominator.intdiv(mcde1))
			} else throw new RuntimeException("resultを呼ぶ条件が揃っていないはず")
		}
		override def + (pair: Num): Base = pair match {
			case _: Int => Div(Add(numerator, Mul(denominator, pair)), denominator)
			case Rational(pn, `denominator`) => Div(Add(numerator, pn), denominator)
			case Rational(pn, pd) => Div(Add(Mul(numerator, pd), Mul(pn, denominator)), Mul(denominator, pd))
			case _: Inf => Inf()
		}
		override def - (pair: Num): Base = pair match {
			case _: Int => Div(Sub(numerator, Mul(denominator, pair)), denominator)
			case Rational(pn, `denominator`) => Div(Sub(numerator, pn), denominator)
			case Rational(pn, pd) => Div(Sub(Mul(numerator, pd), Mul(pn, denominator)), Mul(denominator, pd))
			case _: Inf => Inf()
		}
		override def * (pair: Num): Base = pair match {
			case _: Int => Div(Mul(numerator, pair), denominator)
			case Rational(pn, pd) => Div(Mul(numerator, pn), Mul(denominator, pd))
			case _: Inf => Inf()
		}
		override def / (pair: Num): Base = pair match {
			case _: Int => Div(numerator, Mul(denominator, pair))
			case Rational(pn, pd) => Div(Mul(numerator, pd), Mul(denominator, pn))
			case _: Inf => Inf()
		}
		override def string: String = s"${numerator.string}/${denominator.string}"
		private def canReduce: Boolean = (numerator gcd denominator) != Int(1)
	}
}
