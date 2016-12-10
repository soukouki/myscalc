package myscalc.calc

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
	/**[[#advance]]できない[[Num]]*/
	sealed trait SimpleNum extends Num {
		override def hasFinished = false
		override def advance: Num = sys.error("hasFinishedがfalseなのでadvanceは実行されてはいけない")
	}
	
	case class Int(value: BigInt) extends SimpleNum {
		override def + (pair: Num): Base = addSubBase(pair, _ + _, Add(_, _))
		override def - (pair: Num): Base = addSubBase(pair, _ - _, Sub(_, _))
		override def * (pair: Num): Base = pair match {
			case Int(n) => Int(value * n)
			case _: Inf => Inf()
			case _: Rational => pair * this
		}
		/**
			両方がInt型のときの処理は[[Rational]]でやる
		*/
		override def / (pair: Num): Base = pair match {
			case i: Int => Rational(this, i)
			case _: Inf => Inf()
			case Rational(pn, pd) => Div(Mul(this, pd), pn)
		}
		override def string: String = value.toString
		private[num] def gcd(pair: Int): Int = Int(value gcd pair.value)
		private[num] def minimumCommonDivisorExcept1(pair: Int): Int = {
			def mcde1(i: BigInt, a: BigInt, b: BigInt): BigInt = {
				if(a % i == 0 && b % i == 0) i
				else mcde1(i + 1, a, b)
			}
			Int(mcde1(2, value, pair.value))
		}
		/**[[Rational]]の方で、公約数で割る処理のため*/
		private[num] def intdiv(pair: Int): Int = Int(value / pair.value)
		private[num] def isMinus: Boolean = value < 0
		private[num] def uminus: Int = Int(0 - value)
		
		private def addSubBase(pair: Num, intadvance: (BigInt, BigInt) => BigInt, create: (Base, Base) => Base): Base = {
			pair match {
				case Int(n) => Int(intadvance(value, n))
				case _: Inf => Inf()
				case Rational(pn, pd) => Div(create(pn, Mul(pd, this)), pd)
			}
		}
	}
	
	case class Inf() extends SimpleNum {
		override def + (pair: Num): Num = Inf()
		override def - (pair: Num): Num = Inf()
		override def * (pair: Num): Num = Inf()
		override def / (pair: Num): Num = Inf()
		override def string: String = "Inf"
	}
	
	/**
		[[Int#/]]と違い、両方の数がInt型のときの処理をする
	*/
	case class Rational(numerator: Int, denominator: Int) extends Num with MulDivOperator {
		override def hasFinished: Boolean = {
			denominator == Int(1) | denominator == Int(0) | denominator.isMinus | canReduce
		}
		override def advance: Num = {
			if(denominator == Int(1)) {
				numerator
			} else if(denominator == Int(0)) {
				Inf()
			} else if (denominator.isMinus) {
				Rational(numerator.uminus, denominator.uminus)
			} else if(canReduce) {
				val mcde1, minimumCommonDivisorExcept1 = numerator minimumCommonDivisorExcept1 denominator
				Rational(numerator.intdiv(mcde1), denominator.intdiv(mcde1))
			} else sys.error("hasFinishedがfalseなのでadvanceは実行されてはいけない")
		}
		override def + (pair: Num): Base = addSubBase(pair, Add(_, _))
		override def - (pair: Num): Base = addSubBase(pair, Sub(_, _))
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
		override def string: String = stringBase(numerator, "/", denominator)
		
		private def canReduce: Boolean = (numerator gcd denominator) != Int(1)
		private def addSubBase(pair: Num, create: (Base, Base) => Base): Base = pair match {
			case _: Int => Div(create(numerator, Mul(denominator, pair)), denominator)
			case Rational(pn, `denominator`) => Div(create(numerator, pn), denominator)
			case Rational(pn, pd) => Div(create(Mul(numerator, pd), Mul(pn, denominator)), Mul(denominator, pd))
			case _: Inf => Inf()
		}
	}
}
