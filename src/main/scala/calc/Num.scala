package myscalc.calc

import myscalc.variables.Variables

import scala.{Int => ScalaInt}
import myscalc.calc.operatorbase.MulDivOperator

sealed trait Num extends Base
object Num {
	def unapply(n: Num) = Option(())
}

package num {
	/**[[#advance]]できない[[Num]]*/
	sealed trait SimpleNum extends Num {
		override def hasFinished(va: Variables) = true
		override def advance(va: Variables) = sys.error("hasFinishedがfalseなのでadvanceは実行されてはいけない")
	}
	
	case class Int(value: BigInt) extends SimpleNum with Ordered[Int] {
		override def string: String = value.toString
		override def compare(p: Int) = value.compare(p.value)
		
		// [[Operator]]で計算に使う
		private[calc] def +(pair: Int) = Int(value + pair.value)
		private[calc] def -(pair: Int) = Int(value - pair.value)
		private[calc] def *(pair: Int) = Int(value * pair.value)
		private[calc] def /(pair: Int) = Int(value / pair.value)
		private[calc] def %(pair: Int) = Int(value % pair.value)
	 	private[calc] def unary_- = Int(-value)
		private[calc] def divisible(pair: Int) = (value % pair.value) == 0
		private[calc] def pow10: Int = Int(BigInt(10) pow toInt)
		private[calc] def digits = Int(string.length)
		
		private[num] def gcd(pair: Int): Int = Int(value gcd pair.value)
		private[num] def minimumCommonDivisorExcept1(pair: Int): Int = {
			def mcde1(i: BigInt, a: BigInt, b: BigInt): BigInt = {
				if(a % i == 0 && b % i == 0) i
				else mcde1(i + 1, a, b)
			}
			Int(mcde1(2, value, pair.value))
		}
		private[num] def isMinus: Boolean = value < 0
		private[num] def toUnSign: Int = if(isMinus) -this else this
		private[num] def toInt: ScalaInt = {
			val int = value.toInt
			if(int!=value) sys.error("overflow error")
			int
		}
	}
	
	case class Inf() extends SimpleNum {
		override def string: String = "Inf"
	}
	
	/** [[operator.Div]]と違い、両方の数がInt型のときの処理をする */
	case class Rational(numerator: Int, denominator: Int) extends Num with MulDivOperator {
		override def hasFinished(va: Variables): Boolean =
			denominator != Int(1) && denominator != Int(0) && !denominator.isMinus && !canReduce
		override def advance(va: Variables): (Num, Variables) = {
			if(denominator == Int(1)) {
				(numerator, va)
			} else if(denominator == Int(0)) {
				(Inf(), va)
			} else if (denominator.isMinus) {
				(Rational(-numerator, -denominator), va)
			} else if(canReduce) {
				val mcde1, minimumCommonDivisorExcept1 = numerator minimumCommonDivisorExcept1 denominator
				(Rational(numerator / mcde1, denominator / mcde1), va)
			} else sys.error("hasFinishedがfalseなのでadvanceは実行されてはいけない")
		}
		override def string: String = stringBase(numerator, "/", denominator)
		
		private def canReduce: Boolean = (numerator gcd denominator) != Int(1)
	}
	
	/**
		[[ex]]が0以上の場合は考慮しない<br>
		[[ex]]は内部でInt型になることがある
		
		DecimalSpecでexの動作を確認
		
		@param si significand
		@param ex exponentiation
	*/
	case class Decimal(si: Int, ex: Int) extends SimpleNum {
		if(ex.value > 0) sys.error("exが0超えの場合は考慮しない")
		ex.toInt //Int型の範囲を超えていないかチェックする
		
		def string: String = string(false)
		private[num] def string(appendDotAlways: Boolean): String = {
			if(!appendDotAlways && ex==Int(0)) return si.string
			val minusRevise = if(si.isMinus) 1 else 0
			val nonDotStr = "0" * -(ex.toInt - 1 + si.string.length - minusRevise) + si.toUnSign.string
			val (l, r) = nonDotStr.splitAt(nonDotStr.length + ex.toInt)
			(if(si.isMinus) "-" else "") + l + "." + r
		}
		
		private[calc] def toRational = Rational(si, (-ex).pow10)
	}
	
	case class RecurringDecimal(decimal: Decimal, recurring: Int) extends Num {
		if(recurring.isMinus) sys.error("recurringが負の数になっている")
		
		override def advance(va: Variables): (Base, Variables) = (decimal, va)
		override def hasFinished(va: Variables) = recurring != Int(0)
		override def string: String = s"${decimal.string(appendDotAlways = true)}(${recurring.string})"
		
		private[calc] def toFormula: Base = {
			import operator._ // TODO(v2までに) この行を消す
			val rdp = recurring.digits.pow10
			// 計算で求めるとsiがマイナスの場合とかが面倒なので文字列で計算してる。
			val newDesi = Int(BigInt(decimal.si.string + recurring.string))
			Div(Sub(RecurringDecimal(Decimal(newDesi, decimal.ex), recurring), this), rdp - Int(1))
		}
	}
}
