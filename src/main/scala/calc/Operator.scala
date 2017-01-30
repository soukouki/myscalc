package myscalc.calc.operator

import myscalc.calc._
import myscalc.calc.operatorbase._
import myscalc.calc.num._

sealed trait Operator extends Base {
	override def hasFinished = false
	def left: Base
	def right: Base
	def solve(l: Num,r: Num): Base
	/**
		[[Add]][[Sub]][[Mul]][[Div]]のadvanceをまとめたメソッド
		[[#solve]]で計算をするので、実装する
		
		@param create コンストラクタを設定
	*/
	protected def advanceBase(
			l: Base, r: Base,
			create: (Base, Base) => Operator
		): Base = {
		(l.hasFinished, r.hasFinished) match {
			case (false, false) | (false, true) // (1+2)+(3+4) | (1+2)+3
				=> create(l.advance, r)
			case (true, false) // 1+2+3
				=> create(l, r.advance)
			case (true, true) // 1+2
				=> (l, r) match {
					case (nl: Num, nr: Num) // trueなのはNumだけ(コンパイルエラー出せないのでwarningのまま)
						=> solve(nl, nr)
				}
		}
	}
}
object Operator {
	def unapply(ope: Operator): Option[(Base, Base)] = Option((ope.left, ope.right))
}

/** [[Add]][[Sub]]の主に[[#solve]]をまとめたクラス */
sealed trait AddSub extends Operator with AddSubOperator {
	/**
		[[Add]]と[[Sub]]の[[#solve]]をまとめたもの
		
		@param create コンストラクタを設定
	*/
	def addSolve(
			left: Num, right: Num,
			create: (Base, Base) => AddSub,
			int_solve: (Int, Int) => Int
		): Base = (left, right) match {
		case (Inf(), _) | (_, Inf()) => Inf()
		case (l: Int, r: Int) => int_solve(l, r)
		case (l: Int, Rational(rn, rd)) => Div(create(Mul(rd, l), rn), rd)
		case (Rational(ln, ld), r: Int) => Div(create(ln, Mul(ld, r)), ld)
		case (Rational(ln, d), Rational(rn, rd)) if d == rd => Div(create(ln, rn), d) // 同分母
		case (Rational(ln, ld), Rational(rn, rd)) => Div(create(Mul(ln, rd), Mul(rn, ld)), Mul(ld, rd)) // 異分母
		case (l: Int, Decimal(si, ex)) => Decimal(int_solve(l*((-ex).pow10), si), ex)
		case (Decimal(si, ex), r: Int) => Decimal(int_solve(si, r*((-ex).pow10)), ex)
		case (l: Rational, r: Decimal) => create(l, r.toRational)
		case (l: Decimal, r: Rational) => create(l.toRational, r)
		case (l: Decimal, r: Decimal) => {
			val (lsi, rsi, ex) = decimalAlignment(l, r)
			Decimal(int_solve(lsi, rsi), ex)
		}
	}
	private def decimalAlignment(l: Decimal, r: Decimal): (Int, Int, Int) = {
			 if (l.ex < r.ex) (l.si, r.si * (r.ex - l.ex).pow10, l.ex)
		else if (l.ex > r.ex) (l.si * (l.ex - r.ex).pow10, r.si, r.ex)
		else (l.si, r.si, l.ex) // exは負数で、値が小さいほど小数の桁が多い
	}
}

case class Add(left: Base, right: Base) extends AddSub {
	override def advance: Base = advanceBase(left, right, Add(_, _))
	override def string: String = stringBase(left, "+", right)
	override def solve(l: Num, r: Num) = addSolve(l, r, Add(_, _), _ + _)
}

case class Sub(left: Base, right: Base) extends AddSub {
	override def advance: Base = advanceBase(left, right, Sub(_, _))
	override def string: String = stringBase(left, "-", right)
	override def solve(l: Num, r: Num) = addSolve(l, r, Sub(_, _), _ - _)
}

case class Mul(left: Base, right: Base) extends Operator with MulDivOperator {
	override def advance: Base = advanceBase(left, right, Mul(_, _))
	override def string: String = stringBase(left, "*", right)
	override def solve(l: Num, r: Num) = (left, right) match {
		case (Inf(), _) | (_, Inf()) => Inf()
		case (l: Int, r: Int) => l * r
		case (Rational(ln, ld), p: Int) => Div(Mul(ln, p), ld)
		case (p: Int, Rational(rn, rd)) => Div(Mul(p, rn), rd)
		case (Rational(ln, ld), Rational(rn, rd)) => Div(Mul(ln, rn), Mul(ld, rd))
		case (l: Decimal, r: Decimal) => Mul(l.toRational, r.toRational)
		case (l: Decimal, r) => Mul(l.toRational, r)
		case (l, r: Decimal) => Mul(l, r.toRational)
	}
}

case class Div(left: Base, right: Base) extends Operator with MulDivOperator {
	override def advance: Base = advanceBase(left, right, Div(_, _))
	override def string: String = stringBase(left, "/", right)
	override def solve(l: Num, r: Num) = {
		val zero = Int(0) // 変数に一回入れないとコンパイルが通らなかった
		(left, right) match {
			case (Inf(), _) | (_, Inf()) | (_, `zero`) => Inf()
			case (l: Int, r: Int) if l divisible r => l / r
			case (l: Int, r: Int) => Rational(l, r) // 割り切れないとき
			case (Rational(ln, ld), r: Int) => Div(ln, Mul(ld, r))
			case (l: Int, Rational(rn, rd)) => Div(Mul(l, rd), rn)
			case (Rational(ln, ld), Rational(rn, rd)) => Div(Mul(ln, rd), Mul(ld, rn))
			case (l: Decimal, r: Decimal) => Div(l.toRational, r.toRational)
			case (l: Decimal, r) => Div(l.toRational, r)
			case (l, r: Decimal) => Div(l, r.toRational)
		}
	}
}
