package myscalc.calc.operator

import myscalc.variables.Variables
import myscalc.calc._
import myscalc.calc.operatorbase._
import myscalc.calc.num._

sealed trait Operator extends Base {
	override def hasFinished(va: Variables) = false
}

// TODO DecimalやRecurringDecimalのcaseがいくつもあるのでPartialFunctionを使ってもう少し簡潔にする

sealed trait DoubleOperator extends Operator {
	def left: Base
	def right: Base
	def solve(l: Num,r: Num): Base
	/**
		[[Add]][[Sub]][[Mul]][[Div]]のadvanceをまとめたメソッド
		[[#solve]]で計算をするので、実装する
		
		@param create コンストラクタを設定
	*/
	protected def advanceBase(
			l: Base, r: Base, va: Variables,
			create: (Base, Base) => Operator
		): (Base, Variables) = {
		(l.hasFinished(va), r.hasFinished(va)) match {
			case (false, false) | (false, true) // (1+2)+(3+4) | (1+2)+3
				=> {
					val (nl, _) = l.advance(va)
					(create(nl, r), va)
				}
			case (true, false) // 1+(2+3)
				=> {
					val (nr, _) = r.advance(va)
					(create(l, nr), va)
				}
			case (true, true) // 1+2
				=> (l, r) match {
					case (nl: Num, nr: Num) => (solve(nl, nr), va)
					case (Undef(), _) | (_, Undef()) => (Undef(), va)
					case (Variable(_), _) | (_, Variable(_)) => (Undef(), va) // 変数が設定されていなかったときはここを通る
				}
		}
	}
}

/** [[Add]][[Sub]]の主に[[#solve]]をまとめたクラス */
sealed trait AddSub extends DoubleOperator with AddSubOperator {
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
		case (l: RecurringDecimal, r: RecurringDecimal) => create(l.toFormula, r.toFormula)
		case (l: RecurringDecimal, r) => create(l.toFormula, r)
		case (l, r: RecurringDecimal) => create(l, r.toFormula)
	}
	private def decimalAlignment(l: Decimal, r: Decimal): (Int, Int, Int) = {
			 if (l.ex < r.ex) (l.si, r.si * (r.ex - l.ex).pow10, l.ex)
		else if (l.ex > r.ex) (l.si * (l.ex - r.ex).pow10, r.si, r.ex)
		else (l.si, r.si, l.ex) // exは負数で、値が小さいほど小数の桁が多い
	}
}

case class Add(left: Base, right: Base) extends AddSub {
	override def advance(va: Variables): (Base, Variables) = advanceBase(left, right, va, Add(_, _))
	override def string: String = stringBase(left, "+", right)
	override def solve(l: Num, r: Num) = addSolve(l, r, Add(_, _), _ + _)
}

case class Sub(left: Base, right: Base) extends AddSub {
	override def advance(va: Variables): (Base, Variables) = advanceBase(left, right, va, Sub(_, _))
	override def string: String = stringBase(left, "-", right)
	override def solve(left: Num, right: Num) = (left, right) match {
		// 小数部分の桁が同じで循環部分も同じ場合は、減法の場合だけ直接計算できるため。
		case (RecurringDecimal(ld, lr), RecurringDecimal(rd, rr)) if lr == rr && ld.ex == rd.ex => Sub(ld, rd)
		case (l, r) => {addSolve(l, r, Sub(_, _), _ - _)}
	}
}

case class Mul(left: Base, right: Base) extends DoubleOperator with MulDivOperator {
	override def advance(va: Variables): (Base, Variables) = advanceBase(left, right, va, Mul(_, _))
	override def string: String = stringBase(left, "*", right)
	override def solve(l: Num, r: Num) = (left, right) match {
		case (Inf(), _) | (_, Inf()) => Inf()
		case (_, Int(n)) if n == 0 => Int(0)
		case (l, Int(n)) if n == 1 => l
		case (Int(n), _) if n == 0 => Int(0)
		case (Int(n), r) if n == 1 => r
		case (l: Int, r: Int) => l * r
		case (Rational(ln, ld), p: Int) => Div(Mul(ln, p), ld)
		case (p: Int, Rational(rn, rd)) => Div(Mul(p, rn), rd)
		case (Rational(ln, ld), Rational(rn, rd)) => Div(Mul(ln, rn), Mul(ld, rd))
		case (Decimal(si, ex), r: Int) => Decimal(si * r, ex)
		case (l: Int, Decimal(si, ex)) => Decimal(si * l, ex)
		case (l: Decimal, r: Decimal) => Mul(l.toRational, r.toRational)
		case (l: Decimal, r) => Mul(l.toRational, r)
		case (l, r: Decimal) => Mul(l, r.toRational)
		case (l: RecurringDecimal, r: RecurringDecimal) => Mul(l.toFormula, r.toFormula)
		case (l: RecurringDecimal, r) => Mul(l.toFormula, r)
		case (l, r: RecurringDecimal) => Mul(l, r.toFormula)
	}
}

case class Div(left: Base, right: Base) extends DoubleOperator with MulDivOperator {
	override def advance(va: Variables): (Base, Variables) = advanceBase(left, right, va, Div(_, _))
	override def string: String = stringBase(left, "/", right)
	override def solve(l: Num, r: Num) = {
		val zero: BigInt = 0 // 変数に一回入れないとできないっぽい
		(left, right) match {
			case (Inf(), _) | (_, Inf()) | (_, Int(`zero`)) => Inf()
			case (l, r) if l == r => Int(1)
			case (l, Int(n)) if n == 1 => l
			case (l: Int, r: Int) if l divisible r => l / r
			case (l: Int, r: Int) => Rational(l, r) // 割り切れないとき
			case (Rational(ln, ld), r: Int) => Div(ln, Mul(ld, r))
			case (l: Int, Rational(rn, rd)) => Div(Mul(l, rd), rn)
			case (Rational(ln, ld), Rational(rn, rd)) => Div(Mul(ln, rd), Mul(ld, rn))
			case (l: Decimal, r: Decimal) => Div(l.toRational, r.toRational)
			case (l: Decimal, r) => Div(l.toRational, r)
			case (l, r: Decimal) => Div(l, r.toRational)
			case (l: RecurringDecimal, r: RecurringDecimal) => Div(l.toFormula, r.toFormula)
			case (l: RecurringDecimal, r) => Div(l.toFormula, r)
			case (l, r: RecurringDecimal) => Div(l, r.toFormula)
		}
	}
}

case class Minus(value: Base) extends Operator {
	override def advance(va: Variables): (Base, Variables) =
		value.hasFinished(va) match {
			case false => {
				val (new_value, _) = value.advance(va)
				(Minus(new_value), va)
			}
			case true => {
				(value match {
					case Undef() => Undef()
					case Inf() => Inf()
					case Variable(_) => Undef()
					case Int(i) => Int(-i)
					case Rational(n, d) => Rational(-n, d)
					case Decimal(si, ex) => Decimal(-si, ex)
					case RecurringDecimal(Decimal(si, ex), rc) => RecurringDecimal(Decimal(-si, ex), rc)
				}, va)
			}
		}
	override def string: String = s"-(${value.string})"
}
