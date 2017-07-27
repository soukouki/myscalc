
import myscalc.variables.Variables

import myscalc.calc.num._
import myscalc.calc.operator._

class IntSpec extends FlatCalcTest

class InfSpec extends FlatCalcTest {
	"parse" should "Infリテラル" in {
		assert(p("Inf") === Inf())
	}
}

class RationalSpec extends FlatCalcTest {
	"advance" should "約分できるか" in {
		assert(advance(Rational(Int(4), Int(6))) === Rational(Int(2), Int(3)))
		assert(advance(Rational(Int(-2), Int(4))) === Rational(Int(-1), Int(2)))
	}
	it should "整数への約分" in {
		assert(advance(Rational(Int(2), Int(1))) === Int(2))
	}
	it should "約分は2以上の最小公約数ずつ" in {
		assert(advance(Rational(Int(18), Int(24))) === Rational(Int(9), Int(12)))
		assert(advance(Rational(Int(9), Int(12))) === Rational(Int(3), Int(4)))
		assert(advance(Rational(Int(-4), Int(4))) === Rational(Int(-2), Int(2)))
	}
	it should "マイナスの扱い" in {
		assert(advance(Rational(Int(1), Int(-2))) === Rational(Int(-1), Int(2)))
		assert(advance(Rational(Int(-1), Int(-2))) === Rational(Int(1), Int(2)))
	}
	it should "約分よりマイナスの扱いを先にする" in {
		assert(advance(Rational(Int(2), Int(-4))) === Rational(Int(-2), Int(4)))
	}
	"hasFinished" should "約分ができるとき" in {
		assert(Rational(Int(2), Int(3)).hasFinished(defaultVariables) === true)
		assert(Rational(Int(4), Int(6)).hasFinished(defaultVariables) === false)
	}
	it should "マイナスの扱い" in {
		assert(Rational(Int(1), Int(2)).hasFinished(defaultVariables) === true)
		assert(Rational(Int(-1), Int(2)).hasFinished(defaultVariables) === true)
		assert(Rational(Int(1), Int(-2)).hasFinished(defaultVariables) === false)
		assert(Rational(Int(-1), Int(-2)).hasFinished(defaultVariables) === false)
	}
	"string" should "Divと同じように" in {
		assert(Rational(Int(1), Int(2)).string === "1/2")
	}
	it should "他のOperatorからも正しくできるか" in {
		assert(Div(Int(1), Rational(Int(2), Int(3))).string === "1/(2/3)")
	}
}

class DecimalSpec extends FlatCalcTest {
	"hasFinished" should "常にtrue" in {
		assert(Decimal(Int(12), Int(-1)).hasFinished(defaultVariables) === true)
	}
	"string" should "" in {
		assert(Decimal(Int(123), Int(-2)).string === "1.23")
		assert(Decimal(Int(123), Int(-1)).string === "12.3")
		assert(Decimal(Int(-12), Int(-1)).string === "-1.2")
	}
	it should "キリの良い小数" in {
		assert(Decimal(Int(10), Int(-1)).string === "1.0")
		assert(Decimal(Int(0), Int(-1)).string === "0.0")
		assert(Decimal(Int(-20), Int(-1)).string === "-2.0")
	}
	it should "1以下の小数(0も除く)" in {
		assert(Decimal(Int(2), Int(-2)).string === "0.02")
		assert(Decimal(Int(-2), Int(-1)).string === "-0.2")
	}
	it should "exが0が0のとき(RecurringDecimalで使用するときに出る)" in {
		assert(Decimal(Int(2), Int(0)).string === "2")
	}
}

class RecurringDecimalSpec extends FlatCalcTest {
	"advance" should "0が続く場合" in {
		assert(advance(RecurringDecimal(Decimal(Int(1), Int(0)), Int(0))) === Decimal(Int(1),Int(0)))
		assert(advance(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(0))) === Decimal(Int(12), Int(-1)))
	}
	"hasFinished" should "通常" in {
		assert(RecurringDecimal(Decimal(Int(12), Int(-3)), Int(45)).hasFinished(defaultVariables) === true)
	}
	it should "0が続く場合" in {
		assert(RecurringDecimal(Decimal(Int(1), Int(0)), Int(0)).hasFinished(defaultVariables) === false)
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(0)).hasFinished(defaultVariables) === false)
	}
	"string" should "" in {
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(3)).string === "1.2(3)")
		assert(RecurringDecimal(Decimal(Int(3), Int(0)), Int(3)).string === "3.(3)")
	}
}
