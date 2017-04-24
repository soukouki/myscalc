
import myscalc.variables.Variables

import myscalc.calc.num._
import myscalc.calc.operator._

class IntSpec extends FlatCalcTest {
	"+" should "" in {
		assert(advance(p("1+2")) === p("3"))
		assert(advance(p("1+-3")) === p("-2"))
	}
	"-" should "" in {
		assert(advance(p("2-1")) === p("1"))
		assert(advance(p("-2--3")) === p("1"))
	}
	"*" should "" in {
		assert(advance(p("2*3")) === p("6"))
		assert(advance(p("-2*-3")) === p("6"))
	}
	"/" should "" in {
		// Divをそのまま使っているのは#pだと分数になってしまうため
		assert(advance(Div(Int(4), Int(2))) === p("2"))
		assert(advance(Div(Int(6), Int(2))) === p("3"))
	}
	it should "割り切れないときは分数にして返す" in {
		// #pを使うとわかりにくい気がするのでそのまま
		assert(advance(Div(Int(3), Int(2))) === Rational(Int(3), Int(2)))
		assert(advance(Div(Int(3), Int(10))) === Rational(Int(3), Int(10)))
	}
}

class InfSpec extends FlatCalcTest {
	"parse" should "Infリテラル" in {
		assert(p("Inf") === Inf())
	}
	"+-*/" should "全てInfを返す" in {
		assert(advance(p("Inf+2")) === Inf())
		assert(advance(p("Inf-2")) === Inf())
		assert(advance(p("Inf*2")) === Inf())
		assert(advance(p("Inf/2")) === Inf())
		assert(advance(p("Inf/Inf")) === p("Inf"))
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
	"+" should "整数" in {
		assert(advance(p("1/2+3")) === p("(1+2*3)/2"))
		assert(advance(p("1+2/3")) === p("(3*1+2)/3"))
	}
	it should "同分母" in {
		assert(advance(p("1/3+2/3")) === p("(1+2)/3"))
	}
	it should "異分母" in {
		// 通分は[[Rational]]の仕様でできないので
		assert(advance(p("1/2+3/4")) === p("(1*4+3*2)/(2*4)"))
	}
	"-" should "整数" in {
		assert(advance(p("1/2-3")) === p("(1-2*3)/2"))
		assert(advance(p("1-2/3")) === p("(3*1-2)/3"))
	}
	it should "同分母" in {
		assert(advance(p("1/3-2/3")) === p("(1-2)/3"))
	}
	it should "異分母" in {
		assert(advance(p("1/2-3/4")) === p("(1*4-3*2)/(2*4)"))
	}
	"*" should "整数" in {
		assert(advance(p("1/2*3")) === p("1*3/2"))
		assert(advance(p("4*(2/3)")) === p("4*2/3"))
	}
	it should "分数" in {
		assert(advance(p("1/2*(3/4)")) === p("1*3/(2*4)"))
	}
	"/" should "整数" in {
		assert(advance(p("1/2/3")) === p("1/(2*3)"))
		assert(advance(p("1/(2/3)")) === p("(1*3)/2"))
	}
	it should "分数" in {
		assert(advance(p("1/2/(3/4)")) === p("1*4/(2*3)"))
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
	"+" should "整数" in {
		assert(advance(p("0.01+3")) === p("3.01"))
		assert(advance(p("1+0.2")) === p("1.2"))
	}
	it should "分数" in {
		assert(advance(p("0.1+2/3")) === p("1/10+2/3"))
		assert(advance(p("1/2+3.4")) === p("1/2+34/10"))
	}
	it should "小数" in {
		assert(advance(p("0.1+0.2")) === p("0.3"))
		assert(advance(p("0.01+0.2")) === p("0.21"))
	}
	"-" should "整数" in {
		assert(advance(p("0.3-4")) === p("-3.7"))
		assert(advance(p("1-2.3")) === p("-1.3"))
	}
	it should "分数" in {
		assert(advance(p("0.1-2/3")) === p("1/10-2/3"))
		assert(advance(p("1/2-0.01")) === p("1/2-1/100"))
	}
	it should "小数" in {
		assert(advance(p("0.1-0.03")) === p("0.07"))
	}
	"*" should "整数" in {
		assert(advance(p("0.3*3")) === p("0.9"))
		assert(advance(p("2*0.1")) === p("0.2"))
	}
	it should "分数" in {
		assert(advance(p("0.3*(1/2)")) === p("3/10*(1/2)"))
		assert(advance(p("1/2*0.3")) === p("1/2*(3/10)"))
	}
	it should "小数" in {
		assert(advance(p("0.1*0.03")) === p("1/10*(3/100)"))
	}
	"/" should "整数" in {
		assert(advance(p("0.1/2")) === p("1/10/2"))
		assert(advance(p("2/0.3")) === p("2/(3/10)"))
	}
	it should "分数" in {
		assert(advance(p("0.1/(2/3)")) === p("1/10/(2/3)"))
		assert(advance(p("1/2/0.3")) === p("1/2/(3/10)"))
	}
	it should "小数" in {
		assert(advance(p("0.1/0.3")) === p("1/10/(3/10)"))
	}
}

class RecurringDecimalSpec extends FlatCalcTest {
	"advance" should "0が続く場合" in {
		assert(advance(RecurringDecimal(Decimal(Int(1), Int(0)), Int(0))) === Decimal(Int(1),Int(0)))
		assert(advance(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(0))) === Decimal(Int(12), Int(-1)))
	}
	it should "循環部にまとめられていない場合"
	"hasFinished" should "通常" in {
		assert(RecurringDecimal(Decimal(Int(12), Int(-3)), Int(45)).hasFinished(defaultVariables) === true)
	}
	it should "0が続く場合" in {
		assert(RecurringDecimal(Decimal(Int(1), Int(0)), Int(0)).hasFinished(defaultVariables) === false)
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(0)).hasFinished(defaultVariables) === false)
	}
	it should "循環部にまとめられていない場合"
	"string" should "" in {
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(3)).string === "1.2(3)")
		assert(RecurringDecimal(Decimal(Int(3), Int(0)), Int(3)).string === "3.(3)")
	}
	
	"-" should "シンプルなパターン" in {
		assert(advance(p("12.3(3)-1.2(3)")) === p("12.3-1.2"))
	}
	it should "シンプルなパターンのマイナスの場合" in {
		assert(advance(p("-123.4(34)-(-1.2(34))")) === p("-123.4-(-1.2)") )
	}
	// +|-で循環部分が微妙に違うパターンもつける
	"+-*/" should "循環小数を直す式に直す" in {
		assert(advance(p("1.2(3)+3")) === p("(12.3(3)-1.2(3))/9+3"))
		assert(advance(p("12.(34)-1")) === p("(1234.(34)-12.(34))/99-1"))
		assert(advance(p("1.2(3)*2")) === p("(12.3(3)-1.2(3))/9*2"))
		assert(advance(p("1.2(3)/2")) === p("(12.3(3)-1.2(3))/9/2"))
	}
	it should "マイナスの場合" in {
		assert(advance(p("-1.2(34)+1")) === p("(-123.4(34)-(-1.2(34)))/99+1"))
	}
}
