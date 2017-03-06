import myscalc.calc.num._
import myscalc.calc.operator._

class IntSpec extends FlatCalcTest {
	"+" should "" in {
		assert(p("1+2").advance === p("3"))
		assert(p("1+-3").advance === p("-2"))
	}
	"-" should "" in {
		assert(p("2-1").advance === p("1"))
		assert(p("-2--3").advance === p("1"))
	}
	"*" should "" in {
		assert(p("2*3").advance === p("6"))
		assert(p("-2*-3").advance === p("6"))
	}
	"/" should "" in {
		// Divをそのまま使っているのは#pだと分数になってしまうため
		assert(Div(Int(4), Int(2)).advance === p("2"))
		assert(Div(Int(6), Int(2)).advance === p("3"))
	}
	it should "割り切れないときは分数にして返す" in {
		// #pを使うとわかりにくい気がするのでそのまま
		assert(Div(Int(3), Int(2)).advance === Rational(Int(3), Int(2)))
		assert(Div(Int(3), Int(10)).advance === Rational(Int(3), Int(10)))
	}
}

class InfSpec extends FlatCalcTest {
	"+-*/" should "全てInfを返す" in {
		assert(p("Inf+2").advance === Inf())
		assert(p("Inf-2").advance === Inf())
		assert(p("Inf*2").advance === Inf())
		assert(p("Inf/2").advance === Inf())
		assert(p("Inf/Inf").advance === p("Inf"))
	}
}

class RationalSpec extends FlatCalcTest {
	"advance" should "約分できるか" in {
		assert(Rational(Int(4), Int(6)).advance === Rational(Int(2), Int(3)))
		assert(Rational(Int(-2), Int(4)).advance === Rational(Int(-1), Int(2)))
	}
	it should "整数への約分" in {
		assert(Rational(Int(2), Int(1)).advance === Int(2))
	}
	it should "約分は2以上の最小公約数ずつ" in {
		assert(Rational(Int(18), Int(24)).advance === Rational(Int(9), Int(12)))
		assert(Rational(Int(9), Int(12)).advance === Rational(Int(3), Int(4)))
		assert(Rational(Int(-4), Int(4)).advance === Rational(Int(-2), Int(2)))
	}
	it should "マイナスの扱い" in {
		assert(Rational(Int(1), Int(-2)).advance === Rational(Int(-1), Int(2)))
		assert(Rational(Int(-1), Int(-2)).advance === Rational(Int(1), Int(2)))
	}
	it should "約分よりマイナスの扱いを先にする" in {
		assert(Rational(Int(2), Int(-4)).advance === Rational(Int(-2), Int(4)))
	}
	"hasFinished" should "約分ができるとき" in {
		assert(Rational(Int(2), Int(3)).hasFinished === true)
		assert(Rational(Int(4), Int(6)).hasFinished === false)
	}
	it should "マイナスの扱い" in {
		assert(Rational(Int(1), Int(2)).hasFinished === true)
		assert(Rational(Int(-1), Int(2)).hasFinished === true)
		assert(Rational(Int(1), Int(-2)).hasFinished === false)
		assert(Rational(Int(-1), Int(-2)).hasFinished === false)
	}
	"string" should "Divと同じように" in {
		assert(Rational(Int(1), Int(2)).string === "1/2")
	}
	it should "他のOperatorからも正しくできるか" in {
		assert(Div(Int(1), Rational(Int(2), Int(3))).string === "1/(2/3)")
	}
	"+" should "整数" in {
		assert(p("1/2+3").advance === p("(1+2*3)/2"))
		assert(p("1+2/3").advance === p("(3*1+2)/3"))
	}
	it should "同分母" in {
		assert(p("1/3+2/3").advance === p("(1+2)/3"))
	}
	it should "異分母" in {
		// 通分は[[Rational]]の仕様でできないので
		assert(p("1/2+3/4").advance === p("(1*4+3*2)/(2*4)"))
	}
	"-" should "整数" in {
		assert(p("1/2-3").advance === p("(1-2*3)/2"))
		assert(p("1-2/3").advance === p("(3*1-2)/3"))
	}
	it should "同分母" in {
		assert(p("1/3-2/3").advance === p("(1-2)/3"))
	}
	it should "異分母" in {
		assert(p("1/2-3/4").advance === p("(1*4-3*2)/(2*4)"))
	}
	"*" should "整数" in {
		assert(p("1/2*3").advance === p("1*3/2"))
		assert(p("4*(2/3)").advance === p("4*2/3"))
	}
	it should "分数" in {
		assert(p("1/2*(3/4)").advance === p("1*3/(2*4)"))
	}
	"/" should "整数" in {
		assert(p("1/2/3").advance === p("1/(2*3)"))
		assert(p("1/(2/3)").advance === p("(1*3)/2"))
	}
	it should "分数" in {
		assert(p("1/2/(3/4)").advance === p("1*4/(2*3)"))
	}
}

class DecimalSpec extends FlatCalcTest {
	"hasFinished" should "常にtrue" in {
		assert(Decimal(Int(12), Int(-1)).hasFinished === true)
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
		assert(p("0.01+3").advance === p("3.01"))
		assert(p("1+0.2").advance === p("1.2"))
	}
	it should "分数" in {
		assert(p("0.1+2/3").advance === p("1/10+2/3"))
		assert(p("1/2+3.4").advance === p("1/2+34/10"))
	}
	it should "小数" in {
		assert(p("0.1+0.2").advance === p("0.3"))
		assert(p("0.01+0.2").advance === p("0.21"))
	}
	"-" should "整数" in {
		assert(p("0.3-4").advance === p("-3.7"))
		assert(p("1-2.3").advance === p("-1.3"))
	}
	it should "分数" in {
		assert(p("0.1-2/3").advance === p("1/10-2/3"))
		assert(p("1/2-0.01").advance === p("1/2-1/100"))
	}
	it should "小数" in {
		assert(p("0.1-0.03").advance === p("0.07"))
	}
	"*" should "整数" in {
		assert(p("0.3*3").advance === p("0.9"))
		assert(p("2*0.1").advance === p("0.2"))
	}
	it should "分数" in {
		assert(p("0.3*(1/2)").advance === p("3/10*(1/2)"))
		assert(p("1/2*0.3").advance === p("1/2*(3/10)"))
	}
	it should "小数" in {
		assert(p("0.1*0.03").advance === p("1/10*(3/100)"))
	}
	"/" should "整数" in {
		assert(p("0.1/2").advance === p("1/10/2"))
		assert(p("2/0.3").advance === p("2/(3/10)"))
	}
	it should "分数" in {
		assert(p("0.1/(2/3)").advance === p("1/10/(2/3)"))
		assert(p("1/2/0.3").advance === p("1/2/(3/10)"))
	}
	it should "小数" in {
		assert(p("0.1/0.3").advance === p("1/10/(3/10)"))
	}
}

class RecurringDecimalSpec extends FlatCalcTest {
	"advance" should "0が続く場合" in {
		assert(RecurringDecimal(Decimal(Int(1), Int(0)), Int(0)).advance === Decimal(Int(1),Int(0)))
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(0)).advance === Decimal(Int(12), Int(-1)))
	}
	it should "循環部にまとめられていない場合"
	"hasFinished" should "通常" in {
		assert(RecurringDecimal(Decimal(Int(12), Int(-3)), Int(45)).hasFinished === true)
	}
	it should "0が続く場合" in {
		assert(RecurringDecimal(Decimal(Int(1), Int(0)), Int(0)).hasFinished === false)
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(0)).hasFinished === false)
	}
	it should "循環部にまとめられていない場合"
	"string" should "" in {
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(3)).string === "1.2(3)")
		assert(RecurringDecimal(Decimal(Int(3), Int(0)), Int(3)).string === "3.(3)")
	}
	
	"-" should "シンプルなパターン" in {
		assert(p("12.3(3)-1.2(3)").advance === p("12.3-1.2"))
	}
	it should "シンプルなパターンのマイナスの場合" in {
		assert(p("-123.4(34)-(-1.2(34))").advance === p("-123.4-(-1.2)") )
	}
	// +|-で循環部分が微妙に違うパターンもつける
	"+-*/" should "循環小数を直す式に直す" in {
		assert(p("1.2(3)+3").advance === p("(12.3(3)-1.2(3))/9+3"))
		assert(p("12.(34)-1").advance === p("(1234.(34)-12.(34))/99-1"))
		assert(p("1.2(3)*2").advance === p("(12.3(3)-1.2(3))/9*2"))
		assert(p("1.2(3)/2").advance === p("(12.3(3)-1.2(3))/9/2"))
	}
	it should "マイナスの場合" in {
		assert(p("-1.2(34)+1").advance === p("(-123.4(34)-(-1.2(34)))/99+1"))
	}
}
