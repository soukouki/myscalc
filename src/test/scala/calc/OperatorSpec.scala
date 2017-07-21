
import myscalc.variables.Variables

import myscalc.calc.num._
import myscalc.calc.operator._

import myscalc.calc.Undef
import myscalc.calc.Variable
import myscalc.variables.CharKey

/** [[operator]]共通のテスト */
class OperatorSpec extends FlatCalcTest {
	"advance" should "hasFinishedのやつだけになってから処理を始める" in {
		assert(advance(Add(Rational(Int(5), Int(10)), Int(1))) === Add(Rational(Int(1), Int(2)), Int(1)))
	}
	it should "値が設定されていなかったVariableのときは、Undefを返しておく" in {
		assert(advance(Add(Variable(CharKey('a')), Int(1))) === Undef()) //眠気でバグ鳥無理
		assert(advance(Add(Int(1), Variable(CharKey('a')))) === Undef())
		assert(advance(Add(Variable(CharKey('a')), Variable(CharKey('a')))) === Undef())
	}
}

class AddSubOperatorSpec extends FlatCalcTest {
	"string" should "右側に+-が来た場合、括弧を付ける" in {
		assert(Add(Add(Int(1), Int(2)), Add(Int(3), Int(4))).string === "1+2+(3+4)")
	}
	it should "右側にマイナスの数が来たときは括弧を付ける" in {
		assert(Add(Int(1), Int(-1)).string === "1+(-1)")
		assert(Sub(Int(1), Int(-1)).string === "1-(-1)")
		assert(Add(Int(1), Div(Int(-1), Int(2))).string === "1+(-1/2)")
		assert(Add(Int(1), Rational(Int(-1), Int(2))).string === "1+(-1/2)")
		assert(Add(Int(1), Decimal(Int(-1), Int(-1))).string === "1+(-0.1)")
	}
}

class AddSpec extends FlatCalcTest {
	"advance" should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(advance(Add(Add(Int(1), Int(2)), Int(3))) === Add(Int(3), Int(3)))
	}
	"string" should "式の間に+をつけて返す" in {
		assert(Add(Int(1), Int(2)).string === "1+2")
		assert(Add(Int(1), Mul(Int(2), Int(3))).string === "1+2*3")
		assert(Add(Sub(Int(1), Int(2)), Int(3)).string === "1-2+3")
	}
}

class SubSpec extends FlatCalcTest {
	"advance" should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(advance(Sub(Sub(Int(2), Int(1)), Int(1))) === Sub(Int(1), Int(1)))
	}
	"string" should "式の間に-をつけて返す" in {
		assert(Sub(Int(1), Int(2)).string === "1-2")
		assert(Sub(Mul(Int(1), Int(2)), Int(3)).string === "1*2-3")
		assert(Add(Sub(Int(1), Int(2)), Int(3)).string === "1-2+3")
	}
}

class MulDivOperatorSpec extends FlatCalcTest {
	"string" should "+-が来た場合、括弧を付ける" in {
		assert(Mul(Add(Int(1), Int(2)), Sub(Int(3), Int(4))).string === "(1+2)*(3-4)")
	}
	it should "右側が*/の場合`1/(2/3)`、括弧を付ける" in {
		assert(Mul(Mul(Int(1), Int(2)), Div(Int(3), Int(4))).string === "1*2*(3/4)")
	}
	it should "左側が-+、右側が*/のとき" in {
		assert(Div(Add(Int(1), Int(2)), Mul(Int(3), Int(4))).string === "(1+2)/(3*4)")
	}
}

class MulSpec extends FlatCalcTest {
	"advance" should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(advance(Mul(Add(Int(2), Int(1)), Int(2))) === Mul(Int(3), Int(2)))
	}
	it should "片方が1なら、もう片方を返す" in {
		assert(advance(p("1.1*1")) === p("1.1"))
		assert(advance(p("1*1.1")) === p("1.1"))
	}
	it should "片方が0なら、0を返す" in {
		assert(advance(p("1.1*0")) === p("0"))
		assert(advance(p("0*2")) === p("0"))
	}
	"string" should "式の間に*をつけて返す" in {
		assert(Mul(Int(1), Int(2)).string === "1*2")
		assert(Mul(Mul(Int(1), Int(2)), Int(3)).string === "1*2*3")
	}
}

class DivSpec extends FlatCalcTest {
	"advance" should "0で割った場合、Infを返す" in {
		assert(advance(Div(Int(2), Int(0))) === Inf())
		assert(advance(Div(Int(0), Int(0))) === Inf())
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(advance(Div(Add(Int(2), Int(1)), Add(Int(1), Int(2)))) === Div(Int(3), Add(Int(1), Int(2))))
	}
	it should "1で割った場合、割られたほうを返す" in {
		assert(advance(p("1.1/1")) === p("1.1"))
	}
	it should "割る数と割られる数が同じ場合、1を返す" in {
		assert(advance(p("1.1/1.1")) === p("1"))
	}
	"string" should "式の間に/をつけて返す" in {
		assert(Div(Int(1), Int(2)).string === "1/2")
		assert(Div(Div(Int(1), Int(2)), Int(3)).string === "1/2/3")
	}
}

class MinusSpec extends FlatCalcTest {
	"advance" should "もっと計算できる場合"
	it should "整数" in {
		assert(advance(p("-(2)")) === p("-2"))
		assert(advance(p("-(-2)")) === p("2"))
	}
	it should "分数" in {
		assert(advance(p("-(1/2)")) === p("-1/2"))
		assert(advance(p("-(-1/2)")) === p("1/2"))
	}
	it should "小数" in {
		assert(advance(p("-(1.2)")) === p("-1.2"))
		assert(advance(p("-(-3.31)")) === p("3.31"))
	}
	it should "循環小数" in {
		assert(advance(p("-(1.(2))")) === p("-1.(2)"))
		assert(advance(p("-(-3.23(1))")) === p("3.23(1)"))
	}
	it should "Operator" in {
		assert(advance(p("-(2*3)")) === p("-(6)"))
		assert(advance(p("-(-(3))")) === p("-(-3)"))
	}
	it should "その他" in {
		assert(advance(p("-(Undef)")) === p("Undef"))
		assert(advance(p("-(Inf)")) === p("Inf"))
		assert(advance(p("-(a)")) === p("Undef"))
	}
	"string" should "" in {
		assert(Minus(Int(3)).string === "-(3)")
		assert(Minus(Mul(Int(2), Int(3))).string === "-(2*3)")
	}
	"parse" should "マイナス" in {
		assert(p("-(12)") === Minus(Int(12)))
		assert(p("-(2*3)") === Minus(Mul(Int(2), Int(3))))
	}
}
