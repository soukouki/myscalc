
import myscalc.variables.Variables

import myscalc.calc.num._
import myscalc.calc.operator._

import myscalc.calc.Undef
import myscalc.calc.Variable
import myscalc.variables.CharKey

/** それぞれの[[operator]]で分けるほどでもないもの */
class OperatorSpec extends FlatCalcTest {
	"advance" should "値が設定されていなかったVariableのときは、Undefを返しておく" in {
		assert(advance(p("a+1")) === Undef())
		assert(advance(p("1+a")) === Undef())
		assert(advance(p("a+a")) === Undef())
		assert(advance(p("-(a)")) === p("Undef"))
	}
	it should "Inf" in {
		assert(advance(p("Inf+2")) === Inf())
		assert(advance(p("Inf-2")) === Inf())
		assert(advance(p("Inf*2")) === Inf())
		assert(advance(p("Inf/2")) === Inf())
		assert(advance(p("Inf/Inf")) === p("Inf"))
		assert(advance(p("-(Inf)")) === p("Inf"))
	}
	it should "Undef" in {
		assert(advance(p("1+Undef")) === Undef())
		assert(advance(p("1-Undef")) === Undef())
		assert(advance(p("1*Undef")) === Undef())
		assert(advance(p("1/Undef")) === Undef())
		assert(advance(p("-(Undef)")) === p("Undef"))
	}
	it should "RecurringDecimal" in {
		assert(advance(p("1.2(3)+3")) === p("(12.3(3)-1.2(3))/9+3"))
		assert(advance(p("12.(34)-1")) === p("(1234.(34)-12.(34))/99-1"))
		assert(advance(p("1.2(3)*2")) === p("(12.3(3)-1.2(3))/9*2"))
		assert(advance(p("1.2(3)/2")) === p("(12.3(3)-1.2(3))/9/2"))
	}
	it should "マイナスの場合" in {
		assert(advance(p("-1.2(34)+1")) === p("(-123.4(34)-(-1.2(34)))/99+1"))
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
	"advance" should "hasFinishedがfalseのとき" in {
		assert(advance(p("(1+2)+3")) === p("3+3"))
		assert(advance(p("2/4+1")) === p("1/2+1"))
	}
	it should "整数同士" in {
		assert(advance(p("1+2")) === p("3"))
		assert(advance(p("1+-3")) === p("-2"))
	}
	it should "分数+整数" in {
		assert(advance(p("1/2+3")) === p("(1+2*3)/2"))
		assert(advance(p("1+2/3")) === p("(3*1+2)/3"))
	}
	it should "分数同士(同分母)" in {
		assert(advance(p("1/3+2/3")) === p("(1+2)/3"))
	}
	it should "分数同士(異分母)" in {
		// 通分は[[Rational]]の仕様でできないので
		assert(advance(p("1/2+3/4")) === p("(1*4+3*2)/(2*4)"))
	}
	it should "小数+整数" in {
		assert(advance(p("0.01+3")) === p("3.01"))
		assert(advance(p("1+0.2")) === p("1.2"))
	}
	it should "小数+分数" in {
		assert(advance(p("0.1+2/3")) === p("1/10+2/3"))
		assert(advance(p("1/2+3.4")) === p("1/2+34/10"))
	}
	it should "小数同士" in {
		assert(advance(p("0.1+0.2")) === p("0.3"))
		assert(advance(p("0.01+0.2")) === p("0.21"))
	}
	"string" should "式の間に+をつけて返す" in {
		assert(Add(Int(1), Int(2)).string === "1+2")
		assert(Add(Int(1), Mul(Int(2), Int(3))).string === "1+2*3")
		assert(Add(Sub(Int(1), Int(2)), Int(3)).string === "1-2+3")
	}
}

class SubSpec extends FlatCalcTest {
	"advance" should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(advance(p("(2-1)-1")) === p("1-1"))
	}
	it should "整数同士" in {
		assert(advance(p("2-1")) === p("1"))
		assert(advance(p("-2--3")) === p("1"))
	}
	it should "分数-整数" in {
		assert(advance(p("1/2-3")) === p("(1-2*3)/2"))
		assert(advance(p("1-2/3")) === p("(3*1-2)/3"))
	}
	it should "分数同士(同分母)" in {
		assert(advance(p("1/3-2/3")) === p("(1-2)/3"))
	}
	it should "分数同士(異分母)" in {
		assert(advance(p("1/2-3/4")) === p("(1*4-3*2)/(2*4)"))
	}
	it should "小数-整数" in {
		assert(advance(p("0.3-4")) === p("-3.7"))
		assert(advance(p("1-2.3")) === p("-1.3"))
	}
	it should "小数-分数" in {
		assert(advance(p("0.1-2/3")) === p("1/10-2/3"))
		assert(advance(p("1/2-0.01")) === p("1/2-1/100"))
	}
	it should "小数同士" in {
		assert(advance(p("0.1-0.03")) === p("0.07"))
	}
	it should "循環小数同士(循環部、小数部の桁が同じ)" in {
		assert(advance(p("12.3(3)-1.2(3)")) === p("12.3-1.2"))
		assert(advance(p("-123.4(34)-(-1.2(34))")) === p("-123.4-(-1.2)"))
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
		assert(advance(p("(1*2)*3")) === p("2*3"))
	}
	it should "片方が1なら、もう片方を返す" in {
		assert(advance(p("1.1*1")) === p("1.1"))
		assert(advance(p("1*1.1")) === p("1.1"))
	}
	it should "片方が0なら、0を返す" in {
		assert(advance(p("1.1*0")) === p("0"))
		assert(advance(p("0*2")) === p("0"))
	}
	it should "整数同士" in {
		assert(advance(p("2*3")) === p("6"))
		assert(advance(p("-2*-3")) === p("6"))
	}
	it should "分数*整数" in {
		assert(advance(p("1/2*3")) === p("1*3/2"))
		assert(advance(p("4*(2/3)")) === p("4*2/3"))
	}
	it should "分数同士" in {
		assert(advance(p("1/2*(3/4)")) === p("1*3/(2*4)"))
	}
	it should "小数*整数" in {
		assert(advance(p("0.3*3")) === p("0.9"))
		assert(advance(p("2*0.1")) === p("0.2"))
	}
	it should "小数*分数" in {
		assert(advance(p("0.3*(1/2)")) === p("3/10*(1/2)"))
		assert(advance(p("1/2*0.3")) === p("1/2*(3/10)"))
	}
	it should "小数同士" in {
		assert(advance(p("0.1*0.03")) === p("1/10*(3/100)"))
	}
	"string" should "式の間に*をつけて返す" in {
		assert(Mul(Int(1), Int(2)).string === "1*2")
		assert(Mul(Mul(Int(1), Int(2)), Int(3)).string === "1*2*3")
	}
}

class DivSpec extends FlatCalcTest {
	"advance" should "0で割った場合、Infを返す" in {
		assert(advance(p("2/0")) === p("Inf"))
		assert(advance(p("0/0")) === p("Inf"))
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(advance(p("(2+1)/(1+2)")) === p("3/(1+2)"))
	}
	it should "1で割った場合、割られたほうを返す" in {
		assert(advance(p("1.1/1")) === p("1.1"))
	}
	it should "割る数と割られる数が同じ場合、1を返す" in {
		assert(advance(p("1.1/1.1")) === p("1"))
	}
	it should "整数同士" in {
		// Divをそのまま使っているのは#pだと分数になってしまうため
		assert(advance(Div(Int(4), Int(2))) === p("2"))
		assert(advance(Div(Int(6), Int(2))) === p("3"))
	}
	it should "整数同士(割り切れないとき)" in {
		// #pを使うとわかりにくい気がするのでそのまま
		assert(advance(Div(Int(3), Int(2))) === Rational(Int(3), Int(2)))
		assert(advance(Div(Int(3), Int(10))) === Rational(Int(3), Int(10)))
	}
	it should "分数/整数" in {
		assert(advance(p("1/2/3")) === p("1/(2*3)"))
		assert(advance(p("1/(2/3)")) === p("(1*3)/2"))
	}
	it should "分数同士" in {
		assert(advance(p("1/2/(3/4)")) === p("1*4/(2*3)"))
	}
	it should "小数/整数" in {
		assert(advance(p("0.1/2")) === p("1/10/2"))
		assert(advance(p("2/0.3")) === p("2/(3/10)"))
	}
	it should "小数/分数" in {
		assert(advance(p("0.1/(2/3)")) === p("1/10/(2/3)"))
		assert(advance(p("1/2/0.3")) === p("1/2/(3/10)"))
	}
	it should "小数同士" in {
		assert(advance(p("0.1/0.3")) === p("1/10/(3/10)"))
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
		assert(advance(p("-(2/4)")) === p("-(1/2)"))
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
	"string" should "" in {
		assert(Minus(Int(3)).string === "-(3)")
		assert(Minus(Mul(Int(2), Int(3))).string === "-(2*3)")
	}
	"parse" should "マイナス" in {
		assert(p("-(12)") === Minus(Int(12)))
		assert(p("-(2*3)") === Minus(Mul(Int(2), Int(3))))
	}
}
