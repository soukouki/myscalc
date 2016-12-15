import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.num._
import myscalc.calc.operator._

/** [[operator]]共通のテスト */
class OperatorSpec extends FlatSpec with DiagrammedAssertions {
	"advance" should "hasFinishedのやつだけになってから処理を始める" in {
		assert(Add(Rational(Int(5), Int(10)), Int(1)).advance === Add(Rational(Int(1), Int(2)), Int(1)))
	}
}

class AddSubOperatorSpec extends FlatSpec with DiagrammedAssertions {
	"string" should "右側に+-が来た場合、括弧を付ける" in {
		assert(Add(Add(Int(1), Int(2)), Add(Int(3), Int(4))).string === "1+2+(3+4)")
	}
}

class AddSpec extends FlatSpec with DiagrammedAssertions {
	"advance" should "Intを2つ受け取り、足した数を返す" in {
		assert(Add(Int(1), Int(2)).advance === Int(3))
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Add(Add(Int(1), Int(2)), Int(3)).advance === Add(Int(3), Int(3)))
	}
	"string" should "式の間に+をつけて返す" in {
		assert(Add(Int(1), Int(2)).string === "1+2")
		assert(Add(Int(1), Mul(Int(2), Int(3))).string === "1+2*3")
		assert(Add(Sub(Int(1), Int(2)), Int(3)).string === "1-2+3")
	}
}

class SubSpec extends FlatSpec with DiagrammedAssertions {
	"advance" should "Intを2つ受け取り、引いた値を返す" in {
		assert(Sub(Int(2), Int(1)).advance === Int(1))
		assert(Sub(Int(1), Int(2)).advance === Int(-1))
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Sub(Sub(Int(2), Int(1)), Int(1)).advance === Sub(Int(1), Int(1)))
	}
	"string" should "式の間に-をつけて返す" in {
		assert(Sub(Int(1), Int(2)).string === "1-2")
		assert(Sub(Mul(Int(1), Int(2)), Int(3)).string === "1*2-3")
		assert(Add(Sub(Int(1), Int(2)), Int(3)).string === "1-2+3")
	}
}

class MulDivOperatorSpec extends FlatSpec with DiagrammedAssertions {
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

class MulSpec extends FlatSpec with DiagrammedAssertions {
	"advance" should "Mulを2つ受け取り、掛けた数を返す" in {
		assert(Mul(Int(2), Int(2)).advance === Int(4))
		assert(Mul(Int(-2), Int(2)).advance === Int(-4))
		assert(Mul(Int(2), Int(-2)).advance === Int(-4))
		assert(Mul(Int(-2), Int(-2)).advance === Int(4))
		assert(Mul(Int(2), Int(0)).advance === Int(0))
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Mul(Add(Int(2), Int(1)), Int(2)).advance === Mul(Int(3), Int(2)))
	}
	"string" should "式の間に*をつけて返す" in {
		assert(Mul(Int(1), Int(2)).string === "1*2")
		assert(Mul(Mul(Int(1), Int(2)), Int(3)).string === "1*2*3")
	}
}

class DivSpec extends FlatSpec with DiagrammedAssertions {
	"advance" should "Intを2つ受け取り、分数にして返す" in {
		assert(Div(Int(4), Int(2)).advance === Rational(Int(4), Int(2)))
		assert(Div(Int(3), Int(10)).advance === Rational(Int(3), Int(10)))
	}
	it should "0で割った場合、Infを返す" in {
		assert(Div(Int(2), Int(0)).advance.advance === Inf())
		assert(Div(Int(0), Int(0)).advance.advance === Inf())
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Div(Add(Int(2), Int(1)), Add(Int(1), Int(2))).advance === Div(Int(3), Add(Int(1), Int(2))))
	}
	"string" should "式の間に/をつけて返す" in {
		assert(Div(Int(1), Int(2)).string === "1/2")
		assert(Div(Div(Int(1), Int(2)), Int(3)).string === "1/2/3")
	}
}
