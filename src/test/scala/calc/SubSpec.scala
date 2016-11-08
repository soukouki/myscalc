import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.num._
import myscalc.calc.operator._

class SubSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Intを2つ受け取り、引いた値を返す" in {
		assert(Sub(Int(2), Int(1)).result === Int(1))
		assert(Sub(Int(1), Int(2)).result === Int(-1))
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Sub(Sub(Int(2), Int(1)), Int(1)).result === Sub(Int(1), Int(1)))
	}
	"string" should "式の間に-をつけて返す" in {
		assert(Sub(Int(1), Int(2)).string === "1-2")
		assert(Sub(Add(Int(1), Int(2)), Int(3)).string === "1+2-3")
		assert(Sub(Int(1), Mul(Int(2), Int(3))).string === "1-2*3")
	}
	it should "左側に*/が来た場合、括弧を付ける" in {
		assert(Sub(Mul(Int(1), Int(2)), Int(3)).string === "(1*2)-3")
	}
}
