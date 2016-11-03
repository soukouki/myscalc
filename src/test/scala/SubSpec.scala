import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc._

class SubSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Intを2つ受け取り、引いた値を返す" in {
		assert(Sub(Int(2), Int(1)).result === Int(1))
		assert(Sub(Int(1), Int(2)).result === Int(-1))
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Sub(Sub(Int(2), Int(1)), Int(1)).result === Sub(Int(1), Int(1)))
	}
	"string" should "数字の間に-をつけて返す" in {
		assert(Sub(Int(1), Int(2)).string === "1-2")
	}
	it should "左側が式の場合は、間に+をつける" in {
		assert(Sub(Add(Int(1), Int(2)), Int(3)).string === "1+2-3")
		assert(Sub(Mul(Int(1), Int(2)), Int(3)).string === "1*2-3")
	}
}
