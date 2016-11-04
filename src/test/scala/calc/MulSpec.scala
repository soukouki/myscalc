import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc._

class MulSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Mulを2つ受け取り、掛けた数を返す" in {
		assert(Mul(Int(2), Int(2)).result === Int(4))
		assert(Mul(Int(-2), Int(2)).result === Int(-4))
		assert(Mul(Int(2), Int(-2)).result === Int(-4))
		assert(Mul(Int(-2), Int(-2)).result === Int(4))
		assert(Mul(Int(2), Int(0)).result === Int(0))
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Mul(Add(Int(2), Int(1)), Int(2)).result === Mul(Int(3), Int(2)))
	}
	"string" should "式の間に*をつけて返す" in {
		assert(Mul(Int(1), Int(2)).string === "1*2")
		assert(Mul(Add(Int(1), Int(2)), Int(3)).string === "1+2*3")
		assert(Mul(Mul(Int(1), Int(2)), Int(3)).string === "1*2*3")
	}
	it should "右側が+-の場合、括弧を付ける" in {
		assert(Mul(Int(1), Add(Int(2), Int(3))).string === "1*(2+3)")
	}
}
