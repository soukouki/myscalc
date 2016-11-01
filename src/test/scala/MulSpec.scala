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
}
