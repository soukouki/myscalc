import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.{Mul, Int}

class MulSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Mulを2つ受け取り、掛けた数を返す" in {
		assert(Mul(Int(2), Int(2)).result === Int(4))
		assert(Mul(Int(-2), Int(2)).result === Int(-4))
		assert(Mul(Int(2), Int(-2)).result === Int(-4))
		assert(Mul(Int(-2), Int(-2)).result === Int(4))
		assert(Mul(Int(2), Int(0)).result === Int(0))
	}
}
