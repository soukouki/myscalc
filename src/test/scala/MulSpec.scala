import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.{Mul, Num}

class MulSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Mulを2つ受け取り、掛けた数を返す" in {
		assert(Mul(Num(2), Num(2)).result === Num(4))
		assert(Mul(Num(-2), Num(2)).result === Num(-4))
		assert(Mul(Num(2), Num(-2)).result === Num(-4))
		assert(Mul(Num(-2), Num(-2)).result === Num(4))
		assert(Mul(Num(2), Num(0)).result === Num(0))
	}
}
