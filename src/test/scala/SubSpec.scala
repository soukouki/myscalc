import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.{Sub, Num}

class SubSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Numを2つ受け取り、引いた値を返す" in {
		assert(Sub(Num(2), Num(1)).result === Num(1))
		assert(Sub(Num(1), Num(2)).result === Num(-1))
	}
}
