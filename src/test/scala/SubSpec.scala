import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.{Sub, Int}

class SubSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Intを2つ受け取り、引いた値を返す" in {
		assert(Sub(Int(2), Int(1)).result === Int(1))
		assert(Sub(Int(1), Int(2)).result === Int(-1))
	}
}
