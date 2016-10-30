import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.{Div, Int, Inf}

class DivSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Intを2つ受け取り、割った数を返す" in {
		assert(Div(Int(4), Int(2)).result === Int(2))
	}
	it should "0で割った場合、Infを返す" in {
		assert(Div(Int(2), Int(0)).result === Inf())
		assert(Div(Int(0), Int(0)).result === Inf())
	}
}
