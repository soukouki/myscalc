import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.{Div, Num}

class DivSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Numを2つ受け取り、割った数を返す" in {
		assert(Div(Num(4), Num(2)).result === Num(2))
	}
}
