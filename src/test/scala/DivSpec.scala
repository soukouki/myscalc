import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc._

class DivSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Intを2つ受け取り、割った数を返す" in {
		assert(Div(Int(4), Int(2)).result === Int(2))
	}
	it should "0で割った場合、Infを返す" in {
		assert(Div(Int(2), Int(0)).result === Inf())
		assert(Div(Int(0), Int(0)).result === Inf())
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Div(Add(Int(2), Int(1)), Add(Int(1), Int(2))).result === Div(Int(3), Add(Int(1), Int(2))))
	}
}