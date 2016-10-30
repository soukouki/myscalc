import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.{Add, Num}

class AddSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Numを2つ受け取り、足した数を返す" in {
		assert(Add(Num(1), Num(2)).result === Num(3))
	}
}
