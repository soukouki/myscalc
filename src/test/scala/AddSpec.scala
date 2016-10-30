import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.{Add, Int}

class AddSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Intを2つ受け取り、足した数を返す" in {
		assert(Add(Int(1), Int(2)).result === Int(3))
	}
}
