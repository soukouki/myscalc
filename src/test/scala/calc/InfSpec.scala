import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.num._

class InfSpec extends FlatSpec with DiagrammedAssertions {
	"+-*/" should "全てInfを返す" in {
		assert(Inf() + Int(1) === Inf())
	}
}
