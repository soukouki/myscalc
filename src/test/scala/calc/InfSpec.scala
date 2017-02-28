import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.num._
import myscalc.calc.operator._

class InfSpec extends FlatSpec with DiagrammedAssertions with CalcTestUtility {
	"+-*/" should "全てInfを返す" in {
		assert(p("Inf+2").advance === Inf())
		assert(p("Inf-2").advance === Inf())
		assert(p("Inf*2").advance === Inf())
		assert(p("Inf/2").advance === Inf())
	}
}
