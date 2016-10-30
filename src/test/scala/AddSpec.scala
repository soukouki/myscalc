import org.scalatest._
import myscalc.calc.{Add, Num}

class AddSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "NumとNumを足すことができる" in {
		assert(Add(Num(1), Num(2)).result === Num(3))
	}
}
