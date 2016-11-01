import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.Parse
import myscalc.calc._

class ParseSpec extends FlatSpec with DiagrammedAssertions {
	"apply" should "数字の場合、Intが戻る" in {
		assert(Parse("3") === Int(3))
		assert(Parse("12") === Int(12))
	}
	it should "数字の先頭が+と-の場合は、その値で戻る" in {
		assert(Parse("+3") === Int(3))
		assert(Parse("-3") === Int(-3))
	}
}
