import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.num._
import myscalc.calc.operator._

class RationalSpec extends FlatSpec with DiagrammedAssertions {
	"+" should "Intを渡されたら"
	it should "Rationalを渡されたら"
	"-" should "Intを渡されたら"
	it should "Rationalを渡されたら"
	"+-*/" should "Infを渡されたら"
	"string" should "Divと同じように" in {
		assert(Rational(Int(1), Int(2)).string === "1/2")
	}
	it should "他のOperatorからも正しくできるか" in {
		assert(Div(Int(1), Rational(Int(2), Int(3))).string === "1/(2/3)")
	}
}
