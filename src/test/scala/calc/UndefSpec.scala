
import myscalc.Parse

import myscalc.calc.Undef

class UndefSpec extends FlatCalcTest {
	"#advance" should "numと演算すると必ずUndefになる" in {
		assert(advance(p("1+Undef")) === Undef())
		assert(advance(p("1-Undef")) === Undef())
		assert(advance(p("1*Undef")) === Undef())
		assert(advance(p("1/Undef")) === Undef())
	}
	"parse" should "" in {
		assert(Parse("Undef") === Right(Undef()))
	}
}
