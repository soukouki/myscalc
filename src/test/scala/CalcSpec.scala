
import myscalc.Calc

import myscalc.calc.Base
import myscalc.calc.num.Int
import myscalc.variables._

class CalcSpec extends FlatTest {
	"Calc" should "変数のないやつ" in {
		assert(Calc(defaultVariables).calc("1+2") === (Calc(defaultVariables), List("1+2", "3")))
	}
	it should "変数" in {
		val (ncalc, result) = Calc(defaultVariables).calc("x=12")
		assert(result === List("x=12", "Undef"))
		assert(ncalc === Calc(Variables(Map(CharKey('x') -> Int(12)))))
		assert(ncalc.calc("x") === (Calc(Variables(Map(CharKey('x') -> Int(12)))), List("x", "12")))
	}
}
