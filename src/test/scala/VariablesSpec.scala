
import myscalc.calc.num.Int
import myscalc.calc.operator.Add

import myscalc.variables.Variables
import myscalc.variables.CharKey

/** [[Variable]]とは役割とかも違うので別ファイルで */
class VariablesSpec extends FlatTest {
	"Base#advance" should "Variablesを引数に取る" in {
		assert(
			Add(Int(1), Int(2)).advance(Variables(Map(CharKey('y') -> Int(1)))) ===
			(Int(3), Variables(Map(CharKey('y') -> Int(1)))))
		assert(
			Add(Int(1), Int(2)).advance(
				Variables(Map(CharKey('x') -> Add(Int(1), Int(2)), CharKey('y') -> Int(0))))
			=== (Int(3), Variables(Map(CharKey('x') -> Add(Int(1), Int(2)), CharKey('y') -> Int(0)))))
	}
	"Base#hasFinished" should "Variablesを引数に取る" in {
		assert(Add(Int(1), Int(2)).hasFinished(Variables(Map(CharKey('y') -> Int(1)))) === false)
		assert(Int(2).hasFinished(Variables(Map(CharKey('x') -> Add(Int(1), Int(2)), CharKey('y') -> Int(0)))) === true)
	}
}
