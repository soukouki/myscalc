import org.scalatest.{FlatSpec, DiagrammedAssertions}

import myscalc.Parse
import myscalc.calc.Base

trait CalcTestUtility {
	def p(f: String): Base = {
		def i(t: Base, f: String): Base = if(!t.hasFinished && t.advance.string == f) i(t.advance, f) else t
		i(Parse(f).right.get, f)
	}
}

trait FlatCalcTest extends FlatSpec with DiagrammedAssertions with CalcTestUtility
