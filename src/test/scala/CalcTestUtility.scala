import org.scalatest.{FlatSpec, DiagrammedAssertions}

import myscalc.variables.Variables

import myscalc.Parse
import myscalc.calc.Base

trait FlatTest extends FlatSpec with DiagrammedAssertions

trait FlatCalcTest extends FlatTest {
	def p(formula: String): Base = {
		pI(Parse(formula).right.get, formula)
	}
	
	private def pI(t: Base, formula: String): Base = {
		if(!t.hasFinished(defaultVariables)) {
			val (ad, _) = t.advance(defaultVariables)
			if(ad.string == formula) pI(ad, formula) else t
		} else t
	}
	
	val defaultVariables = Variables(Map())
	
	def advance(b: Base, vs: Variables = defaultVariables): Base = {val (ad, _) = b.advance(vs); ad}
}
