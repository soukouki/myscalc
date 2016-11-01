package myscalc

import scala.util.parsing.combinator._
import myscalc.calc._

object Parse extends RegexParsers {
	def apply(input: String): Base = parseAll(expr, input) match {
		case Success(result, _) => result
		case failure : NoSuccess => scala.sys.error(failure.msg)
	}
	
	def expr: Parser[Base] = {
		"""(\+|-)?\d+""".r ^^ {s => Int(s.toInt)}
	}
}
