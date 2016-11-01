package myscalc

import scala.util.parsing.combinator._
import myscalc.calc._

object Parse extends RegexParsers {
	def apply(input: String): Base = parseAll(expr, input) match {
		case Success(result, _) => result
		case failure : NoSuccess => scala.sys.error(failure.msg + "\ninput is \"" + input + "\"")
	}
	
	def expr: Parser[Base] = chainl1(
		number,
		"+" ^^ {op => (l, r) => Add(l, r)} |
		"-" ^^ {op => (l, r) => Sub(l, r)} |
		"*" ^^ {op => (l, r) => Mul(l, r)} |
		"/" ^^ {op => (l, r) => Div(l, r)}
	)
	def number: Parser[Base] = """(\+|-)?\d+""".r ^^ {s => Int(s.toInt)}
}
