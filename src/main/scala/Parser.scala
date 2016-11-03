package myscalc

import scala.util.parsing.combinator._
import myscalc.calc._

object Parse extends RegexParsers {
	def apply(input: String): Base = parseAll(expr, input) match {
		case Success(result, _) => result
		case failure : NoSuccess => scala.sys.error(failure.msg + "\ninput is \"" + input + "\"")
	}
	
	def expr: Parser[Base] = exprAddSub
	def exprAddSub: Parser[Base] = chainl1(
		exprMulDiv,
		"+" ^^ {op => (l, r) => Add(l, r)} |
		"-" ^^ {op => (l, r) => Sub(l, r)}
	)
	def exprMulDiv: Parser[Base] = chainl1(
		number | parenthesis,
		"*" ^^ {op => (l, r) => Mul(l, r)} |
		"/" ^^ {op => (l, r) => Div(l, r)}
	)
	def parenthesis: Parser[Base] = "(" ~ expr ~ ")" ^^ {case "(" ~ exp ~ ")" => exp}
	def number: Parser[Base] = """(\+|-)?\d+""".r ^^ {s => Int(s.toInt)}
}
