package myscalc

import scala.util.parsing.combinator._
import myscalc.calc._
import myscalc.calc.num._
import myscalc.calc.operator._

object Parse extends RegexParsers {
	def apply(input: String): Either[String, Base] = parseAll(expr, input) match {
		case Success(advance, _) => Right(advance)
		case failure : NoSuccess => Left(failure.msg)
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
	def parenthesis: Parser[Base] = "(" ~> expr <~ ")" ^^ identity
	
	def number: Parser[Base] = integer ||| decimal
	def integer: Parser[Base] = "(\\+|-)?[1-9][0-9]*".r ^^ {s => Int(BigInt(s))}
	def decimal: Parser[Base] = "(\\+|-)?[0-9]+".r ~ "." ~ "[0-9]+".r ^^
		{case l ~ "." ~ r => Decimal(Int(BigInt(l + r)), Int(-(r.length)))}
}
