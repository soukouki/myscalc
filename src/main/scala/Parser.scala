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
	
	private def expr: Parser[Base] = exprAddSub
	private def exprAddSub: Parser[Base] = chainl1(
		exprMulDiv,
		"+" ^^ {op => (l, r) => Add(l, r)} |||
		"-" ^^ {op => (l, r) => Sub(l, r)}
	)
	private def exprMulDiv: Parser[Base] = chainl1(
		parenthesis ||| number ||| specialLiteral,
		"*" ^^ {op => (l, r) => Mul(l, r)} |||
		"/" ^^ {op => (l, r) => Div(l, r)}
	)
	private def parenthesis: Parser[Base] = "(" ~> expr <~ ")" ^^ identity
	
	private def number: Parser[Base] = integer ||| decimal ||| recurringDecimal
	private def integer: Parser[Base] = signedIntegerLiteral ^^ {s => Int(BigInt(s))}
	private def decimal: Parser[Base] = signedIntegerLiteral ~ "." ~ "[0-9]+".r ^^
		{case l ~ "." ~ r => Decimal(Int(BigInt(l + r)), Int(-(r.length)))}
	private def recurringDecimal: Parser[Base] = signedIntegerLiteral ~ "." ~ "[0-9]*".r ~ "(" ~ "[0-9]+".r ~ ")" ^^
		{case di ~ "." ~ dd ~ "(" ~ r ~ ")" =>
			RecurringDecimal(Decimal(Int(BigInt(di + dd)), Int(-(dd.length))), Int(BigInt(r)))}
	
	private def signedIntegerLiteral: Parser[String] = "(\\+|-)?([1-9][0-9]*|0)".r ^^ identity
	
	private def specialLiteral: Parser[Base] = "Inf" ^^ ((a) => {Inf()})
}
