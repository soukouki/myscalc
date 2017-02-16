package myscalc

import scala.util.parsing.combinator._
import myscalc.calc._
import myscalc.calc.num._
import myscalc.calc.operator._

import myscalc.variables.CharKey

object Parse extends RegexParsers {
	def apply(input: String): Either[String, Base] = parseAll(expr, input) match {
		case Success(advance, _) => Right(advance)
		case failure : NoSuccess => Left(failure.msg)
	}
	
	private def expr: Parser[Base] = exprNonEqual ||| exprEqual
	private def exprNonEqual: Parser[Base] = exprAddSub
	private def exprEqual: Parser[Base] = variable ~ "=" ~ exprNonEqual ^^ {case(l ~ "=" ~ r) => Equal(l, r)}
	
	private def exprAddSub: Parser[Base] = chainl1(
		exprMulDiv,
		"+" ^^ {op => (l, r) => Add(l, r)} |||
		"-" ^^ {op => (l, r) => Sub(l, r)}
	)
	private def exprMulDiv: Parser[Base] = chainl1(
		parenthesis ||| number ||| variable ||| specialLiteral,
		"*" ^^ {op => (l, r) => Mul(l, r)} |||
		"/" ^^ {op => (l, r) => Div(l, r)}
	)
	private def parenthesis: Parser[Base] = "(" ~> expr <~ ")" ^^ identity
	
	private def number: Parser[Num] = integer ||| decimal ||| recurringDecimal
	private def integer: Parser[Int] = signedIntegerLiteral ^^ {s => Int(BigInt(s))}
	private def decimal: Parser[Decimal] = signedIntegerLiteral ~ "." ~ "[0-9]+".r ^^
		{case l ~ "." ~ r => Decimal(Int(BigInt(l + r)), Int(-(r.length)))}
	private def recurringDecimal: Parser[RecurringDecimal] =
		signedIntegerLiteral ~ "." ~ "[0-9]*".r ~ "(" ~ "[0-9]+".r ~ ")" ^^	{case di ~ "." ~ dd ~ "(" ~ r ~ ")" =>
			RecurringDecimal(Decimal(Int(BigInt(di + dd)), Int(-(dd.length))), Int(BigInt(r)))}
	
	private def signedIntegerLiteral: Parser[String] = "(\\+|-)?([1-9][0-9]*|0)".r ^^ identity
	
	private def variable: Parser[Variable] = "[a-z]".r ^^ (key => {Variable(CharKey(key.charAt(0)))})
		
	private def specialLiteral: Parser[Base] = "Inf" ^^ (a => Inf()) ||| "Undef" ^^ (a => Undef())
}
