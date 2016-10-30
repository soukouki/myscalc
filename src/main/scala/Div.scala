package myscalc.calc

case class Div(left: Num, right: Num) {
	def result: Num = {
		left / right
	}
}
