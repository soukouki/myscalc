package myscalc.calc

case class Mul(left: Num, right: Num) {
	def result: Num = {
		left * right
	}
}
