package myscalc.calc

case class Sub(left: Num, right: Num) {
	def result: Num = {
		left - right
	}
}
