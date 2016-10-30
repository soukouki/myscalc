package myscalc.calc

case class Add(left: Num, right: Num) {
	def result: Num = {
		left + right
	}
}
