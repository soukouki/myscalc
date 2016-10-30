package myscalc.calc

case class Num(n: Int) {
	def + (pair: Num): Num = Num(n + pair.n)
	def - (pair: Num): Num = Num(n - pair.n)
	def * (pair: Num): Num = Num(n * pair.n)
	def / (pair: Num): Num = Num(n / pair.n)
}
