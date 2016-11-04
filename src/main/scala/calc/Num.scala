package myscalc.calc

abstract class Num extends Base {
	override def isContinue = false
	override def result: Num = this
	def + (pair: Num): Num
	def - (pair: Num): Num
	def * (pair: Num): Num
	def / (pair: Num): Num
}
object Num {
	def unapply(n: Num) = Option(())
}
