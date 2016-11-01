package myscalc.calc

abstract class Num extends Base {
	override def isNum = true
	override def isContinue = false
	override def result: Num = this
	def + (pair: Num): Num
	def - (pair: Num): Num
	def * (pair: Num): Num
	def / (pair: Num): Num
}
