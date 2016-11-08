package myscalc.calc

/**[[calc]]パッケージ内の全てのクラスの基底trait*/
trait Base {
	def isContinue: Boolean
	def result: Base
	def string: String
}
