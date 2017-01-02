package myscalc.calc

/**[[calc]]パッケージ内の全てのクラスの基底trait*/
trait Base {
	def hasFinished: Boolean
	def advance: Base
	def string: String
	
	private[calc] def isRightmostMinus: Boolean = string.head=='-'
}
