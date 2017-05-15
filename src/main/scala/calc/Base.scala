package myscalc.calc

import myscalc.variables.Variables

/**[[calc]]パッケージ内の全てのクラスの基底trait*/
trait Base {
	def hasFinished(v: Variables): Boolean
	def advance(v: Variables): (Base, Variables)
	def string: String
	
	private[calc] def isRightmostMinus: Boolean = string.head=='-'
}
