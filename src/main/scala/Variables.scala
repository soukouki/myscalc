package myscalc.variables

import myscalc.calc.Base

/**
	[[calc.Variable]]を保管する役割。
*/
case class Variables(vas: Map[Key, Base]) {
	def value(k: Key) = vas.get(k).get
	
	def existing(k: Key) = vas.contains(k)
	
	def substitution(k: Key, v: Base) = Variables(vas.updated(k, v))
}

/** 変数名を保持するクラス */
trait Key {
	override def toString: String
}

/** 一文字の変数名を保存するクラス */
case class CharKey(c: Char) extends Key {
	override def toString = c.toString
}
