package myscalc.calc

case class Inf() extends Num {
	override def + (pair: Num) = Inf()
	override def - (pair: Num) = Inf()
	override def * (pair: Num) = Inf()
	override def / (pair: Num) = Inf()
}
