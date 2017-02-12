import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.Num
import myscalc.calc.num._
import myscalc.calc.operator._

class RecurringDecimalSpec extends FlatSpec with DiagrammedAssertions with CalcTestUtility {
	"advance" should "0が続く場合" in {
		assert(RecurringDecimal(Decimal(Int(1), Int(0)), Int(0)).advance === Decimal(Int(1),Int(0)))
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(0)).advance === Decimal(Int(12), Int(-1)))
	}
	it should "循環部にまとめられていない場合"
	"hasFinished" should "通常" in {
		assert(RecurringDecimal(Decimal(Int(12), Int(-3)), Int(45)).hasFinished === true)
	}
	it should "0が続く場合" in {
		assert(RecurringDecimal(Decimal(Int(1), Int(0)), Int(0)).hasFinished === false)
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(0)).hasFinished === false)
	}
	it should "循環部にまとめられていない場合"
	"string" should "" in {
		assert(RecurringDecimal(Decimal(Int(12), Int(-1)), Int(3)).string === "1.2(3)")
		assert(RecurringDecimal(Decimal(Int(3), Int(0)), Int(3)).string === "3.(3)")
	}
	
	"-" should "シンプルなパターン" in {
		assert(p("12.3(3)-1.2(3)").advance === p("12.3-1.2"))
	}
	// +|-で循環部分が微妙に違うパターンもつける
	"+-*/" should "循環小数を直す式に直す" in {
		assert(p("1.2(3)+3").advance === p("(12.3(3)-1.2(3))/9+3"))
		assert(p("12.(34)-1").advance === p("(1234.(34)-12.(34))/99-1"))
		assert(p("1.2(3)*2").advance === p("(12.3(3)-1.2(3))/9*2"))
		assert(p("1.2(3)/2").advance === p("(12.3(3)-1.2(3))/9/2"))
	}
}
