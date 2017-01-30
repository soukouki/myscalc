import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.num._
import myscalc.calc.operator._

class InfSpec extends FlatSpec with DiagrammedAssertions {
	"+-*/" should "全てInfを返す" in {
		//TODO: Infのテストを[[#p]]を使えるように
		//Infのリテラルがないので[[#p]]を使えない。
		//リテラルを追加したときにまた対応する。
		assert(Add(Int(1), Inf()).advance === Inf())
		assert(Sub(Int(1), Inf()).advance === Inf())
		assert(Mul(Int(1), Inf()).advance === Inf())
		assert(Div(Int(1), Inf()).advance === Inf())
	}
}
