import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.Parse
import myscalc.calc._

class ParseSpec extends FlatSpec with DiagrammedAssertions {
	"apply" should "数字の場合、Intが戻る" in {
		assert(Parse("3") === Int(3))
		assert(Parse("12") === Int(12))
	}
	it should "数字の先頭が+と-の場合、その値で戻る" in {
		assert(Parse("+3") === Int(3))
		assert(Parse("-3") === Int(-3))
	}
	it should "+-*/の単体の場合、それ用のクラスになって戻る" in {
		assert(Parse("1+1") === Add(Int(1), Int(1)))
		assert(Parse("1-1") === Sub(Int(1), Int(1)))
		assert(Parse("1*1") === Mul(Int(1), Int(1)))
		assert(Parse("1/1") === Div(Int(1), Int(1)))
	}
	it should "+-の連続を正しく処理できるか" in {
		assert(Parse("1+2+3") === Add(Add(Int(1), Int(2)), Int(3)))
		assert(Parse("1-2+3") === Add(Sub(Int(1), Int(2)), Int(3)))
	}
	it should "*/の連続を正しく処理できるか" in {
		assert(Parse("1*2*3") === Mul(Mul(Int(1), Int(2)), Int(3)))
		assert(Parse("1*2/3") === Div(Mul(Int(1), Int(2)), Int(3)))
	}
	it should "+-と*/の連続を正しく処理できるか" in {
		assert(Parse("1+2*3") === Add(Int(1), Mul(Int(2), Int(3))))
	}
}
