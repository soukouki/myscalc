import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.Parse
import myscalc.calc.num._
import myscalc.calc.operator._

class ParseSpec extends FlatSpec with DiagrammedAssertions {
	"apply" should "数字の場合、Intが戻る" in {
		assert(Parse("3") === Int(3))
		assert(Parse("12") === Int(12))
	}
	it should "大きな数をパースできるか" in {
		// 2147483647は32bitの符号付き整数の最大
		assert(Parse("2147483648") === Int(BigInt("80000000", 16)))
		// 9223372036854775807は64bitの符号付き整数の最大
		assert(Parse("9223372036854775808") === Int(BigInt("8000000000000000", 16)))
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
	it should "括弧を正しく処理できるか" in {
		assert(Parse("(12)") === Int(12))
		assert(Parse("(1+2)") === Add(Int(1), Int(2)))
		assert(Parse("(1+2)*3") === Mul(Add(Int(1), Int(2)), Int(3)))
	}
}
