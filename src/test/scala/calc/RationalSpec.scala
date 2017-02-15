import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.num._
import myscalc.calc.operator._

class RationalSpec extends FlatSpec with DiagrammedAssertions with CalcTestUtility {
	"advance" should "約分できるか" in {
		assert(Rational(Int(4), Int(6)).advance === Rational(Int(2), Int(3)))
		assert(Rational(Int(-2), Int(4)).advance === Rational(Int(-1), Int(2)))
	}
	it should "整数への約分" in {
		assert(Rational(Int(2), Int(1)).advance === Int(2))
	}
	it should "約分は2以上の最小公約数ずつ" in {
		assert(Rational(Int(18), Int(24)).advance === Rational(Int(9), Int(12)))
		assert(Rational(Int(9), Int(12)).advance === Rational(Int(3), Int(4)))
		assert(Rational(Int(-4), Int(4)).advance === Rational(Int(-2), Int(2)))
	}
	it should "マイナスの扱い" in {
		assert(Rational(Int(1), Int(-2)).advance === Rational(Int(-1), Int(2)))
		assert(Rational(Int(-1), Int(-2)).advance === Rational(Int(1), Int(2)))
	}
	it should "約分よりマイナスの扱いを先にする" in {
		assert(Rational(Int(2), Int(-4)).advance === Rational(Int(-2), Int(4)))
	}
	"hasFinished" should "約分ができるとき" in {
		assert(Rational(Int(2), Int(3)).hasFinished === true)
		assert(Rational(Int(4), Int(6)).hasFinished === false)
	}
	it should "マイナスの扱い" in {
		assert(Rational(Int(1), Int(2)).hasFinished === true)
		assert(Rational(Int(-1), Int(2)).hasFinished === true)
		assert(Rational(Int(1), Int(-2)).hasFinished === false)
		assert(Rational(Int(-1), Int(-2)).hasFinished === false)
	}
	"string" should "Divと同じように" in {
		assert(Rational(Int(1), Int(2)).string === "1/2")
	}
	it should "他のOperatorからも正しくできるか" in {
		assert(Div(Int(1), Rational(Int(2), Int(3))).string === "1/(2/3)")
	}
	"+" should "整数" in {
		assert(p("1/2+3").advance === p("(1+2*3)/2"))
		assert(p("1+2/3").advance === p("(3*1+2)/3"))
	}
	it should "同分母" in {
		assert(p("1/3+2/3").advance === p("(1+2)/3"))
	}
	it should "異分母" in {
		// 通分は[[Rational]]の仕様でできないので
		assert(p("1/2+3/4").advance === p("(1*4+3*2)/(2*4)"))
	}
	"-" should "整数" in {
		assert(p("1/2-3").advance === p("(1-2*3)/2"))
		assert(p("1-2/3").advance === p("(3*1-2)/3"))
	}
	it should "同分母" in {
		assert(p("1/3-2/3").advance === p("(1-2)/3"))
	}
	it should "異分母" in {
		assert(p("1/2-3/4").advance === p("(1*4-3*2)/(2*4)"))
	}
	"*" should "整数" in {
		assert(p("1/2*3").advance === p("1*3/2"))
		assert(p("4*(2/3)").advance === p("4*2/3"))
	}
	it should "分数" in {
		assert(p("1/2*(3/4)").advance === p("1*3/(2*4)"))
	}
	"/" should "整数" in {
		assert(p("1/2/3").advance === p("1/(2*3)"))
		assert(p("1/(2/3)").advance === p("(1*3)/2"))
	}
	it should "分数" in {
		assert(p("1/2/(3/4)").advance === p("1*4/(2*3)"))
	}
	"+-*/" should "Infを渡されたら" // case不足で警告が出るので省略
}
