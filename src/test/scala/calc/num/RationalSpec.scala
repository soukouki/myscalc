import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.num._
import myscalc.calc.operator._

class RationalSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "約分できるか" in {
		assert(Rational(Int(4), Int(6)).result === Rational(Int(2), Int(3)))
		assert(Rational(Int(-2), Int(4)).result === Rational(Int(-1), Int(2)))
	}
	it should "約分は2以上の最小公約数ずつ" in {
		assert(Rational(Int(18), Int(24)).result === Rational(Int(9), Int(12)))
		assert(Rational(Int(9), Int(12)).result === Rational(Int(3), Int(4)))
		assert(Rational(Int(-4), Int(4)).result === Rational(Int(-2), Int(2)))
	}
	it should "マイナスの扱い" in {
		assert(Rational(Int(1), Int(-2)).result === Rational(Int(-1), Int(2)))
		assert(Rational(Int(-1), Int(-2)).result === Rational(Int(1), Int(2)))
	}
	it should "約分よりマイナスの扱いを先にする" in {
		assert(Rational(Int(2), Int(-4)).result === Rational(Int(-2), Int(4)))
	}
	"isContinue" should "約分ができるときはtrue" in {
		assert(Rational(Int(2), Int(3)).isContinue === false)
		assert(Rational(Int(4), Int(6)).isContinue === true)
	}
	it should "マイナスの扱い" in {
		assert(Rational(Int(1), Int(2)).isContinue === false)
		assert(Rational(Int(-1), Int(2)).isContinue === false)
		assert(Rational(Int(1), Int(-2)).isContinue === true)
		assert(Rational(Int(-1), Int(-2)).isContinue === true)
	}
	"string" should "Divと同じように" in {
		assert(Rational(Int(1), Int(2)).string === "1/2")
	}
	it should "他のOperatorからも正しくできるか" in {
		assert(Div(Int(1), Rational(Int(2), Int(3))).string === "1/(2/3)")
	}
	"+" should "整数" in {
		// 1/2+3=(1+2*3)/2=(1+6)/2=7/2
		assert(Add(Rational(Int(1), Int(2)), Int(3)).result === Div(Add(Int(1), Mul(Int(2), Int(3))), Int(2)))
		// 1+2/3=(2+3*1)/3=(2+3)/3=5/3
		assert(Add(Int(1), Rational(Int(2), Int(3))).result === Div(Add(Int(2), Mul(Int(3), Int(1))), Int(3)))
	}
	it should "同分母" in {
		// 1/3+2/3=(1+2)/3=3/3=1
		assert(Add(Rational(Int(1), Int(3)), Rational(Int(2), Int(3))).result === Div(Add(Int(1), Int(2)), Int(3)))
	}
	it should "異分母" in {
		// 通分は[[Rational]]の仕様でできないので
		// 1/2+3/4=(1*4+3*2)/(2*4)=(4+6)/8=10/8=5/4
		assert(
			Add(Rational(Int(1), Int(2)), Rational(Int(3), Int(4))).result ===
			Div(Add(Mul(Int(1), Int(4)), Mul(Int(3), Int(2))), Mul(Int(2), Int(4))))
	}
	"-" should "整数" in {
		// 1/2-3=(1-2*3)/2=(1-6)/2=-5/2
		assert(Sub(Rational(Int(1), Int(2)), Int(3)).result === Div(Sub(Int(1), Mul(Int(2), Int(3))), Int(2)))
		// 1-2/3=(2-3*1)/3=(2-3)/2=-1/2
		assert(Sub(Int(1), Rational(Int(2), Int(3))).result === Div(Sub(Int(2), Mul(Int(3), Int(1))), Int(3)))
	}
	it should "同分母" in {
		// 1/3-2/3=(1-2)/3=-1/3
		assert(Sub(Rational(Int(1), Int(3)), Rational(Int(2), Int(3))).result === Div(Sub(Int(1), Int(2)), Int(3)))
	}
	it should "異分母" in {
		// 1/2-3/4=(1*4-3*2)/(2*4)=(4-6)/8=-2/8=-1/4
		assert(
			Sub(Rational(Int(1), Int(2)), Rational(Int(3), Int(4))).result ===
			Div(Sub(Mul(Int(1), Int(4)), Mul(Int(3), Int(2))), Mul(Int(2), Int(4))))
	}
	"*" should "整数" in {
		// 1/2*3=1*3/2=3/2
		assert(Mul(Rational(Int(1), Int(2)), Int(3)).result === Div(Mul(Int(1), Int(3)), Int(2)))
		// 1*(2/3)=1*2/3=2/3
		assert(Mul(Int(1), Rational(Int(2), Int(3))).result === Div(Mul(Int(2), Int(1)), Int(3)))
	}
	it should "分数" in {
		// 1/2*(3/4)=1*3/(2*4)=3/8
		assert(
			Mul(Rational(Int(1), Int(2)), Rational(Int(3), Int(4))).result ===
			Div(Mul(Int(1), Int(3)), Mul(Int(2), Int(4))))
	}
	"/" should "整数" in {
		// 1/2/3=1/(2*3)=1/6
		assert(Div(Rational(Int(1), Int(2)), Int(3)).result === Div(Int(1), Mul(Int(2), Int(3))))
		// 1/(2/3)=(1*3)/2=3/2
		assert(Div(Int(1), Rational(Int(2), Int(3))).result === Div(Mul(Int(1), Int(3)), Int(2)))
	}
	it should "分数" in {
		// 1/2/(3/4)=1*4/(2*3)=4/6=2/3
		assert(
			Div(Rational(Int(1), Int(2)), Rational(Int(3), Int(4))).result ===
			Div(Mul(Int(1), Int(4)), Mul(Int(2), Int(3))))
	}
	"+-*/" should "Infを渡されたら" // case不足で警告が出るので省略
}
