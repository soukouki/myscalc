import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.num._
import myscalc.calc.operator._

class DecimalSpec extends FlatSpec with DiagrammedAssertions with CalcTestUtility {
	"hasFinished" should "常にtrue" in {
		assert(Decimal(Int(12), Int(-1)).hasFinished === true)
	}
	"string" should "" in {
		assert(Decimal(Int(123), Int(-2)).string === "1.23")
		assert(Decimal(Int(123), Int(-1)).string === "12.3")
		assert(Decimal(Int(-12), Int(-1)).string === "-1.2")
	}
	it should "キリの良い小数" in {
		assert(Decimal(Int(10), Int(-1)).string === "1.0")
		assert(Decimal(Int(0), Int(-1)).string === "0.0")
		assert(Decimal(Int(-20), Int(-1)).string === "-2.0")
	}
	it should "1以下の小数(0も除く)" in {
		assert(Decimal(Int(2), Int(-2)).string === "0.02")
		assert(Decimal(Int(-2), Int(-1)).string === "-0.2")
	}
	"+" should "整数" in {
		assert(p("0.01+3").advance === p("3.01"))
		assert(p("1+0.2").advance === p("1.2"))
	}
	it should "分数" in {
		assert(p("0.1+2/3").advance === p("1/10+2/3"))
		assert(p("1/2+3.4").advance === p("1/2+34/10"))
	}
	it should "小数" in {
		assert(p("0.1+0.2").advance === p("0.3"))
		assert(p("0.01+0.2").advance === p("0.21"))
	}
	"-" should "整数" in {
		assert(p("0.3-4").advance === p("-3.7"))
		assert(p("1-2.3").advance === p("-1.3"))
	}
	it should "分数" in {
		assert(p("0.1-2/3").advance === p("1/10-2/3"))
		assert(p("1/2-0.01").advance === p("1/2-1/100"))
	}
	it should "小数" in {
		assert(p("0.1-0.03").advance === p("0.07"))
	}
	"*" should "整数" in {
		assert(p("0.3*3").advance === p("0.9"))
		assert(p("2*0.1").advance === p("0.2"))
	}
	it should "分数" in {
		assert(p("0.3*(1/2)").advance === p("3/10*(1/2)"))
		assert(p("1/2*0.3").advance === p("1/2*(3/10)"))
	}
	it should "小数" in {
		assert(p("0.1*0.03").advance === p("1/10*(3/100)"))
	}
	"/" should "整数" in {
		assert(p("0.1/2").advance === p("1/10/2"))
		assert(p("2/0.3").advance === p("2/(3/10)"))
	}
	it should "分数" in {
		assert(p("0.1/(2/3)").advance === p("1/10/(2/3)"))
		assert(p("1/2/0.3").advance === p("1/2/(3/10)"))
	}
	it should "小数" in {
		assert(p("0.1/0.3").advance === p("1/10/(3/10)"))
	}
	"+-*/" should "Inf" // case不足で警告が出るので省略
}
