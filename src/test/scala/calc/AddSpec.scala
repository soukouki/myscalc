import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc._

class AddSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Intを2つ受け取り、足した数を返す" in {
		assert(Add(Int(1), Int(2)).result === Int(3))
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Add(Add(Int(1), Int(2)), Int(3)).result === Add(Int(3), Int(3)))
	}
	"string" should "数字の間に+をつけて返す" in {
		assert(Add(Int(1), Int(2)).string === "1+2")
	}
	it should "受け取ったのが式の場合は、間に+をつける" in {
		assert(Add(Add(Int(1), Int(2)), Int(3)).string === "1+2+3")
		assert(Add(Int(1), Mul(Int(2), Int(3))).string === "1+2*3")
	}
	it should "左側に*/が来たときは括弧を付ける" in {
		assert(Add(Mul(Int(1), Int(2)), Int(3)).string === "(1*2)+3")
	}
}