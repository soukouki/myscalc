import org.scalatest.{FlatSpec, DiagrammedAssertions}
import myscalc.calc.{Add, Int}

class AddSpec extends FlatSpec with DiagrammedAssertions {
	"result" should "Intを2つ受け取り、足した数を返す" in {
		assert(Add(Int(1), Int(2)).result === Int(3))
	}
	it should "式を受け取ると、先に計算する方だけを計算し、返す" in {
		assert(Add(Add(Int(1), Int(2)), Int(3)).result === Add(Int(3), Int(3)))
	}
}
