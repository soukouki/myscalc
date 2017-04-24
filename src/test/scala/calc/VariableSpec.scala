
import myscalc.Parse

import myscalc.calc.error.SubstitutionError
import myscalc.calc.num.Int
import myscalc.calc.Variable
import myscalc.calc.Equal
import myscalc.calc.Undef

import myscalc.variables.Variables
import myscalc.variables.CharKey

// 変数関係のテストはとりあえずまとめる
class VariableSpec extends FlatCalcTest {
	"parse" should "とりま初期値" in {
		assert(p("x") === Variable(CharKey('x')))
		assert(p("a") === Variable(CharKey('a')))
	}
	"hasFinished" should "Variablesにあるときはtrue" in {
		assert(p("x").hasFinished(Variables(Map(CharKey('x') -> Int(0)))) === false)
	}
	it should "Variablesにないときはfalse" in {
		assert(p("x").hasFinished(defaultVariables) === true)
	}
	"string" should "keyだけ" in {
		assert(Variable(CharKey('x')).string === "x")
	}
	"Variableの値になれる" should "" in {
		assert(advance(p("x"), Variables(Map(CharKey('x') -> Int(3)))) === Int(3))
	}
}

class UndefSpec extends FlatCalcTest {
	"parse" should "" in {
		assert(p("Undef") === Undef())
	}
	"string" should "" in {
		assert(Undef().string === "Undef")
	}
}

class EqualSpec extends FlatCalcTest {
	"parse" should "変数=式だけで" in {
		assert(p("x=1") === Equal(Variable(CharKey('x')), Int(1)))
	}
	it should "その他はエラー" in {
		assert(Parse("1=1").isLeft)
		assert(Parse("x=y=2").isLeft)
		assert(Parse("x+2=3").isLeft)
		assert(Parse("x=(x=3)").isLeft)
	}
	"string" should "" in {
		assert(Equal(Variable(CharKey('a')), Int(4)).string === "a=4")
	}
	/**
		数値のほうを返すとかにすると、x=yとかやられたときにyでxでyで・・・ってなるのでUndefに
	*/
	"#advance" should "Undefを返す" in {
		assert(advance(p("a=0")) === Undef())
	}
	it should "Variablesに代入できる" in {
		assert(p("x=2").advance(defaultVariables) === (Undef(), Variables(Map(CharKey('x') -> Int(2)))))
	}
	it should "Variablesにもうある場合、値が同じならそのまま行く" in {
		assert(
			p("x=2").advance(Variables(Map(CharKey('x') -> Int(2))))
			=== (Undef(), Variables(Map(CharKey('x') -> Int(2)))))
	}
	it should "Variablesにもうある場合、違ったらエラーを出す" in {
		assert(
			p("x=2").advance(Variables(Map(CharKey('x') -> Int(3))))
			=== (SubstitutionError(), Variables(Map(CharKey('x') -> Int(3)))))
	}
}
