
# myscalc

途中式を表示するやつ。

## 使える式
- `10` 自然数
- `-3` 負の整数
- `1.2` 小数
- `(-5)` 括弧（構文解析する際に優先順位を変える）
- `1+1` 加法
- `2-3` 減法
- `3*-5` 乗法
- `4/2` 除法
	- 割り切れないときは分数になります
- `2/3` 分数
	- 約分は1を除いた最小公約数になります
		- `6/12` => `3/6` => `1/2`
- `1.2` 小数
- `1.2(34)` 循環小数
	- 例は`1.2343434...`と同じ
- 特殊なリテラル
	- 変更される恐れが高い
	- `Inf`

## バグ

## 追加したい
- 論理式
- 分数から小数に変換する構文を実装
- 計算を省略するオプションを付ける
	- 例えば、分数の細かい計算は方程式を方程式を解いたりするのには不要
- `-(2+3)`などの式を動かせるように
- 関数の追加
- 変数の追加
- 循環小数の途中式をもっとよく

## バージョン情報
- v1.0._
	- v1.0.0
		- 整数の四則演算だけ
	- v1.0.1
		- 括弧関係の修正
	- v1.0.2
		- 分数の表示
	- v1.0.3
		- 分数の約分
- v1.1._
	- v1.1.0
		- 分数の計算
		- 同じ文字列が続いたときに連続で表示しないように
	- v1.1.1
		- 分数の分母側がマイナスのときに分子側に寄せるように
	- v1.1.2
		- 約分時に少しずつ割っていくように
	- v1.1.3
		- Readmeを修正
	- v1.1.4
		- 大きな数か使えるように
	- v1.1.5
		- 割り切れる数でも分数にして、少しずつ割っていくように
		- 配布ファイルzip化
	- v1.1.6
		- パースエラー時の表示を作成
		- 約分せずに計算するのを修正
		- 整数-分数のバグを修正
- v1.2._
	- v1.2.0
		- 小数の実装
	- v1.2.1
		- 小数と整数の加法減法、小数同士の加法減法、分数と整数の乗法の途中式を変更
		- `1+-2`や`1--2`に括弧を付けて、`1+(-2)`や`1-(-2)`になるように
- v1.3._
	- v1.3.0
		- 循環小数を実装
	- v1.3.1
		- 小数で最後にドットがつくバグを修正
		- 0や1で割ったり掛けたりするときの途中式を省略するように
	- v1.3.2
		- マイナスの循環小数を分数に直すときのバグを修正
	- v1.3.3
		- `Inf`のリテラルを追加
