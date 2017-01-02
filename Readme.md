
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
	- 結果は既約とは限らない分数になります
- `2/3` 分数
	- 約分は1を除いた最小公約数になります
		- `6/12` => `3/6` => `1/2`

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


## これから
- 論理式
- 1+-2や1--2に括弧を付けて、1+(-2)や1-(-2)になるように
- 小数の式の改良
- 循環小数を実装
- 分数から小数に変換する構文を実装
