
# myscalc

途中式を表示するやつ。

## 使える式
- `10` 自然数
- `-3` 負の整数
- `(-5)` 括弧（構文解析する際に優先順位を変える）
- `1+1` 加法
- `2-3` 減法
- `3*-5` 乗法
- `4/2` 除法
	- 結果は分数か整数になります
- `2/3` 分数
	- 約分は1を除いた最小公約数になります
		- `6/12` => `3/6` => `1/2`

## この先
- 同じ式(文字)が出てきたときに統一するようにする
- 小数
- 大きい数(内部がInt型なので)
- 論理式
