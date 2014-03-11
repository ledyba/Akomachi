Akomachi
========

Very Simple .NET Language for embed

阿小町って、何？
==================
阿小町（あこまち）は、稲荷神　宇迦之御魂神（ウカノミタマノカミ）の眷属の狐の一匹です。伏見稲荷大社では、白虎社に祀られています。

トランス状態（狐憑き）になって神と交わるダンスをする巫女と同一視されてたそーです。

文法
======
またこりもせずJSパクリ言語です。

リテラル
---------

```
// 演算子は中置記法
1+2;

// 変数は宣言が必要（初期化子は絶対必要）
var x = 1;

// オブジェクト記法
var obj = {val1 -> 1, val2 -> true};
obj.val2; // --> true

// if式
var res = if 1 == 1 then "t" else "f";
res; // --> "t"

// 複文を書く時はカッコで囲う
var res = if 1 == 1 then {val x=1; x+1;} else "f";
res; // --> 2

// 関数
var fn = fun (a,b,c) a+b+c;

// 複文を書く時はif同様カッコで囲う
var fn = fun (a,b,c) { a+b+c; 1+2; }

// 関数
var fn = fun (a,b,c) a+b+c;

// 複文を書く時はカッコで囲う
var fn = fun (a,b,c) { a+b+c; 1+2; }

// グローバル変数を宣言するには、"global"に代入。
global.z = 1;
z; // --> 1
```
