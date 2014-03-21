Akomachi
========

Very Simple .NET Language for embed

阿小町って、何？
==================
阿小町（あこまち）は、稲荷神　宇迦之御魂神（ウカノミタマノカミ）の眷属の狐の一匹です。伏見稲荷大社では、白虎社に祀られています。

トランス状態（狐憑き）になって神と交わるダンスをする巫女と同一視されてたそーです。

主な特徴、もといウリ
========================
 * 処理系のセーブデータを保存して、復帰できる
 * （エンジンの）ソースコードが短い
 * メモリ使用量が少ない
 * 遅くても仕方ない

文法 / Syntax
======
またこりもせずJSパクリ言語です。 This lang is similar to JS.

リテラル
---------

```
1; //int
1.0; //float
true; false; //bool
"Hey!" // string (JSONと一緒)
[1, 2, 3, "str"]; //list

// オブジェクト記法
var obj = {val1: 1, val2: true};
obj.val2; // --> true
```

すべては式
------------
```
// 演算子は中置記法
1+2;

// 変数は宣言が必要（初期化子は絶対必要）
var x = 1;

// if式
var res = if 1 == 1 then "t" else "f";
res; // --> "t"

// 複文を書く時はカッコで囲う
var res = if 1 == 1 then {var x=1; x+1;} else "f";
res; // --> 2
```
関数
-------
```
// 関数
var fn = fun (a,b,c) a+b+c;

// 複文を書く時はif同様カッコで囲う
var fn = fun (a,b,c) { a+b+c; 1+2; }
```

グローバル変数
-----------------
```
// グローバル変数を宣言するには、"global"に代入。
global.z = 1;
z; // --> 1
```

self
--------
```
var obj = { fn: fun (x, y, z) self.x, x: true };
obj.fn(1,2,3); // --> true つまり、selfの仕様はJSのthisと同じ

var fn = fun (x, y, z) self;
fn(1,2,3); // --> null このあたりも同じ
```

組み込み方 / How to embed
==========================

C#
------
ビルドして出来上がったAkomachi.dllとFShapCore.dllをreferencesに追加し、以下のサンプルコードを参考に組み込んでください。
````C#
namespace AkomachiRunCS
{
    class Sys
    {
        public void println(String src)
        {
            Console.WriteLine(src);
        }
        public void print(String src)
        {
            Console.Write(src);
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            Akomachi.Akomachi ako = new Akomachi.Akomachi();
            ako.setGlobalObject("System", (Object)new Sys());
            if(args.Length > 0) {
                System.IO.StreamReader stream = System.IO.File.OpenText (args[0]);
                String src = stream.ReadToEnd();
                Akomachi.Parser.Result parseResult = ako.parse(src);
                if( parseResult.IsSuccess ) {
                    Akomachi.Parser.Result.Success succ = (Akomachi.Parser.Result.Success)parseResult;
                    ako.dance(succ.Item);
					// or
					// Akomachi.Parser ast = ako.parseOrThrow(src);
					// ako.dance(ast);
					// この時、パースエラーの時は例外を投げます。
                }else{
                    Akomachi.Parser.Result.Error err = (Akomachi.Parser.Result.Error)parseResult;
                    Console.WriteLine("Failed to parse: {0}", err.Item);
                }
            }else{
                Console.WriteLine("Usage: {0} <src>" , System.AppDomain.CurrentDomain.FriendlyName);
            }
        }
    }
}
````

F#
------

ビルドして出来上がったAkomachi.dllをreferencesに追加し、以下のサンプルコードを参考に組み込んでください。

````F#
open Akomachi

type System()=
    member self.println(s:string) = printfn "%s" s
    member self.print(s:string) = printf "%s" s

[<EntryPoint>]
let main argv = 
    let akomachi = Akomachi.Akomachi();
    akomachi.setGlobalObject("System", new System() :> obj)
    if argv.Length > 0 then
        let stream = System.IO.File.OpenText (argv.[0])
        let src = stream.ReadToEnd()
        match akomachi.parse src with
            | Akomachi.Parser.Success ast ->
                let v = akomachi.dance ast
                0
            | Akomachi.Parser.Error err ->
                printfn "Failed to parse: %s" err
                -1
    else
        printfn "Usage: %s <src>"  System.AppDomain.CurrentDomain.FriendlyName
        0
````

セーブの仕方
------------------
<h3> C&#35</h3>
````
// セーブデータはStringです。
String savedata = ako.save();
// コンストラクタに指定すると、読み込みます。
Akomachi.Akomachi new_ako = new Akomachi.Akomachi(savedata);
// 以下続行
````

オブジェクトの登録
------------------
<h3> C&#35</h3>

````
ako.setGlobalObject("IntValue", 1);
ako.setGlobalObject("FloatValue", 1.0);
ako.setGlobalObject("BoolValue", true);
ako.setGlobalObject("StringValue", "string");
ako.setGlobalObject("NativeObjectValue", new YourClass());
````
登録したオブジェクトはグローバル変数として登録されます。
ネイティブオブジェクトを登録し、それをセーブデータに含める場合、public String Save()というメソッドが必要です。
