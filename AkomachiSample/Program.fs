﻿// F# の詳細については、http://fsharp.net を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。

open Akomachi
open FParsec

[<EntryPoint>]
let main argv = 
    let akomachi = Akomachi.Stage.Akomachi();
    let  v = (Akomachi.Value.Int 1) = (Akomachi.Value.Int 1);
    match (Akomachi.Parser.run "global.z = fun(x,y,z) { x+y+z }; Math.sin(0);") with
      | Success (r,us,p)   ->
           match akomachi.dance r with
              | (Value.Float x) -> (x = 0.0) |> ignore
              | x -> raise (invalidOp (sprintf "%A" x))
      | Failure (msg,err,us) -> raise (invalidOp (sprintf "Failed to parse: %s" msg)); Value.Null |> ignore
    printfn "%A" argv
    let str = akomachi.save()
    let ako = Akomachi.Stage.Akomachi(str);
    match (Akomachi.Parser.run "z(1,2,3);") with
      | Success (r,us,p)   ->
           match ako.dance r with
              | (Value.Int 6) -> ()
              | x -> raise (invalidOp (sprintf "%A" x))
      | Failure (msg,err,us) -> raise (invalidOp (sprintf "Failed to parse: %s" msg)); Value.Null |> ignore
    0 // 整数の終了コードを返します
