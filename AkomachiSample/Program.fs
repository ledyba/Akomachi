// F# の詳細については、http://fsharp.net を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。

open Akomachi
open Akomachi.Runtime
open Akomachi.Parser

[<EntryPoint>]
let main argv = 
    let akomachi = Akomachi.Akomachi();
    let  v = (Value.Int 1) = (Value.Int 1);
    match (akomachi.parse "global.z = fun(x,y,z) { x+y+z }; Math.sin(0);") with
      | Success r   ->
           match akomachi.dance r with
              | (Value.Float x) -> (x = 0.0) |> ignore
              | x -> raise (invalidOp (sprintf "%A" x))
      | Error msg -> raise (invalidOp (sprintf "Failed to parse: %s" msg)); Value.Null |> ignore
    let str = akomachi.save()
    let ako = Akomachi.Akomachi(str);
    match (ako.parse "z(1,2,3);") with
      | Success r   ->
           match ako.dance r with
              | (Value.Int 6) -> ()
              | x -> raise (invalidOp (sprintf "%A" x))
      | Error msg -> raise (invalidOp (sprintf "Failed to parse: %s" msg)); Value.Null |> ignore
    0 // 整数の終了コードを返します
