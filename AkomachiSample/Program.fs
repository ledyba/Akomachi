// F# の詳細については、http://fsharp.net を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。

open Akomachi
open FParsec

[<EntryPoint>]
let main argv = 
    let akomachi = Akomachi.Stage.Akomachi();
    match (FParsec.CharParsers.run Akomachi.Parser.prog "Math.sin(0)") with
      | Success (r,us,p)   ->
           match Akomachi().dance r with
              | (Value.Float x) -> (x = 0.0) |> ignore
              | x -> raise (invalidOp (sprintf "%A" x))
      | Failure (msg,err,us) -> raise (invalidOp (sprintf "Failed to parse: %s" msg)); Value.Null |> ignore
    printfn "%A" argv
    let str = akomachi.save()
    0 // 整数の終了コードを返します
