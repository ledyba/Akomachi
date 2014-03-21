// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

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
                0 // 整数の終了コードを返します
            | Akomachi.Parser.Error err ->
                printfn "Failed to parse: %s" err
                -1
    else
        printfn "Usage: %s <src>"  System.AppDomain.CurrentDomain.FriendlyName
        0
