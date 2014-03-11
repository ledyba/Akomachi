namespace AkomachiTest

open Microsoft.VisualStudio.TestTools.UnitTesting
open Akomachi;
open System
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

[<TestClass>]
type EvalTest() =
    [<TestMethod>]
    member this.TestEval() =
      match (run Parser.prog "1+2+3") with
        | Success (r,us,p)   ->
             match Stage.dance (Stage.World()) r with
                | (Value.Int 6) -> ()
                | x -> Assert.Fail(sprintf "%A" x)
        | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg); Value.Null |> ignore
    [<TestMethod>]
    member this.TestFunc() =
      match (run Parser.prog "var z = fun (x, y) x+y; z(1,2)") with
        | Success (r,us,p)   ->
             match Stage.dance (Stage.World()) r with
                | (Value.Int 3) -> ()
                | x -> Assert.Fail(sprintf "%A" x)
        | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg); Value.Null |> ignore
      
      ()