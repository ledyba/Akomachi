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
    [<TestMethod>]
    member this.TestIf() =
      match (run Parser.prog "var z = fun (x, y) x+y; if z(1,2) == 3 then 1 else 2") with
        | Success (r,us,p)   ->
             match Stage.dance (Stage.World()) r with
                | (Value.Int 1) -> ()
                | x -> Assert.Fail(sprintf "%A" x)
        | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg); Value.Null |> ignore
      
      ()
    [<TestMethod>]
    member this.TestObj() =
      match (run Parser.prog "var obj = {v->1, b->true}; if obj.b then 1 else 2") with
        | Success (r,us,p)   ->
             match Stage.dance (Stage.World()) r with
                | (Value.Int 1) -> ()
                | x -> Assert.Fail(sprintf "%A" x)
        | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg); Value.Null |> ignore
      
      ()
    [<TestMethod>]
    member this.TestTak() =
      (*
      match (run Parser.prog "var t = fun (x, y, z) if x <= y then y else t(t(x-1,y,z), t(y-1,z,x), t(z-1,x,y)); t(12,6,0)") with
        | Success (r,us,p)   ->
             match Stage.dance (Stage.World()) r with
                | (Value.Int 12) -> ()
                | x -> Assert.Fail(sprintf "%A" x)
        | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg); Value.Null |> ignore
      *)
      ()
    [<TestMethod>]
    member this.TestSelf() =
      match (run Parser.prog "var obj = { fn -> fun (x, y, z) self.x, x->1 }; obj.fn(1,2,3);") with
        | Success (r,us,p)   ->
             match Stage.dance (Stage.World()) r with
                | (Value.Int 1) -> ()
                | x -> Assert.Fail(sprintf "%A" x)
        | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg); Value.Null |> ignore
      ()


