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
        let v =
          match (run Parser.prog "1+2+3") with
            | Success (r,us,p)   -> Stage.dance (Stage.World()) r
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg); Value.Null
        ()
