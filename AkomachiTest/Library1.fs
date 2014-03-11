namespace AkomachiTest


open Microsoft.VisualStudio.TestTools.UnitTesting
open Akomachi;
open System
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

[<TestClass>]
type ParseTest() =
    [<TestMethod>]
    member this.TestNumber() =
        match (run Parser.numLiteral "123.456") with
            | Success (r,us,p)   -> 
                match r with
                    | Float x -> Assert.IsTrue(123.456 = x)
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
        match (run Parser.numLiteral "123") with
            | Success (r,us,p)   -> 
                match r with
                    | Int x -> Assert.IsTrue(123 = x)
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
        match (run Parser.numLiteral "0x123") with
            | Success (r,us,p)   -> 
                match r with
                    | Int x -> Assert.AreEqual(0x123, x) |> ignore
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
    [<TestMethod>]
    member this.TestString() =
        match (run Parser.strLiteral "\"test\"") with
            | Success (r,us,p)   -> 
                match r with
                    | String x -> Assert.AreEqual(x, "test") |> ignore
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
        match (run Parser.strLiteral "\"あいうえお\"") with
            | Success (r,us,p)   -> 
                match r with
                    | String x -> Assert.AreEqual(x, "あいうえお") |> ignore
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)

    [<TestMethod>]
    member this.TestBool() =
        match (run Parser.boolLiteral "true") with
            | Success (r,us,p)   -> 
                match r with
                    | Bool x -> Assert.AreEqual(x, true) |> ignore
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
        match (run Parser.boolLiteral "false") with
            | Success (r,us,p)   -> 
                match r with
                    | Bool x -> Assert.AreEqual(x, false) |> ignore
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
    [<TestMethod>]
    member this.TestAST() =
        match (run Parser.prog "   ( true     + 1    )  ") with
            | Success (r,us,p)   -> 
                match r with
                    | (Binary (Bool true, "+", Int 1)) -> ()
                    | x -> Assert.Fail(sprintf "%A" x)
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
        match (run Parser.prog "obj.method(1,2,3)") with
            | Success (r,us,p)   -> 
                match r with
                    | (Binary (Bool true, "+", Int 1)) -> ()
                    | x -> Assert.Fail(sprintf "%A" x)
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
