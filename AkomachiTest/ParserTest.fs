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
                    | [(Binary (Bool true, "+", Int 1))] -> ()
                    | x -> Assert.Fail(sprintf "%A" x)
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
        match (run Parser.prog "obj.method(1,2,3); obj = 1 + 10 ^ 10") with
            | Success (r,us,p)   -> 
                match r with
                    | [Call (Access ((Ident "obj"), "method"), [Int 1; Int 2; Int 3]); Assign (Ident "obj", Binary (Binary (Int 1, "+", Int 10), "^", Int 10)) ] -> ()
                    | x -> Assert.Fail(sprintf "%A" x)
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
    [<TestMethod>]
    member this.TestFunc() =
        match (run Parser.prog "fun (a,b, c) {1+2+z;}") with
            | Success (r,us,p)   -> 
                match r with
                    | [Fun (["a"; "b"; "c"],Block [Binary (Binary (Int 1,"+",Int 2),"+",Ident "z")])] -> ()
                    | x -> Assert.Fail(sprintf "%A" x)
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
        match (run Parser.prog "var fn = fun (a,b, c) {1+2+z;}") with
            | Success (r,us,p)   -> 
                match r with
                    | [Var ("fn", Fun (["a"; "b"; "c"],Block [Binary (Binary (Int 1,"+",Int 2),"+",Ident "z")]))] -> ()
                    | x -> Assert.Fail(sprintf "%A" x)
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
    [<TestMethod>]
    member this.TestFunCall() =
        match (run Parser.prog "obj(1,2,3,4,true);") with
            | Success (r,us,p)   -> 
                match r with
                    | [Call (Ident "obj",[Int 1; Int 2; Int 3; Int 4; Bool true])] -> ()
                    | x -> Assert.Fail(sprintf "%A" x)
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
    [<TestMethod>]
    member this.TestObject() =
        match (run Parser.prog "obj(1,2,3,4,true);{obj -> 1, obj2 -> 2}") with
            | Success (r,us,p)   -> 
                match r with
                    | [Call (Ident "obj",[Int 1; Int 2; Int 3; Int 4; Bool true]); Object [("obj", Int 1); ("obj2", Int 2)]] -> ()
                    | x -> Assert.Fail(sprintf "%A" x)
            | Failure (msg,err,us) -> Assert.Fail(sprintf "Failed to parse: %s" msg)
