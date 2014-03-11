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
            | Failure (msg,err,us) -> Assert.Fail()
        match (run Parser.numLiteral "123") with
            | Success (r,us,p)   -> 
                match r with
                    | Int x -> Assert.IsTrue((int64 123) = x)
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail()
        match (run Parser.numLiteral "0x123") with
            | Success (r,us,p)   -> 
                match r with
                    | Int x -> Assert.AreEqual((int64 0x123), x) |> ignore
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail()
    [<TestMethod>]
    member this.TestString() =
        match (run Parser.strLiteral "\"test\"") with
            | Success (r,us,p)   -> 
                match r with
                    | String x -> Assert.AreEqual(x, "test") |> ignore
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail()
        match (run Parser.strLiteral "\"あいうえお\"") with
            | Success (r,us,p)   -> 
                match r with
                    | String x -> Assert.AreEqual(x, "あいうえお") |> ignore
                    | _ -> Assert.Fail()
            | Failure (msg,err,us) -> Assert.Fail()

