namespace Akomachi

open System
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

module Parser =
    let internal numberFormat = 
                           NumberLiteralOptions.AllowMinusSign
                       ||| NumberLiteralOptions.AllowFraction
                       ||| NumberLiteralOptions.AllowExponent
                       ||| NumberLiteralOptions.AllowHexadecimal
                       ||| NumberLiteralOptions.AllowInfinity
    let numLiteral : Parser<AST, unit> =
        numberLiteral numberFormat "number"
        |>> fun p ->
                if p.IsInteger then Int (int64 p.String)
                else Float (float p.String)
    let strLiteral : Parser<AST, unit> = pstring