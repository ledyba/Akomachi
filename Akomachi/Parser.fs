﻿namespace Akomachi

open System
open FParsec

#nowarn "40"
module Parser =
    (*******************************************************************)
    (* Literals *)
    (*******************************************************************)
    let internal numberFormat = 
                           NumberLiteralOptions.AllowMinusSign
                       ||| NumberLiteralOptions.AllowFraction
                       ||| NumberLiteralOptions.AllowExponent
                       ||| NumberLiteralOptions.AllowHexadecimal
                       ||| NumberLiteralOptions.AllowInfinity
    let numLiteral : Parser<AST, unit> =
        numberLiteral numberFormat "number"
        |>> fun p ->
                if p.IsInteger then Int (int p.String)
                else Float (float p.String)
    // These literals are from FParsec JSON samples.
    // Copyright (c) Stephan Tolksdorf 2008-2011
    // License: Simplified BSD License. See accompanying documentation.
    let internal str s = pstring s
    let strLiteral : Parser<AST, unit> =
        let escape =  anyOf "\"\\/bfnrt"
                      |>> function
                          | 'b' -> "\b"
                          | 'f' -> "\u000C"
                          | 'n' -> "\n"
                          | 'r' -> "\r"
                          | 't' -> "\t"
                          | c   -> string c // every other char is mapped to itself

        let unicodeEscape =
            str "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                let hex2int c = (int c &&& 15) + (int c >>> 6)*9 // hex char to int
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
            )

        between (str "\"") (str "\"")
                (stringsSepBy (manySatisfy (fun c -> c <> '"' && c <> '\\'))
                              (str "\\" >>. (escape <|> unicodeEscape)))
        |>> AST.String
    let boolLiteral : Parser<AST, unit>
        = (stringReturn "true" true <|> stringReturn "false" false) |>> AST.Bool
    let internal isAsciiIdStart c    = isAsciiLetter c
    let internal isAsciiIdContinue c = isAsciiLetter c || isDigit c
    let internal optsWithPreCheck = IdentifierOptions(
                                     isAsciiIdStart = isAsciiIdStart,
                                     isAsciiIdContinue = isAsciiIdContinue,
                                     allowAllNonAsciiCharsInPreCheck = true,
                                     normalization = System.Text.NormalizationForm.FormKC,
                                     normalizeBeforeValidation = true)
    let internal ident : Parser<string, unit> = (identifier optsWithPreCheck)
    let identLiteral : Parser<AST, unit> = ident |>> AST.Name
    (*******************************************************************)
    (* Syntax *)
    (*******************************************************************)
    let internal ws = spaces
    let internal strws s = pstring s .>> ws
    let expr, exprImpl = createParserForwardedToRef()
    let internal callOp = between (strws "(") (strws ")") (sepBy expr (strws ","))
    let internal indexOp = between (strws "[") (strws "]") expr
    let internal accessOp = (strws ".") >>. ident
    let primary : Parser<AST, unit> =
            numLiteral <|>
            strLiteral <|>
            boolLiteral <|>
            (ident |>> AST.Ident)
    let postFixBase =
            primary >>= fun r ->
                choice [
                    callOp |>> fun x -> AST.Call (r,x);
                    indexOp |>> fun x -> AST.Index (r,x);
                    accessOp |>> fun x -> AST.Access (r, x)
                    preturn r
                ]
    let postFix =
            postFixBase >>= fun r ->
                (callOp |>> fun x -> AST.Call (r,x)) <|> (indexOp |>> fun x -> AST.Index (r,x)) <|> preturn r
    let opp = new FParsec.OperatorPrecedenceParser<AST,unit,unit>()
    let expr1 = opp.ExpressionParser
    opp.TermParser <- (postFix.>> ws) <|> between (strws "(") (strws ")") expr
    opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> Binary (x, "+", y)))
    opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> Binary (x, "-", y)))
    opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun x y -> Binary (x, "*", y)))
    opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun x y -> Binary (x, "/", y)))
    opp.AddOperator(InfixOperator("%", ws, 2, Associativity.Left, fun x y -> Binary (x, "%", y)))
    opp.AddOperator(InfixOperator("^", ws, 3, Associativity.Right, fun x y -> Binary (x, "^", y)))
    opp.AddOperator(PrefixOperator("-", ws, 4, true, fun x -> Uni ("-", x)))
    opp.AddOperator(PrefixOperator("+", ws, 4, true, fun x -> Uni ("+", x)))
    opp.AddOperator(PrefixOperator("!", ws, 4, true, fun x -> Uni ("!", x)))
    opp.AddOperator(PrefixOperator("~", ws, 4, true, fun x -> Uni ("~", x)))
    opp.AddOperator(InfixOperator("=", ws, 10, Associativity.Right, fun x y -> Assign (x,y)))
    let expr2 = (between (strws "[") (strws "]") (sepBy expr (strws ",")) |>> AST.List) <|> expr1
    let exprs = sepBy expr (strws ";")
    let block = between (strws "{") (strws "}") exprs
    let expr3 = (((strws "fun") >>. ( between (strws "(") (strws ")") (sepBy ident (strws ",")) ) .>>. block) |>> AST.Fun) <|> expr2
    do exprImpl := expr3
    let prog = ws >>. exprs .>> eof
