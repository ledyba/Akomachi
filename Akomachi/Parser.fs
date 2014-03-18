namespace Akomachi

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
    let identLiteral : Parser<AST, unit> = ident |>> AST.Ident
    let nullLiteral = stringReturn "null" AST.Null
    let selfLiteral = stringReturn "self" AST.Self
    (*******************************************************************)
    (* Syntax *)
    (*******************************************************************)
    let internal ws = spaces
    let internal strws s = pstring s .>> ws
    let expr, exprImpl = createParserForwardedToRef()
    let internal callOp = between (strws "(") (strws ")") (sepBy expr (strws ","))
    let internal indexOp = between (strws "[") (strws "]") expr
    let internal accessOp = (strws ".") >>. ident
    let exprs = sepEndBy expr (strws ";")
    let block = between (strws "{") (strws "}") exprs

    let internal func = ((strws "fun") >>. ( between (strws "(") (strws ")") (sepBy ident (strws ",")) ) .>>. expr) |>> AST.Fun
    let internal obj_ = between (strws "{") (strws "}") (sepBy (ident .>> ws .>> strws ":" .>>. expr ) (strws ",")) |>> AST.Object
    let internal list = between (strws "[") (strws "]") (sepBy expr (strws ",")) |>> AST.List
    let internal if_ = (tuple3 ((strws "if") >>. expr .>> (strws "then")) expr ((strws "else") >>. expr) ) |>> AST.If
    let internal for_ = (tuple4 ((strws "for") >>. (strws "(") >>. expr .>> (strws ";")) (expr .>> (strws ";")) expr ((strws ")") >>. expr)) |>> AST.Loop

    let primary : Parser<AST, unit> =
            choice
                [func;
                 attempt obj_;
                 block |>> AST.Block;
                 list;
                 if_;
                 for_;
                 numLiteral;
                 strLiteral;
                 boolLiteral;
                 nullLiteral;
                 selfLiteral;
                 identLiteral;
                 between (strws "(") (strws ")") expr
                 ]
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
    let expr0 = postFix .>> ws
    let opp = new FParsec.OperatorPrecedenceParser<AST,unit,unit>()
    let expr1 = opp.ExpressionParser
    opp.TermParser <- expr0
    opp.AddOperator(InfixOperator("=", ws, 1, Associativity.Right, fun x y -> Assign (x,y)))

    opp.AddOperator(InfixOperator("^",  ws, 2, Associativity.Right, fun x y -> Binary (x, "opPow", y)))

    opp.AddOperator(InfixOperator("==", ws, 3, Associativity.Right, fun x y -> Binary (x, "opEq", y)))
    opp.AddOperator(InfixOperator("!=", ws, 3, Associativity.Right, fun x y -> Binary (x, "opNe", y)))

    opp.AddOperator(InfixOperator(">",  ws, 4, Associativity.Right, fun x y -> Binary (x, "opGt", y)))
    opp.AddOperator(InfixOperator(">=", ws, 4, Associativity.Right, fun x y -> Binary (x, "opGe", y)))
    opp.AddOperator(InfixOperator("<=", ws, 4, Associativity.Right, fun x y -> Binary (x, "opLe", y)))
    opp.AddOperator(InfixOperator("<",  ws, 4, Associativity.Right, fun x y -> Binary (x, "opLt", y)))

    opp.AddOperator(InfixOperator("+", ws, 5, Associativity.Left, fun x y -> Binary (x, "opAdd", y)))
    opp.AddOperator(InfixOperator("-", ws, 5, Associativity.Left, fun x y -> Binary (x, "opSub", y)))

    opp.AddOperator(InfixOperator("*", ws, 6, Associativity.Left, fun x y -> Binary (x, "opMul", y)))
    opp.AddOperator(InfixOperator("/", ws, 6, Associativity.Left, fun x y -> Binary (x, "opDiv", y)))
    opp.AddOperator(InfixOperator("%", ws, 6, Associativity.Left, fun x y -> Binary (x, "opMod", y)))

    opp.AddOperator(PrefixOperator("-", ws, 7, true, fun x -> Uni ("opMinus", x)))
    opp.AddOperator(PrefixOperator("+", ws, 7, true, fun x -> Uni ("opPlus", x)))
    opp.AddOperator(PrefixOperator("!", ws, 7, true, fun x -> Uni ("opNot", x)))
    opp.AddOperator(PrefixOperator("~", ws, 7, true, fun x -> Uni ("opComplement", x)))
    let expr2 = ( ( ((strws "var") >>. ident .>> (ws >>. strws "=")) .>>. expr1) |>> AST.Var) <|> expr1
                //expr2 >>= fun x -> ( (((preturn x .>> ws .>> strws "=") .>>. expr2) |>> AST.Assign ) <|> preturn x )
    do exprImpl := expr2
    let prog = (ws >>. exprs .>> eof) |>> fun e -> AST.Fun ([], Block e)
