namespace Akomachi
open FParsec

module Sexp =
    let sexp, sexpImpl = createParserForwardedToRef()
    (*******************************************************************)
    (* Literals *)
    (*******************************************************************)
    let internal numberFormat = 
                           NumberLiteralOptions.AllowMinusSign
                       ||| NumberLiteralOptions.AllowFraction
                       ||| NumberLiteralOptions.AllowExponent
                       ||| NumberLiteralOptions.AllowHexadecimal
                       ||| NumberLiteralOptions.AllowInfinity
    let numLiteral =
        numberLiteral numberFormat "number"
        |>> fun p ->
                if p.IsInteger then (int p.String) :> obj
                else (float p.String) :> obj
    // These literals are from FParsec JSON samples.
    // Copyright (c) Stephan Tolksdorf 2008-2011
    // License: Simplified BSD License. See accompanying documentation.
    let internal str s = pstring s
    let strLiteral =
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
        |>> (fun x -> x :> obj)
    let boolLiteral
        = (stringReturn "true" true <|> stringReturn "false" false) |>> (fun x -> x :> obj)
    let internal ws = spaces
    let internal strws s = pstring s .>> ws
    let listLiteral = between (strws "(") (pstring ")") (sepEndBy sexp spaces1) |>> (fun x -> x :> obj)
    do sexpImpl := choice [ numLiteral; strLiteral; boolLiteral; listLiteral ]

    type ParseResult =
        | Success of obj
        | Error of string

    let parse (str:string) =
        match runParserOnString sexp str "body" str with
             | ParserResult.Success (ast,us,p)   -> ParseResult.Success ast
             | ParserResult.Failure (msg,err,us) -> ParseResult.Error (sprintf "Failed to parse: %s" msg)
    let internal encodeString (s:string) = s.Replace("\'", "\\\'").Replace("\b", "\\b").Replace("\f", "\\f").Replace("\n", "\\n").Replace("\r", "\\r").Replace("\t", "\\t")
    let rec toSexp (ex:obj) =
      let ty = if ex.GetType().IsGenericType then ex.GetType().GetGenericTypeDefinition() else ex.GetType()
      if      ty.Equals(typeof<int>)     then (string (ex :?> int))
      else if ty.Equals(typeof<float>)   then (string (ex :?> float))
      else if ty.Equals(typeof<bool>)    then (if (ex:?>bool) then "true" else "false")
      else if ty.Equals(typeof<string>)  then "\"" + encodeString (ex :?> string) + "\""
      else if ty.IsArray || ty.Equals(typedefof<_ []>) then ("("+(System.String.Join(" ", Array.map toSexp (ex :?> obj []))+")"))
      else if ty.Equals(typedefof<_ list>)    then ("("+(System.String.Join(" ", List.toArray (List.map toSexp (ex :?> _ list)))+")"))
      else if ty.Equals(typedefof<System.Collections.Generic.List<_>>)    then ("("+(System.String.Join(" ", Seq.toArray (Seq.map toSexp (ex :?> System.Collections.Generic.List<obj>))))+")")
      else raise (System.Exception("Parse err"))
