namespace Akomachi

open FParsec
type Value =
      Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Obj    of AkObj
    | Fun    of Value * AST
    | Native of NativeObject
    | Null
and AkObj = System.Collections.Generic.Dictionary<string, Value>
and NativeObject =
    abstract ToString : string
    abstract Get : string -> Value
    abstract Set : string -> Value -> Value
    abstract Index : Value -> Value
    abstract IndexSet : Value -> Value -> Value
    abstract Call : Value list -> Value
and Provider<'a> =
    abstract ToString : 'a -> string
    abstract Get : 'a -> string -> Value
    abstract Set : 'a -> string -> Value -> Value
    abstract Index : 'a -> Value -> Value
    abstract IndexSet : 'a -> Value -> Value -> Value
    abstract Call : 'a -> Value list -> Value

module Stage =
    type World ()=
        member self.Global = new AkObj()
        member internal self.intProvicer = new System.Collections.Generic.Dictionary<string, NativeObject>()
        member internal self.floatProvicer = new System.Collections.Generic.Dictionary<string, NativeObject>()
        member internal self.boolProvicer = new System.Collections.Generic.Dictionary<string, NativeObject>()
        member internal self.objProvicer = new System.Collections.Generic.Dictionary<string, NativeObject>()
    let internal get (w:World) (obj:Value) (name:string):Value =
        match obj with
            | Int    i -> Null
            | Float  f -> Null
            | Bool   b -> Null
            | String s -> Null
            | Obj    obj -> Null
            | Fun    (env, body) -> Null
            | Native obj -> Null
    let rec dance_ (w:World) (callStack:AkObj list) (stack:AkObj list) (src:AST) : Value =
        match src with
            | AST.Int x -> Int x
            | AST.Float x -> Float x
            | AST.Bool x -> Bool x
            | AST.String x -> String x
            | AST.Null -> Null
            | AST.Object elements -> Null
            | AST.List exprs -> Null
            | AST.Block exprs -> Null
            | AST.Ident name -> Null

            | AST.Access (value, name) -> Null
            | AST.Index (value, index) -> Null
            | AST.Call (value, args) -> Null

            | AST.Uni (sym, value) -> Null
            | AST.Binary (val1, sym, val2) -> Null
            | AST.Assign (val1, val2) -> Null

            | AST.Fun (args, exprs) -> Null
            | AST.Var (name, expr) -> Null
    let dance (w:World) (src:AST) = dance_ w [w.Global] [] src
(*
type Stage() =
    let 
    let globalObject = new System.Collections.Generic.Dictionary<string, Value>()
    member self.regNative (name:string) (load:(string -> NativeObject)) (save:(NativeObject -> string)) = ()
    member self.parse = run Parser.prog
    member self.dance (src:AST) = ()
*)