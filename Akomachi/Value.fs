namespace Akomachi

open FParsec

type Value =
      Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Obj    of AkObj
    | Fun    of AkObj * string list * AST
    | NativeObj  of NativeObject
    | NativeFunc of NativeFunc
    | Null
and AkObj = System.Collections.Generic.Dictionary<string, Value>
and NativeFunc = (Value -> Value list -> Value)
and NativeObject =
    abstract member ToString : string
    abstract member Get : string -> Value
    abstract member Set : string -> Value -> Value
    abstract member Index : Value -> Value
    abstract member IndexSet : Value -> Value -> Value
and Provider<'a> =
    abstract member ToString : 'a -> string
    abstract member Get : 'a -> string -> Value
    abstract member Set : 'a -> string -> Value -> Value
    abstract member Index : 'a -> Value -> Value
    abstract member IndexSet : 'a -> Value -> Value -> Value

module Val =
    let val2int v = match v with
        | Int i -> i
        | Float i -> (int i)
        | _ -> raise (invalidOp (sprintf "%A cannot to be converted to int" v))
