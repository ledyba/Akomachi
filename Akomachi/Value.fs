namespace Akomachi

type Value =
      Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Obj    of AkObj
    | Fun    of AkFun
    | NativeObject  of obj
    | NativeFunc of (System.Type * string)
    | Null
and AkFun = AkObj * string list * AST * string
and AkObj = System.Collections.Generic.Dictionary<string, Value>
and AkNativeFunc = (Value list -> Value)
