namespace Akomachi

type Value =
      Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Obj    of AkObj
    | Fun    of AkFun
    | NativeObject  of AkNativeObject
    | NativeFunc of (System.Type * string)
    | Null
and AkFun = AkObj * string list * AST
and AkObj = System.Collections.Generic.Dictionary<string, Value>
and AkNativeFunc = (Value list -> Value)
and AkNativeObject = interface
    abstract member ToString : string
    abstract member Get : string -> Value
    abstract member Set : string -> Value -> Value
    abstract member Save : string
    end
;;