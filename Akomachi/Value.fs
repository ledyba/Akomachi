namespace Akomachi

open FParsec

type Value =
      Int    of int
    | Float  of float
    | Bool   of bool
    | String of string
    | Obj    of AkObj
    | Fun    of AkFun
    | NativeObject  of AkNativeObject
    | NativeFunc of AkNativeFunc
    | Null
and AkFun = AkObj * string list * AST
and AkObj = System.Collections.Generic.Dictionary<string, Value>
and AkNativeFunc = (Value list -> Value)
and AkNativeObject =
    abstract member ToString : string
    abstract member Get : string -> Value
    abstract member Set : string -> Value -> Value

[<AutoOpen>]
module ValueHelper =
    let val2string (obj:Value) :string =
        match obj with
            | Int    i -> (string i)
            | Float  f -> (string f)
            | Bool   b -> (string b)
            | String s -> s
            | Obj    obj -> (sprintf "<<Pure Obj: %A>>" obj)
            | Fun    (env, arglist, body) -> (sprintf "Fun %A -> %A" arglist body)
            | NativeObject x -> sprintf "<<Native Object: %A>>" x
            | NativeFunc f -> sprintf "<<Native Fun>>: %A" f
            | Null -> "<<null>>"
    let val2int v =
      match v with
        | Int i -> i
        | Float i -> (int i)
        | _ -> raise (invalidOp (sprintf "%s cannot to be converted to int" (val2string v)))
    let val2bool v =
      match v with
        | Bool i -> i
        | _ -> raise (invalidOp (sprintf "%s cannot to be converted to bool" (val2string v)))
    let val2float v =
      match v with
        | Int i -> (float i)
        | Float i -> (i)
        | _ -> raise (invalidOp (sprintf "%s cannot to be converted to float" (val2string v)))
    let inline unboxFun< 'T > : Value -> 'T =
        let t = typeof<('T)>
        if      t.Equals(typeof<int>)   then unbox<Value->'T> (box val2int)
        else if t.Equals(typeof<float>) then unbox<Value->'T> (box val2float)
        else if t.Equals(typeof<int>)   then unbox<Value->'T> (box val2string)
        else (raise (invalidOp "Unsupported"))
    let inline boxFun<'T> : 'T -> Value =
        let t = typeof<'T>;
        if       t.Equals(typeof<int>)   then (unbox<'T->Value>  (box Int))
        else if  t.Equals(typeof<float>) then (unbox<'T->Value>  (box Float))
        else if  t.Equals(typeof<bool>)   then (unbox<'T->Value> (box Bool))
        else if  t.Equals(typeof<string>) then (unbox<'T->Value> (box String))
        else if  t.Equals(typeof<AkObj>) then (unbox<'T->Value>  (box Obj))
        else if  t.IsInstanceOfType(typeof<AkNativeObject>) then (unbox<'T->Value>  (box NativeObject))
        else raise (invalidOp (sprintf "Unsupported type: %s" (t.ToString())))
type Provider<'a>(description:string) =
    let dic = new System.Collections.Generic.Dictionary<string, Value>()
    member self.ToString  = sprintf "Provider of %s" ((typeof<'a>).ToString())
    member self.Get (name:string) = if dic.ContainsKey name then dic.Item name else Null
    member self.Set (name:string) (value:Value) =
        dic.Remove name |> ignore
        dic.Add (name,value)
    member self.regFunList (name:string, f: 'a list -> 'b ) : unit =
        let castF = unboxFun<'a>
        let boxF = boxFun<'b>
        let nf:AkNativeFunc = fun xs -> boxF (f (List.map castF xs))
        self.Set name (NativeFunc nf) |> ignore
    member self.regFun (name:string, (f: 'a -> 'b)) : unit=
        self.regFunList (name,
            fun args->
                match args with
                    | a :: _ -> (f a)
                    | _ -> raise (invalidOp ("You need two arguments for " + name))
        )
    member self.regFun (name:string, (f: 'a -> ' b -> 'c)) : unit=
        let castFA = unboxFun<'a>
        let castFB = unboxFun<'b>
        let boxF = boxFun<'c>
        let nf:AkNativeFunc =
            fun xs ->
                match xs with
                    | a :: b :: _ -> boxF (f (castFA a) (castFB b))
                    | _ -> raise (invalidOp ("You need three arguments for " + name))
        self.Set name (NativeFunc nf) |> ignore
