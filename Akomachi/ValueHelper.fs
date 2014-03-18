namespace Akomachi

open FParsec

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
    let val2obj (obj:Value) : obj =
        match obj with
            | Int    i -> i :> obj
            | Float  f -> f :> obj
            | Bool   b -> b :> obj
            | String s -> s :> obj
            | Obj    obj -> obj :> obj
            | Fun    f -> f :> obj
            | NativeObject x -> x :> obj
            | NativeFunc f -> f :> obj
            | Null -> null
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
    let val2nativeobj v =
      match v with
        | NativeObject i -> (i)
        | _ -> raise (invalidOp (sprintf "%s cannot to be converted to Native Object" (val2string v)))
    let inline unboxFun< 'T > : Value -> 'T =
        let t = typeof<('T)>
        if      t.Equals(typeof<int>)   then unbox<Value->'T> (box val2int)
        else if t.Equals(typeof<float>) then unbox<Value->'T> (box val2float)
        else if t.Equals(typeof<int>)   then unbox<Value->'T> (box val2string)
        else if t.Equals(typeof<bool>)   then unbox<Value->'T> (box val2bool)
        else if t.Equals(typeof<Value>)   then unbox<Value->'T> (box id)
        else (raise (invalidOp "Unsupported"))
    let inline boxFun<'T> : 'T -> Value =
        let t = typeof<'T>;
        if       t.Equals(typeof<int>)   then (unbox<'T->Value>  (box Int))
        else if  t.Equals(typeof<float>) then (unbox<'T->Value>  (box Float))
        else if  t.Equals(typeof<bool>)   then (unbox<'T->Value> (box Bool))
        else if  t.Equals(typeof<string>) then (unbox<'T->Value> (box String))
        else if  t.Equals(typeof<AkObj>) then (unbox<'T->Value>  (box Obj))
        else if  t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>)   then unbox<'T->Value> (box id)
        else if  t.IsInstanceOfType(typeof<AkNativeObject>) then (unbox<'T->Value>  (box NativeObject))
        else raise (invalidOp (sprintf "Unsupported type: %s" (t.ToString())))
    let rec unboxDynamic (v:Value) (to_type:System.Type) : obj =
        let t = to_type
        if      t.Equals(typeof<int>)   then (val2int v) :> obj
        else if t.Equals(typeof<float>) then (val2float v) :> obj
        else if t.Equals(typeof<int>)   then (val2string v) :> obj
        else if t.Equals(typeof<bool>)   then (val2bool v) :> obj
        else if  t.IsInstanceOfType(typeof<AkNativeObject>) then (val2nativeobj v) :> obj
        else if t.Equals(typeof<Value>)   then v :> obj
        else (raise (invalidOp "Unsupported"))
    let rec boxDynamic (v:obj) : Value =
        let t = v.GetType()
        if       t.Equals(typeof<int>)   then Int (v :?> int)
        else if  t.Equals(typeof<float>) then Float (v :?> float)
        else if  t.Equals(typeof<bool>)   then Bool (v :?> bool)
        else if  t.Equals(typeof<string>) then String (v :?> string)
        else if  t.Equals(typeof<AkObj>) then Obj  (v :?> AkObj)
        else if  t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>)  then v :?> Value
        else if  t.IsInstanceOfType(typeof<AkNativeObject>) then NativeObject (v :?> AkNativeObject)
        else if Reflection.FSharpType.IsFunction(t) then
            let ty_arg, ty_ret = Reflection.FSharpType.GetFunctionElements t
            let f = v :?> Microsoft.FSharp.Core.FSharpFunc<Value, Microsoft.FSharp.Core.FSharpFunc<double, double>>
            let fn (args: Value list) : Value =
                match args with
                   | v :: left -> boxDynamic (f.Invoke v)
            NativeFunc fn
        else raise (invalidOp (sprintf "Unsupported type: %s" (t.ToString())))
    let list2obj (lst : Value list) = AkObj ( dict (List.zip (List.map string [0..(List.length lst)-1]) lst) )
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
type NativeObjectWrapper(obj:'a) =
  let ty = (obj.GetType())
  interface AkNativeObject with
    override self.ToString : string = (self.ToString())
    override self.Get name =
        if (ty.GetField name) <> null then
            let field = ty.GetField name
            let v = (field.GetValue(obj))
            boxDynamic v
        else if (ty.GetProperty name) <> null then
            let p = ty.GetProperty name
            let v = p.GetValue(obj, null)
            boxDynamic v
        else if (ty.GetMethod name) <> null then
            let fn = ty.GetMethod name
            let ps = List.map (fun (it:System.Reflection.ParameterInfo) -> (it.ParameterType)) (Array.toList (fn.GetParameters()));

            let f lst =
                match lst with
                    | _ :: left ->
                            let r = fn.Invoke(obj, (List.toArray (List.map (fun (x,y) -> unboxDynamic x y) (List.zip left ps))))
                            boxDynamic (r)
                    | [] -> (raise (invalidOp "???"))
            NativeFunc f
        else Null
    override self.Set name v =
        if (ty.GetField name) <> null then
            let field = ty.GetField name
            field.SetValue(obj, (val2obj v));
            v
        else if (ty.GetProperty name) <> null then
            let p = ty.GetProperty name
            p.SetValue(obj, (val2obj v), null)
            v
        else if (ty.GetMethod name) <> null then
            (raise (invalidOp "???"))
        else Null
