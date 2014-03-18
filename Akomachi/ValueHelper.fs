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
            | NativeObject x -> x
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
        else if t.Equals(typeof<unit>)   then unbox<Value->'T> (box (fun _ -> ()))
        else if t.IsInstanceOfType(typeof<obj>) then unbox<Value->'T> (box val2nativeobj)
        else if t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>) then unbox<Value->'T> (box id)
        else (raise (invalidOp "Unsupported"))
    let unboxDynamic (v:Value) (to_type:System.Type) : obj =
        let t = to_type
        if      t.Equals(typeof<int>)   then (val2int v) :> obj
        else if t.Equals(typeof<float>) then (val2float v) :> obj
        else if t.Equals(typeof<int>)   then (val2string v) :> obj
        else if t.Equals(typeof<bool>)   then (val2bool v) :> obj
        else if t.Equals(typeof<unit>)   then () :> obj
        else if t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>) then v :> obj
        else if  t.IsInstanceOfType(typeof<obj>) then (val2nativeobj v)
        else (raise (invalidOp "Unsupported"))

    let inline boxFun<'T> : 'T -> Value =
        let t = typeof<'T>;
        if       t.Equals(typeof<int>)   then (unbox<'T->Value>  (box Int))
        else if  t.Equals(typeof<float>) then (unbox<'T->Value>  (box Float))
        else if  t.Equals(typeof<bool>)   then (unbox<'T->Value> (box Bool))
        else if  t.Equals(typeof<string>) then (unbox<'T->Value> (box String))
        else if  t.Equals(typeof<AkObj>) then (unbox<'T->Value>  (box Obj))
        else if  t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>)   then unbox<'T->Value> (box id)
        else if  t.IsInstanceOfType(typeof<obj>) then (unbox<'T->Value>  (box NativeObject))
        else raise (invalidOp (sprintf "Unsupported type: %s" (t.ToString())))
    let rec boxDynamic (v:obj) : Value =
        let t = v.GetType()
        if       t.Equals(typeof<int>)   then Int (v :?> int)
        else if  t.Equals(typeof<float>) then Float (v :?> float)
        else if  t.Equals(typeof<bool>)   then Bool (v :?> bool)
        else if  t.Equals(typeof<string>) then String (v :?> string)
        else if  t.Equals(typeof<AkObj>) then Obj  (v :?> AkObj)
        else if  t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>)  then v :?> Value
        else if  t.IsInstanceOfType(typeof<obj>) then NativeObject (v)
        else raise (invalidOp (sprintf "Unsupported type: %s" (t.ToString())))
    let list2obj (lst : Value list) = AkObj ( dict (List.zip (List.map string [0..(List.length lst)-1]) lst) )
module NativeHelper =
    let invoke (ty:System.Type) (name:string) (self:Value) (args:Value list) =
        let fn = ty.GetMethod name
        let ps = List.map (fun (x:System.Reflection.ParameterInfo) -> x.ParameterType) (Array.toList (fn.GetParameters()))
        if fn.IsStatic then
            let v = fn.Invoke(null, List.toArray (List.map (fun (x,y) -> unboxDynamic x y) (List.zip (self::args) ps)))
            boxDynamic ( v )
        else
            match self with
               | NativeObject sobj -> boxDynamic (fn.Invoke(sobj, List.toArray (List.map (val2obj) args)))
               | _ -> (raise (invalidOp ""))
    let get spr name =
        let ty = spr.GetType()
        if (ty.GetField name) <> null then
            let field = ty.GetField name
            let v = (field.GetValue(spr))
            boxDynamic v
        else if (ty.GetProperty name) <> null then
            let p = ty.GetProperty name
            let v = p.GetValue(spr, null)
            boxDynamic v
        else if (ty.GetMethod name) <> null then
            NativeFunc (ty, name)
        else Null
    let set spr name v =
        let ty = spr.GetType()
        if (ty.GetField name) <> null then
            let field = ty.GetField name
            field.SetValue(spr, (val2obj v));
            v
        else if (ty.GetProperty name) <> null then
            let p = ty.GetProperty name
            p.SetValue(spr, (val2obj v), null)
            v
        else if (ty.GetMethod name) <> null then
            (raise (invalidOp "???"))
        else Null
    let save spr =
        let ty = spr.GetType()
        let fn = ty.GetMethod "Save"
        if not (fn.ReturnType.Equals(typeof<string>)) then (raise (invalidArg (ty.ToString()) "You need to Save method for saving")) else ()
        if fn.IsStatic then fn.Invoke(null, List.toArray [spr]) else fn.Invoke(spr, null)
