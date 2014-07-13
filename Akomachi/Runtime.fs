namespace Akomachi

open Akomachi.Parser
open Akomachi.Exceptions
open System.Reflection

module Runtime =
  (*******************************************************************)
  (* Runtime Value *)
  (*******************************************************************)
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
  (*******************************************************************)
  (* Builtin Objects *)
  (*******************************************************************)
  module Builtin=
    type Math(str:string)=
        new() = Math("")
        static member pi = System.Math.PI
        static member sin (_:obj) x = System.Math.Sin(x)
        static member cos (_:obj) x = System.Math.Cos(x)
        static member tan (_:obj) x = System.Math.Tan(x)
        static member abs (_:obj) (v: Value) =
                match v with
                         | Int x -> (Int (System.Math.Abs x))
                         | Float x -> (Float (System.Math.Abs x))
                         | _ -> (raise (invalidArg "" ""))
        member self.Save () : string = ""
    type Number(str:string)=
        new() = Number("")
        static member op name x y fn1 fn2 =
            match [x;y] with
                | [Int x; Float y] -> Float (fn1 (float x) y)
                | [Float x; Float y] -> Float (fn1 x y)
                | [Float x; Int y] -> Float (fn1 x (float y))
                | [Int x; Int y] -> Int (fn2 x y)
                | _ -> (raise (InvalidArgumentException ("You need to pass two numerical arguments for " + name)))
        static member opBool name x y fn1 fn2 =
                      match [x;y] with
                       |  [Int x; Float y] -> Bool (fn1 (float x) y)
                       |  [Float x; Float y] -> Bool (fn1 x y)
                       |  [Float x; Int y] -> Bool (fn1 x (float y))
                       |  [Int x; Int y] -> Bool (fn2 x y)
                       | _ -> (raise (InvalidArgumentException ("You need to pass two numerical arguments for " + name)))
        static member opAdd x y = Number.op "+" x y (+) (+)
        static member opSub x y = Number.op "-" x y (-) (-)
        static member opMul x y = Number.op "*" x y (*) (*)
        static member opDiv x y = Number.op "/" x y (/) (/)
        static member opMod x y = Number.op "%" x y (%) (%)
        static member opPow x y = Number.op "^" x y (fun x y -> System.Math.Pow(x,y)) (pown)
        static member opEq x y = Number.opBool "==" x y (=) (=)
        static member opNe x y = Number.opBool "!=" x y (<>) (<>)
        static member opLt x y = Number.opBool "<" x y (<) (<)
        static member opGt x y = Number.opBool ">" x y (>) (>)
        static member opLe x y = Number.opBool "<=" x y (<=) (<=)
        static member opGe x y = Number.opBool ">=" x y (>=) (>=)
        static  member opComplement x =
          match x with
             | Int x -> ~~~x
             | _ -> raise (TypeMismatchException (x.GetType(), "Type mismatch"))
        static  member opPlus x =
          match x with
             | Int x -> (Int x)
             | Float x -> (Float x)
             | _ -> raise (TypeMismatchException (x.GetType(), "Type mismatch"))
        static  member opMinus x =
          match x with
             | Int x -> (Int -x)
             | Float x -> (Float -x)
             | _ -> raise (TypeMismatchException (x.GetType(), "Type mismatch"))
    type Bool(str:string) =
        new() = Bool("")
        static  member opNot x =
          match x with
             | Bool x -> (Value.Bool (not x))
             | _ -> raise (TypeMismatchException (x.GetType(), "Type mismatch"))
    type String(str:string) =
        new() = String("")
        static  member opAdd x y =
          match (x,y) with
             | (String x, String y) -> (Value.String (x+y))
             | _ -> raise (TypeMismatchException (x.GetType(), "Type mismatch"))
  (*******************************************************************)
  (* Value Boxing/Unboxing *)
  (*******************************************************************)
  let value2string (obj:Value) :string =
      match obj with
          | Int    i -> (string i)
          | Float  f -> (string f)
          | Bool   b -> (string b)
          | String s -> s
          | Obj    obj -> (sprintf "<<Pure Obj: %A>>" obj)
          | Fun    (env, arglist, body, src) -> (sprintf "Fun %s" src)
          | NativeObject x -> sprintf "<<Native Object: %A>>" x
          | NativeFunc f -> sprintf "<<Native Fun>>: %A" f
          | Null -> "<<null>>"
  let value2obj (obj:Value) : obj =
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
  let value2int v =
    match v with
      | Int i -> i
      | Float i -> (int i)
      | _ -> raise (InvalidCastException (sprintf "%s cannot to be converted to int" (value2string v)))
  let value2bool v =
    match v with
      | Bool i -> i
      | _ -> raise (InvalidCastException (sprintf "%s cannot to be converted to bool" (value2string v)))
  let value2float v =
    match v with
      | Int i -> (float i)
      | Float i -> (i)
      | _ -> raise (InvalidCastException (sprintf "%s cannot to be converted to float" (value2string v)))
  let value2nativeobj v =
    match v with
      | NativeObject i -> (i)
      | _ -> raise (InvalidCastException (sprintf "%s cannot to be converted to Native Object" (value2string v)))
  let inline unboxFun< 'T > : Value -> 'T =
      let t = typeof<('T)>
      if      t.Equals(typeof<int>)   then unbox<Value->'T> (box value2int)
      else if t.Equals(typeof<float>) then unbox<Value->'T> (box value2float)
      else if t.Equals(typeof<int>)   then unbox<Value->'T> (box value2string)
      else if t.Equals(typeof<bool>)   then unbox<Value->'T> (box value2bool)
      else if t.Equals(typeof<unit>)   then unbox<Value->'T> (box (fun _ -> ()))
      #if PORTABLE
      else if typeof<obj>.GetTypeInfo().IsAssignableFrom(t.GetTypeInfo()) then unbox<Value->'T> (box value2nativeobj)
      else if t.Equals(typeof<Value>) || t.GetTypeInfo().IsSubclassOf(typeof<Value>) then unbox<Value->'T> (box id)
      #else
      else if typeof<obj>.IsAssignableFrom(t) then unbox<Value->'T> (box value2nativeobj)
      else if t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>) then unbox<Value->'T> (box id)
      #endif
      else (raise (UnsupportedUnboxingException (t, "Unsupported")))
  let unboxDynamic (v:Value) (to_type:System.Type) : obj =
      let t = to_type
      if      t.Equals(typeof<int>)   then (value2int v) :> obj
      else if t.Equals(typeof<float>) then (value2float v) :> obj
      else if t.Equals(typeof<int>)   then (value2string v) :> obj
      else if t.Equals(typeof<bool>)   then (value2bool v) :> obj
      else if t.Equals(typeof<unit>)   then () :> obj
      #if PORTABLE
      else if t.Equals(typeof<Value>) || t.GetTypeInfo().IsSubclassOf(typeof<Value>) then v :> obj
      else if  typeof<obj>.GetTypeInfo().IsAssignableFrom(t.GetTypeInfo()) then (value2nativeobj v)
      #else
      else if t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>) then v :> obj
      else if  typeof<obj>.IsAssignableFrom(t) then (value2nativeobj v)
      #endif
      else (raise (UnsupportedUnboxingException (t, "Unsupported")))

  let inline boxFun<'T> : 'T -> Value =
      let t = typeof<'T>;
      if       t.Equals(typeof<int>)   then (unbox<'T->Value>  (box Int))
      else if  t.Equals(typeof<float>) then (unbox<'T->Value>  (box Float))
      else if  t.Equals(typeof<bool>)   then (unbox<'T->Value> (box Bool))
      else if  t.Equals(typeof<string>) then (unbox<'T->Value> (box String))
      else if  t.Equals(typeof<AkObj>) then (unbox<'T->Value>  (box Obj))
      #if PORTABLE
      else if  t.Equals(typeof<Value>) || t.GetTypeInfo().IsSubclassOf(typeof<Value>)   then unbox<'T->Value> (box id)
      else if  typeof<obj>.GetTypeInfo().IsAssignableFrom(t.GetTypeInfo()) then (unbox<'T->Value>  (box NativeObject))
      #else
      else if  t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>)   then unbox<'T->Value> (box id)
      else if  typeof<obj>.IsAssignableFrom(t) then (unbox<'T->Value>  (box NativeObject))
      #endif
      else (raise (UnsupportedBoxingException (t, "Unsupported")))
  let rec boxDynamic (v:obj) : Value =
    if v = null then Null
    else
      let t = v.GetType()
      if       t.Equals(typeof<int>)   then Int (v :?> int)
      else if  t.Equals(typeof<float>) then Float (v :?> float)
      else if  t.Equals(typeof<bool>)   then Bool (v :?> bool)
      else if  t.Equals(typeof<string>) then String (v :?> string)
      else if  t.Equals(typeof<AkObj>) then Obj  (v :?> AkObj)
      #if PORTABLE
      else if  t.Equals(typeof<Value>) || t.GetTypeInfo().IsSubclassOf(typeof<Value>)  then v :?> Value
      else if  typeof<obj>.GetTypeInfo().IsAssignableFrom(t.GetTypeInfo()) then NativeObject (v)
      #else
      else if  t.Equals(typeof<Value>) || t.IsSubclassOf(typeof<Value>)  then v :?> Value
      else if  typeof<obj>.IsAssignableFrom(t) then NativeObject (v)
      #endif
      else (raise (UnsupportedBoxingException (t, "Unsupported")))
  let list2obj (lst : Value list) = AkObj ( dict (List.zip (List.map string [0..(List.length lst)-1]) lst) )
  (*******************************************************************)
  (* Native binding Helpers *)
  (*******************************************************************)
  module Native =
      let invoke (ty:System.Type) (name:string) (self:Value) (args:Value list) =
          #if PORTABLE
          let fn = (ty.GetTypeInfo()).GetDeclaredMethod name
          #else
          let fn = (ty).GetMethod name
          #endif
          let ps = List.map (fun (x:System.Reflection.ParameterInfo) -> x.ParameterType) (Array.toList (fn.GetParameters()))
          if fn.IsStatic then
              let v = fn.Invoke(null, List.toArray (List.map (fun (x,y) -> unboxDynamic x y) (List.zip (self::args) ps)))
              boxDynamic ( v )
          else
              match self with
                 | NativeObject sobj -> boxDynamic (fn.Invoke(sobj, List.toArray (List.map (value2obj) args)))
                 | _ -> (raise (TypeMismatchException (self.GetType(), "You must supply Native Object to invoke Native function.")))
      let get spr name =
          #if PORTABLE
          let ty = spr.GetType()
          let ti = ty.GetTypeInfo()
          if (ti.GetDeclaredField name) <> null then
              let field = ti.GetDeclaredField name
              let v = (field.GetValue(spr))
              boxDynamic v
          else if (ti.GetDeclaredProperty name) <> null then
              let p = ti.GetDeclaredProperty name
              let v = p.GetValue(spr, null)
              boxDynamic v
          else if (ti.GetDeclaredMethod name) <> null then
              NativeFunc (ty, name)
          else Null
          #else
          let ti = spr.GetType()
          let ty = ti
          if (ti.GetField name) <> null then
              let field = ti.GetField name
              let v = (field.GetValue(spr))
              boxDynamic v
          else if (ti.GetProperty name) <> null then
              let p = ti.GetProperty name
              let v = p.GetValue(spr, null)
              boxDynamic v
          else if (ti.GetMethod name) <> null then
              NativeFunc (ty, name)
          else Null
          #endif
      let set spr name v =
          #if PORTABLE
          let ty = spr.GetType()
          let ti = ty.GetTypeInfo()
          if (ti.GetDeclaredField name) <> null then
              let field = ti.GetDeclaredField name
              field.SetValue(spr, (value2obj v));
              v
          else if (ti.GetDeclaredProperty name) <> null then
              let p = ti.GetDeclaredProperty name
              p.SetValue(spr, (value2obj v), null)
              v
          else if (ti.GetDeclaredMethod name) <> null then
              (raise (invalidOp "???"))
          else Null
          #else
          let ty = spr.GetType()
          let ti = ty
          if (ti.GetField name) <> null then
              let field = ti.GetField name
              field.SetValue(spr, (value2obj v));
              v
          else if (ti.GetProperty name) <> null then
              let p = ti.GetProperty name
              p.SetValue(spr, (value2obj v), null)
              v
          else if (ti.GetMethod name) <> null then
              (raise (invalidOp "???"))
          else Null
          #endif
      let save spr =
          #if PORTABLE
          let ty = spr.GetType()
          let ti = ty.GetTypeInfo()
          let fn = ti.GetDeclaredMethod "Save"
          if not (fn.ReturnType.Equals(typeof<string>)) then raise (InvalidNativeObjectException (ty, "You need to Save method for saving")) else ()
          if fn.IsStatic then fn.Invoke(null, List.toArray [spr]) else fn.Invoke(spr, null)
          #else
          let ty = spr.GetType()
          let ti = ty
          let fn = ti.GetMethod "Save"
          if not (fn.ReturnType.Equals(typeof<string>)) then raise (InvalidNativeObjectException (ty, "You need to Save method for saving")) else ()
          if fn.IsStatic then fn.Invoke(null, List.toArray [spr]) else fn.Invoke(spr, null)
          #endif
