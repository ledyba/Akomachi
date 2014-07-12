namespace Akomachi

open System.Linq
open System.Reflection
open Akomachi.Runtime
open Akomachi.Exceptions

module Makimono =
    let save (obj:AkObj) =
        let objSet = System.Collections.Generic.List<Value>()
        let map = System.Collections.Generic.Dictionary<Value, int>();
        let indexOf x = if map.ContainsKey x then (map.Item x) else -1
        let add (x:Value) =
            if map.ContainsKey x
                then false
                else 
                    objSet.Add x |> ignore
                    let n = objSet.Count - 1
                    map.Add(x, n)
                    true
        let rec walk (o: AkObj) =
            if add (Value.Obj o)
                then
                  for key in o.Keys do
                    let v = o.Item key
                    match v with
                        | Value.Obj akobj -> walk(akobj)
                        | Value.NativeObject natobj -> add(v) |> ignore
                        | Value.NativeFunc nativef -> add(v) |> ignore
                        | Value.Fun _v ->
                            let env,args,ast,src = _v
                            walk(env)
                            add(v) |> ignore
                        | _ -> ()
                else ()
        let toListItem x : obj list =
          match x with
            | Int   i -> ["Int"; i]
            | Float f -> ["Float"; f]
            | Bool  b -> ["Bool"; b]
            | String s -> ["String"; s]
            | Obj    o -> ["Obj"; indexOf x]
            | Fun    f -> ["Fun"; indexOf x]
            | NativeObject o ->["NativeObject"; indexOf x]
            | NativeFunc o ->["NativeFunc"; indexOf x]
            | Null -> ["Null"]
        let toList x : obj list =
           match x with
            | Value.Obj akobj ->
                let d = Array.map ( fun (k:string) -> [k :> obj; (toListItem (akobj.Item k)) :> obj]) (akobj.Keys.ToArray())
                ["Obj"; d]
            | Value.NativeObject natobj ->
                ["NativeObject"; natobj.GetType().FullName; Native.save natobj]
            | Value.NativeFunc (typ, name) ->
                ["NativeFunction"; typ.FullName; name]
            | Value.Fun (obj, args, ast, src) ->
                ["Fun"; toListItem (Value.Obj obj); (List.map (fun x -> x:> obj) args); src ]
            | _ -> (raise (BrokenSavedataException (sprintf "Unknwon data type: %A" x)))
        walk(obj)
        let saveObj = Array.map toList (objSet.ToArray())
        Sexp.toSexp saveObj

    let loadImpl (o:obj)=
        let lst = o :?> obj list
        let objs = System.Collections.Generic.List<Value>()
        let spawner (lst:obj list) =
          let fst = lst.Head :?> string
          match fst with
            | "Obj" -> Obj (new AkObj())
            | "NativeObject" ->
                let t = (lst .[1] :?> string )
                let save = (lst .[2] :?> string)
                let typ = System.Type.GetType(t)
                #if PORTABLE
                let ti = typ.GetTypeInfo()
                let selector =
                  fun (x:ConstructorInfo) ->
                    let para = x.GetParameters()
                    x.IsPublic && para.Length = 1 && para.[0].ParameterType.Equals(typeof<string>)
                let con = ti.DeclaredConstructors.Single( new System.Func<ConstructorInfo, bool>(selector) )q
                #else
                let con = typ.GetConstructor [| typeof<string> |]
                #endif
                NativeObject (con.Invoke([|save|]))
            | "NativeFunction" ->
                let typ = (System.Type.GetType(lst.[1] :?> string))
                let fname = (lst.[2] :?> string)
                NativeFunc (typ, fname )
            | "Fun" ->
                let objIdx = (lst.[1] :?> obj list).[1] :?> int
                let args = Array.map (fun (x:obj) -> x.ToString()) (Seq.toArray (lst.[2] :?> obj list))
                let src = lst.[3] :?> string
                let obj =
                    let v = objs.Item objIdx
                    match v with
                      | (Value.Obj obj) -> obj
                      | _ -> (raise (BrokenSavedataException (sprintf "Unknwon data type: %A" v)))
                match Parser.runForFunc src with
                  | Parser.Result.Success (Parser.AST.Fun (args, expr, src))   ->
                        Value.Fun (obj, args, expr, src)
                  | Parser.Result.Success _ -> raise (BrokenSavedataException (sprintf "Not a function: %s" src)); Value.Null
                  | Parser.Result.Error msg -> raise (BrokenSavedataException (sprintf "Failed to parse: %s" msg)); Value.Null
            | _ -> raise (BrokenSavedataException (sprintf "Unknown data: %A" lst))
        for ob in lst do
            objs.Add(spawner (ob :?> obj list))
        let spawnItem (x : obj list) : Value =
          let head = x.[0] :?> string
          let next = x.[1]
          match head with
            | "Int" -> (Value.Int (int (next :?> int)))
            | "Float" -> (Float (next :?> float))
            | "Bool" -> (Bool (next :?> bool))
            | "String" -> (String (next :?> string) )
            | "Obj" -> (objs.[(int (next :?> int))])
            | "Fun" -> (objs.[(int (next :?> int))])
            | "NativeObject" -> (objs.[(next:?> int)])
            | "NativeFunc" -> (objs.[(next :?> int)])
            | "Null" -> Null
            | _ -> raise (BrokenSavedataException (sprintf "Unknown data: %A" x))
        let inject (x:Value, y : obj list) =
          let head = y.[0] :?> string
          match x with
                | Obj x ->
                    let dic = y.[1] :?> obj list
                    for o_ in dic do
                        let o = o_ :?> obj list
                        let k = o.[0] :?> string
                        let v = o.[1]
                        x.Add(k, spawnItem (v :?> obj list))
                    ()
                | NativeObject x -> ()
                | NativeFunc _ -> ()
                | Fun x -> ()
                | _ -> raise (BrokenSavedataException (sprintf "Unknown data: %A" lst))
          x
        Array.map inject (Array.zip (objs.ToArray()) (List.toArray (List.map (fun (x:obj) -> x :?> obj list) lst)) ) |> ignore
        objs.[0]
    let load (src:string)=
        match (Sexp.parse src) with
            | Sexp.Success res -> loadImpl res
            | Sexp.Error err   -> raise (System.Exception(err))
