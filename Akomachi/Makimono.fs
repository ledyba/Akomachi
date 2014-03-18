namespace Akomachi

open Newtonsoft.Json
open System.Linq
open Microsoft.FSharp.Linq

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
                            let env,args,ast = _v
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
                let d = new System.Collections.Generic.Dictionary<string, obj list>();
                for k in akobj.Keys do
                    d.Add(k, toListItem (akobj.Item k))
                ["Obj"; d]
            | Value.NativeObject natobj ->
                ["NativeObject"; natobj.GetType().FullName; NativeHelper.save natobj]
            | Value.NativeFunc (typ, name) ->
                ["NativeFunction"; typ.FullName; name]
            | Value.Fun (obj, args, ast) ->
                ["Fun"; toListItem (Value.Obj obj); args; ast]
        walk(obj)
        JsonConvert.SerializeObject( Array.map toList (objSet.ToArray()) )

    let load (src:string)=
        let lst = JsonConvert.DeserializeObject<obj [] []>(src)
        let spawner (lst:obj []) =
          match (lst.[0]) :?> string with
            | "Obj" -> Obj (new AkObj())
            | "NativeObject" ->
                let t = lst.[1] :?> string
                let save = lst.[2] :?> string
                let typ = System.Type.GetType(t)
                let con = typ.GetConstructor([| typeof<string> |] )
                NativeObject (con.Invoke([|save|]))
            | "NativeFunction" -> NativeFunc (System.Type.GetType(lst.[1] :?> string), lst.[2] :?> string)
            | "Fun" ->
                //let objIdx = lst.[1] :?> int
                //let args = lst.[2] :?> obj []
                //let ast = JsonConvert.DeserializeObject<AST>(JsonConvert.SerializeObject(lst.[3]))
                Value.Fun (null, [], AST.Null)
            | _ -> (raise (invalidArg (sprintf "%A" (lst.[0])) "Unknwown"))
        let objs: Value [] = Array.map spawner lst
        let spawnItem (x : obj list) : Value =
          let head = (List.head x) :?> string
          let left = List.tail x
          match head with
            | "Int" -> (Value.Int ((List.head x):?>int))
            | "Float" -> (Float ((List.head x):?>float))
            | "Bool" -> (Bool ((List.head x):?> bool))
            | "String" -> (String ((List.head x):?> string) )
            | "Obj" -> (objs.[(List.head x):?>int])
            | "Fun" -> (objs.[(List.head x):?>int])
            | "NativeObject" -> (objs.[(List.head x):?>int])
            | "NativeFunc" -> (objs.[(List.head x):?>int])
            | "Null" -> Null
            | _ -> (raise (invalidArg (sprintf "%A" (List.head x)) "Unknwown"))
        let inject (x:Value, y : obj []) =
          let head = y.[0] :?> string
          let left = List.tail (Array.toList y)
          match x with
                | Obj x ->
                    ()
                | NativeObject x -> ()
                | NativeFunc _ -> ()
                | Fun x ->
                    //let objIdx = lst.[1] :?> int
                    //let args = lst.[2] :?> obj []
                    //let ast = JsonConvert.DeserializeObject<AST>(JsonConvert.SerializeObject(lst.[3]))
                    ()
                | _ -> (raise (invalidArg (sprintf "%A" (lst.[0])) "Unknwown"))
          x
        let v = Array.map inject (Array.zip objs lst)
        v