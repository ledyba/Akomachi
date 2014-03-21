namespace Akomachi

open Newtonsoft.Json
open Newtonsoft.Json.Linq
open System.Linq
open Microsoft.FSharp.Linq
open FParsec

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
                let d = new System.Collections.Generic.Dictionary<string, obj list>();
                for k in akobj.Keys do
                    d.Add(k, toListItem (akobj.Item k))
                ["Obj"; d]
            | Value.NativeObject natobj ->
                ["NativeObject"; natobj.GetType().FullName; NativeHelper.save natobj]
            | Value.NativeFunc (typ, name) ->
                ["NativeFunction"; typ.FullName; name]
            | Value.Fun (obj, args, ast, src) ->
                ["Fun"; toListItem (Value.Obj obj); args; src ]
        walk(obj)
        JsonConvert.SerializeObject( Array.map toList (objSet.ToArray()) )
    let load (src:string)=
        let lst = JsonConvert.DeserializeObject<obj [] []>(src)
        let objs = System.Collections.Generic.List<Value>()
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
                let objIdx = ((lst.[1] :?> Newtonsoft.Json.Linq.JArray).ToArray()).[1].Value<int>()
                let args = Array.map (fun (x:JToken) -> x.ToString()) ((lst.[2] :?> Newtonsoft.Json.Linq.JArray).ToArray())
                let src = lst.[3] :?> string
                let obj =
                    match objs.Item objIdx with
                      | (Value.Obj obj) -> obj
                      | _ -> (raise (invalidArg (sprintf "%A" (lst.[0])) "Unknwown"))
                match Parser.runForFunc src with
                  | Success (AST.Fun (args, expr, src),us,p)   ->
                        Value.Fun (obj, args, expr, src)
                  | Success (_, _, _) -> raise (invalidOp (sprintf "Not a function: %s" src)); Value.Null
                  | Failure (msg,err,us) -> raise (invalidOp (sprintf "Failed to parse: %s" msg)); Value.Null
            | _ -> (raise (invalidArg (sprintf "%A" (lst.[0])) "Unknwown"))

        for obj in lst do
            objs.Add(spawner obj)
        let spawnItem (x : obj list) : Value =
          let head = (List.head x) :?> string
          let left = List.tail x
          let next = List.head left
          match head with
            | "Int" -> (Value.Int (int (next:?>int64)))
            | "Float" -> (Float (next:?>float))
            | "Bool" -> (Bool (next:?> bool))
            | "String" -> (String (next:?> string) )
            | "Obj" -> (objs.[(int (next:?>int64))])
            | "Fun" -> (objs.[(int (next:?>int64))])
            | "NativeObject" -> (objs.[(int (next:?>int64))])
            | "NativeFunc" -> (objs.[(int (next:?>int64))])
            | "Null" -> Null
            | _ -> (raise (invalidArg (sprintf "%A" (List.head x)) "Unknwown"))
        let inject (x:Value, y : obj []) =
          let head = y.[0] :?> string
          let left = List.tail (Array.toList y)
          match x with
                | Obj x ->
                    let dic = (left.Head :?> JObject)
                    for k in dic do
                        let item = spawnItem ( Array.toList (Array.map (fun (o:JToken) -> o.ToObject()) (k.Value.ToArray())))
                        x.Add(k.Key, item)
                    ()
                | NativeObject x -> ()
                | NativeFunc _ -> ()
                | Fun x ->
                    ()
                | _ -> (raise (invalidArg (sprintf "%A" (lst.[0])) "Unknwown"))
          x
        Array.map inject (Array.zip (objs.ToArray()) lst) |> ignore
        objs.[0]