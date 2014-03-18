namespace Akomachi

module Makimono =
    type SValue =
          Int    of int
        | Float  of float
        | Bool   of bool
        | String of string
        | Obj    of AkObj
        | Fun    of AkFun
        | NativeObject  of obj
        | NativeFunc of (System.Type * string)
        | Null
    let save (obj:AkObj) =
        let objSet = System.Collections.Generic.List<obj>()
        let map = System.Collections.Generic.Dictionary<obj, int>();
        let indexOf x = if map.ContainsKey x then (map.Item x) else -1
        let add (x:obj) =
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
                            add(v) |> ignore
                            let env,args,ast = _v
                            walk(env)
                        | _ -> ()
                else ()
        walk(obj)
        objSet
