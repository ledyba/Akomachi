namespace Akomachi

open FParsec

module Stage =
    type World ()=
        let globalObj = new AkObj()
        let intP = new System.Collections.Generic.Dictionary<string, int -> Value list -> Value>()
        let floatP = new System.Collections.Generic.Dictionary<string, float -> Value list -> Value >()
        let boolP = new System.Collections.Generic.Dictionary<string, bool -> Value list -> Value>()
        let stringP = new System.Collections.Generic.Dictionary<string, string -> Value list -> Value>()
        do
            globalObj.Add("global", Obj globalObj)
            intP.Add("+", fun x nums ->
                let nu = (List.map Val.val2int nums)
                (Int (x + List.fold (+) 0 nu)))
        member self.Global = globalObj
        
        member internal self.intProvider = intP
        member internal self.floatProvider = floatP
        member internal self.boolProvider = boolP
        member internal self.stringProvider = stringP
    let internal get (w:World) (obj:Value) (name:string):Value =
        match obj with
            | Int    i -> NativeFunc ( fun (Int s) f -> (w.intProvider.Item name) s f )
            | Float  f -> NativeFunc ( fun (Float s) f -> (w.floatProvider.Item name) s f )
            | Bool   b -> NativeFunc ( fun (Bool s) f -> (w.boolProvider.Item name) s f)
            | String s -> NativeFunc ( fun (String s) f-> (w.stringProvider.Item name) s f)
            | Obj    obj -> obj.Item name
            | Fun    (env, arglist, body) -> Null
            | NativeObj obj -> Null
            | NativeFunc obj -> Null
            | Null -> Null
    let internal set (w:World) (obj:Value) (name:string) (v:Value):Value =
        match obj with
            | Int    i -> Null
            | Float  f -> Null
            | Bool   b -> Null
            | String s -> Null
            | Obj    obj ->
                obj.Remove(name) |> ignore
                obj.Add(name, v);
                Obj obj
            | Fun    (env, arglist, body) -> Null
            | NativeObj obj -> obj.Set name v
            | NativeFunc obj -> Null
            | Null -> Null
    let internal val2string (w:World) (obj:Value) :string =
        match obj with
            | Int    i -> (string i)
            | Float  f -> (string f)
            | Bool   b -> (string b)
            | String s -> s
            | Obj    obj -> (sprintf "%A" obj)
            | Fun    (env, arglist, body) -> (sprintf "Fun %A -> %A" arglist body)
            | obj -> sprintf "%A" obj
    let internal list2obj (lst : Value list) = AkObj ( dict (List.zip (List.map string [0..(List.length lst)-1]) lst) )
    let internal inheritObj (obj : AkObj) =
        let d = new AkObj();
        d.Add("__proto__", Obj obj)
        d
    let rec internal protoSearch (akobj : AkObj) (name:string) =
                                if (akobj.ContainsKey name)
                                    then akobj
                                    else if akobj.ContainsKey "__proto__"
                                        then protoSearchV (akobj.Item "__proto__") name
                                        else raise (invalidArg "" "")
    and internal protoSearchV (obj : Value) (name:string) =
                        match obj with
                            | Obj akobj -> protoSearch akobj name
                            | _ -> raise (invalidArg "" "")
    let rec internal eval (w:World) (selfStack:Value list) (stack:AkObj list) (src:AST) : Value =
        match src with
            | AST.Int x -> Int x
            | AST.Float x -> Float x
            | AST.Bool x -> Bool x
            | AST.String x -> String x
            | AST.Null -> Null
            | AST.Object elements -> Obj ( AkObj(dict ( List.map (fun (n, ast) -> (n, (eval w selfStack stack ast))) elements)) )
            | AST.List exprs -> Obj ( list2obj ( List.map ( fun ast -> eval w selfStack stack ast ) exprs ) )
            | AST.Block exprs -> evalList w selfStack stack exprs
            | AST.Ident name -> (protoSearch (List.head stack) name).Item name

            | AST.Access (valueAst, name) -> get w (eval w selfStack stack valueAst) name
            | AST.Index (valueAst, index) -> get w (eval w selfStack stack valueAst) (val2string w (eval w selfStack stack valueAst))
            | AST.Call (valueAst, argAst) ->
                let args = List.map ( fun ast -> eval w selfStack stack ast ) argAst
                match valueAst with
                    | (AST.Access (ast, name)) ->
                        let obj = eval w selfStack stack ast
                        let fn = get w obj name
                        match fn with
                            | Value.Fun (env, arglist, fnast) -> eval w (obj :: selfStack) ((inheritObj env) :: stack) fnast
                            | Value.NativeFunc fn -> (fn obj args)
                            | _ -> raise (invalidArg "" "")
                    | v ->
                        match (eval w selfStack stack v) with
                            | Value.Fun (env, arglist, fnast) ->
                                let env = (inheritObj env)
                                for (n,v) in List.zip arglist args do
                                    env.Add(n,v)
                                eval w (Null :: selfStack) (env :: stack) fnast
                            | Value.NativeFunc fn -> (fn Null args)
                            | v -> raise (invalidArg "ValueAST" (sprintf "%A" v))
            | AST.Uni (sym, valueAst) ->
                let obj = eval w selfStack stack valueAst
                let fn = get w obj sym
                match fn with
                    | Value.Fun (env, arglist, fnast) -> eval w (obj :: selfStack) ((inheritObj env) :: stack) fnast
                    | Value.NativeFunc fn -> (fn obj [])
                    | _ -> raise (invalidArg "" "")
            | AST.Binary (val1ast, sym, val2ast) ->
                let obj1 = eval w selfStack stack val1ast
                let obj2 = eval w selfStack stack val2ast
                let fn = get w obj1 sym
                match fn with
                    | Value.Fun (env, arglist, fnast) -> eval w (obj1 :: selfStack) ((inheritObj env) :: stack) fnast
                    | Value.NativeFunc fn -> (fn obj1 [obj2])
                    | _ -> raise (invalidArg "" "")
            | AST.Assign (val1ast, val2ast) ->
                match val1ast with
                    | AST.Access (ast, name) ->
                        let obj1 = eval w selfStack stack ast
                        let obj2 = eval w selfStack stack val2ast
                        set w obj1 name obj2
                    | AST.Ident name ->
                        let obj1 = protoSearch (List.head stack) name
                        let obj2 = eval w selfStack stack val2ast
                        set w (Obj obj1) name obj2
                    | _ -> raise (invalidArg "" "")
            | AST.Fun (args, exprs) -> Value.Fun ((inheritObj (List.head stack)), args, exprs)
            | AST.Var (name, expr) ->
                let obj = eval w selfStack stack expr
                (List.head stack).Remove(name) |> ignore
                (List.head stack).Add(name, obj)
                obj
    and internal evalList (w:World) (selfStack:Value list) (scopeStack:AkObj list) (src:AST list) : Value =
        match src with
            | it :: [] -> eval w selfStack scopeStack it
            | it :: left :: xs -> eval w selfStack scopeStack it |> ignore; evalList w selfStack scopeStack (left :: xs)
            | [] -> Null
    let dance (w:World) (src:AST) = eval w [] [w.Global] (AST.Call (src, []))
        