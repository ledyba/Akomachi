namespace Akomachi

open FParsec

[<AutoOpen>]
module Stage =
    type Akomachi ()=
        let globalObj = new AkObj()
        let numP = new Builtin.Number();
        let boolP = new Builtin.Bool();
        let stringP = new Builtin.String()
        do
            globalObj.Add("global", Obj globalObj)
            globalObj.Add ("Math", NativeObject (new Builtin.Math()) )
        let get (obj:Value) (name:string):Value =
            match obj with
                | Int    i -> NativeHelper.get numP name
                | Float  f -> NativeHelper.get numP name
                | Bool   b -> NativeHelper.get boolP name
                | String s -> NativeHelper.get stringP name
                | Obj    obj -> obj.Item name
                | Fun    (env, arglist, body, _) -> Null
                | NativeObject obj -> NativeHelper.get obj name
                | NativeFunc obj -> Null
                | Null -> Null
        let set (obj:Value) (name:string) (v:Value):Value =
            match obj with
                | Int    i -> Null
                | Float  f -> Null
                | Bool   b -> Null
                | String s -> Null
                | Obj    o ->
                    o.Remove(name) |> ignore
                    o.Add(name, v);
                    obj
                | Fun    (env, arglist, body, _) -> Null
                | NativeObject obj -> NativeHelper.set obj name v
                | NativeFunc obj -> Null
                | Null -> Null
        let inheritObj (obj : AkObj) =
            let d = new AkObj();
            d.Add("__proto__", Obj obj)
            d
        let rec protoSearch (akobj : AkObj) (name:string) =
                                    if (akobj.ContainsKey name)
                                        then akobj
                                        else if akobj.ContainsKey "__proto__"
                                            then protoSearchV (akobj.Item "__proto__") name
                                            else raise (invalidArg "" "")
        and protoSearchV (obj : Value) (name:string) =
                            match obj with
                                | Obj akobj -> protoSearch akobj name
                                | _ -> raise (invalidArg "" "")
        let rec eval (selfStack:Value list) (stack:AkObj list) (src:AST) : Value =
            match src with
                | AST.Int x -> Int x
                | AST.Float x -> Float x
                | AST.Bool x -> Bool x
                | AST.String x -> String x
                | AST.Null -> Null
                | AST.Object elements -> Obj ( AkObj(dict ( List.map (fun (n, ast) -> (n, (eval selfStack stack ast))) elements)) )
                | AST.List exprs -> Obj ( list2obj ( List.map ( fun ast -> eval selfStack stack ast ) exprs ) )
                | AST.Block exprs -> evalList selfStack stack exprs
                | AST.Ident name -> (protoSearch (List.head stack) name).Item name
                | AST.Self -> (List.head selfStack)

                | AST.Access (valueAst, name) -> get (eval selfStack stack valueAst) name
                | AST.Index (valueAst, index) -> get (eval selfStack stack valueAst) (val2string (eval selfStack stack valueAst))
                | AST.Call (valueAst, argAst) ->
                    let args = List.map ( fun ast -> eval selfStack stack ast ) argAst
                    match valueAst with
                        | (AST.Access (ast, name)) ->
                            let obj = eval selfStack stack ast
                            let fn = get obj name
                            match fn with
                                | Value.Fun (env, arglist, fnast, _) -> eval (obj :: selfStack) ((inheritObj env) :: stack) fnast
                                | Value.NativeFunc (typ, fname) -> NativeHelper.invoke typ fname obj args
                                | _ -> raise (invalidOp (sprintf "%A" fn))
                        | v ->
                            let recv = (eval selfStack stack v)
                            match recv with
                                | Value.Fun (env, arglist, fnast, _) ->
                                    if (List.length arglist <> List.length args ) then
                                            raise (invalidOp (sprintf "Argument length does not match: %d vs %d" (List.length arglist) (List.length args)))
                                        else ()
                                    let env = (inheritObj env)
                                    for (n,v) in List.zip arglist args do
                                        env.Add(n,v)
                                    eval (Null :: selfStack) (env :: stack) fnast
                                | Value.NativeFunc (typ, fname) ->  NativeHelper.invoke typ fname Null args
                                | Value.NativeObject o -> ( NativeHelper.invoke (o.GetType()) "opApply" recv args  )
                                | Value.Obj o ->
                                    let it = o.Item "opApply"
                                    match it with
                                        | Value.Fun (env, arglist, fast, _) ->
                                            if (List.length arglist <> List.length args ) then
                                                    raise (invalidOp (sprintf "Argument length does not match: %d vs %d" (List.length arglist) (List.length args)))
                                                else ()
                                            let env = (inheritObj env)
                                            for (n,v) in List.zip arglist args do
                                                env.Add(n,v)
                                            eval (Null :: selfStack) (env :: stack) fast
                                        | Value.NativeFunc (typ, fname) ->  NativeHelper.invoke typ fname Null ( recv :: args )
                                        | v -> raise (invalidArg "ValueAST" (sprintf "%A" v))
                                | v -> raise (invalidArg "ValueAST" (sprintf "%A" v))
                | AST.If (condAst, thenAst, elseAst) ->
                    match eval selfStack stack condAst with
                        | (Bool true) -> eval selfStack stack thenAst
                        | (Bool false) -> eval selfStack stack elseAst
                        | v -> raise (invalidOp (sprintf "%A" v))
                | AST.Loop (initAst, condAst, thenAst, doAst) ->
                    evalLoop selfStack stack initAst condAst thenAst doAst
                | AST.Uni (sym, valueAst) ->
                    let obj = eval selfStack stack valueAst
                    let fn = get obj sym
                    match fn with
                        | Value.Fun (env, arglist, fnast, _) -> eval (obj :: selfStack) ((inheritObj env) :: stack) fnast
                        | Value.NativeFunc (typ, fname) -> NativeHelper.invoke typ fname obj []
                        | _ -> raise (invalidArg "" "")
                | AST.Binary (val1ast, sym, val2ast) ->
                    let obj1 = eval selfStack stack val1ast
                    let obj2 = eval selfStack stack val2ast
                    let fn = get obj1 sym
                    match fn with
                        | Value.Fun (env, arglist, fnast, _) -> eval (obj1 :: selfStack) ((inheritObj env) :: stack) fnast
                        | Value.NativeFunc (typ, fname) ->  NativeHelper.invoke typ fname obj1 [obj2]
                        | _ -> raise (invalidArg "" "")
                | AST.Assign (val1ast, val2ast) ->
                    match val1ast with
                        | AST.Access (ast, name) ->
                            let obj1 = eval selfStack stack ast
                            let obj2 = eval selfStack stack val2ast
                            set obj1 name obj2
                        | AST.Ident name ->
                            let obj1 = protoSearch (List.head stack) name
                            let obj2 = eval selfStack stack val2ast
                            set (Obj obj1) name obj2
                        | _ -> raise (invalidArg "" "")
                | AST.Fun (args, exprs, src) -> Value.Fun ((inheritObj (List.head stack)), args, exprs, src)
                | AST.Var (name, expr) ->
                    let obj = eval selfStack stack expr
                    (List.head stack).Remove(name) |> ignore
                    (List.head stack).Add(name, obj)
                    obj
        and evalLoop (selfStack:Value list) (scopeStack:AkObj list) (initAst:AST) (condAst:AST) (stepAst:AST) (doAst:AST) =
            let rec loop (last:Value) =
                    match eval selfStack scopeStack condAst with
                        | (Bool false) -> last
                        | (Bool true) ->
                            let doRes = (eval selfStack scopeStack doAst)
                            (eval selfStack scopeStack stepAst) |> ignore
                            loop doRes
                        | _ -> (raise (invalidOp "")) 
            eval selfStack scopeStack initAst |> ignore
            loop Null
        and evalList (selfStack:Value list) (scopeStack:AkObj list) (src:AST list) : Value =
            match src with
                | it :: [] -> eval selfStack scopeStack it
                | it :: left :: xs -> eval selfStack scopeStack it |> ignore; evalList selfStack scopeStack (left :: xs)
                | [] -> Null
        member self.dance (src:AST) = eval [] [globalObj] (AST.Call (src, []))
        member self.save() = Makimono.load( Makimono.save(globalObj) )