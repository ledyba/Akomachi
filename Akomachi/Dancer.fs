namespace Akomachi

open FParsec

[<AutoOpen>]
module Stage =
    type World ()=
        let globalObj = new AkObj()
        let intP = new Provider<int>("Int provider")
        let floatP = new Provider<float>("Float provider")
        let boolP = new Provider<bool>("Bool provider")
        let stringP = new Provider<string>("String provider")
        do
            let regBinary name fn1 fn2 =
                let natfn lst =
                      match lst with
                        | Int x :: Float y :: _ -> Float (fn1 (float x) y)
                        | Float x :: Float y :: _ -> Float (fn1 x y)
                        | Float x :: Int y :: _ -> Float (fn1 x (float y))
                        | Int x :: Int y :: _ -> Int (fn2 x y)
                        | _ -> (raise (invalidOp ("You need two arguments for " + name)))
                intP.Set name (NativeFunc natfn) |> ignore
                floatP.Set name (NativeFunc natfn) |> ignore
            let regBinaryBool name fn1 fn2 =
                let natfn lst =
                      match lst with
                       |  Int x :: Float y :: _ -> Bool (fn1 (float x) y)
                       |  Float x :: Float y :: _ -> Bool (fn1 x y)
                       |  Float x :: Int y :: _ -> Bool (fn1 x (float y))
                       |  Int x :: Int y :: _ -> Bool (fn2 x y)
                       | _ -> (raise (invalidOp ("You need two arguments for " + name)))
                intP.Set name (NativeFunc natfn) |> ignore
                floatP.Set name (NativeFunc natfn) |> ignore
            globalObj.Add("global", Obj globalObj)
            regBinary "/" (/) (/)
            regBinary "*" (*) (*)
            regBinary "%" (%) (%)
            regBinary "^" (fun x y -> System.Math.Pow(x,y)) pown
            intP.regFun<int,int> ("+", (+))
            intP.regFun<int,int> ("-", (-))
            regBinaryBool "==" (=) (=)
            regBinaryBool "!=" (<>) (<>)
            regBinaryBool ">=" (>=) (>=)
            regBinaryBool ">" (>) (>)
            regBinaryBool "<=" (<=) (<=)
            regBinaryBool "<" (<) (<)

        member self.Global = globalObj
        
        member internal self.intProvider = intP
        member internal self.floatProvider = floatP
        member internal self.boolProvider = boolP
        member internal self.stringProvider = stringP
    let internal get (w:World) (obj:Value) (name:string):Value =
        match obj with
            | Int    i -> w.intProvider.Get name
            | Float  f -> w.floatProvider.Get name
            | Bool   b -> w.boolProvider.Get name
            | String s -> w.stringProvider.Get name
            | Obj    obj -> obj.Item name
            | Fun    (env, arglist, body) -> Null
            | NativeObject obj -> Null
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
            | NativeObject obj -> obj.Set name v
            | NativeFunc obj -> Null
            | Null -> Null
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
            | AST.Self -> (List.head selfStack)

            | AST.Access (valueAst, name) -> get w (eval w selfStack stack valueAst) name
            | AST.Index (valueAst, index) -> get w (eval w selfStack stack valueAst) (val2string (eval w selfStack stack valueAst))
            | AST.Call (valueAst, argAst) ->
                let args = List.map ( fun ast -> eval w selfStack stack ast ) argAst
                match valueAst with
                    | (AST.Access (ast, name)) ->
                        let obj = eval w selfStack stack ast
                        let fn = get w obj name
                        match fn with
                            | Value.Fun (env, arglist, fnast) -> eval w (obj :: selfStack) ((inheritObj env) :: stack) fnast
                            | Value.NativeFunc fn -> (fn (obj :: args))
                            | _ -> raise (invalidArg "" "")
                    | v ->
                        match (eval w selfStack stack v) with
                            | Value.Fun (env, arglist, fnast) ->
                                if (List.length arglist <> List.length args ) then
                                        raise (invalidOp (sprintf "Argument length does not match: %d vs %d" (List.length arglist) (List.length args)))
                                    else ()
                                let env = (inheritObj env)
                                for (n,v) in List.zip arglist args do
                                    env.Add(n,v)
                                eval w (Null :: selfStack) (env :: stack) fnast
                            | Value.NativeFunc fn -> (fn (Null :: args))
                            | v -> raise (invalidArg "ValueAST" (sprintf "%A" v))
            | AST.If (condAst, thenAst, elseAst) ->
                match eval w selfStack stack condAst with
                    | (Bool true) -> eval w selfStack stack thenAst
                    | (Bool false) -> eval w selfStack stack elseAst
                    | v -> raise (invalidOp (sprintf "%A" v))
            | AST.Uni (sym, valueAst) ->
                let obj = eval w selfStack stack valueAst
                let fn = get w obj sym
                match fn with
                    | Value.Fun (env, arglist, fnast) -> eval w (obj :: selfStack) ((inheritObj env) :: stack) fnast
                    | Value.NativeFunc fn -> (fn [obj])
                    | _ -> raise (invalidArg "" "")
            | AST.Binary (val1ast, sym, val2ast) ->
                let obj1 = eval w selfStack stack val1ast
                let obj2 = eval w selfStack stack val2ast
                let fn = get w obj1 sym
                match fn with
                    | Value.Fun (env, arglist, fnast) -> eval w (obj1 :: selfStack) ((inheritObj env) :: stack) fnast
                    | Value.NativeFunc fn -> (fn [obj1; obj2])
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
        