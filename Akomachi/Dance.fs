namespace Akomachi

open FParsec

[<AutoOpen>]
module Stage =
    let list2obj (lst : Value list) = AkObj ( dict (List.zip (List.map string [0..(List.length lst)-1]) lst) )
    type Akomachi ()=
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
            let regUniBin name fn1 fn2 fn3 fn4 =
                    let fn lst =
                        match lst with
                            | Int x :: [] -> Int (fn1 x)
                            | Float x :: [] -> Float (fn2 x)
                            | Int x :: Int y :: [] -> Int (fn3 x y)
                            | Int x :: Float y :: [] -> Float (fn4 (float x) y)
                            | Float x :: Int y :: [] -> Float (fn4 x (float y))
                            | Float x :: Float y :: [] -> Float (fn4 x y)
                            | _ -> (raise (invalidOp ("You need two arguments for " + name)))
                    intP.Set name (NativeFunc fn) |> ignore
                    floatP.Set name (NativeFunc fn) |> ignore
            regUniBin "+" id id (+) (+)
            regUniBin "-" (fun x-> -x) (fun x-> -x) (-) (-)
            intP.regFun ("~", (~~~))
            boolP.regFun ("!", fun x -> not x)
            regBinaryBool "==" (=) (=)
            regBinaryBool "!=" (<>) (<>)
            regBinaryBool ">=" (>=) (>=)
            regBinaryBool ">" (>) (>)
            regBinaryBool "<=" (<=) (<=)
            regBinaryBool "<" (<) (<)
        let get (obj:Value) (name:string):Value =
            match obj with
                | Int    i -> intP.Get name
                | Float  f -> floatP.Get name
                | Bool   b -> boolP.Get name
                | String s -> stringP.Get name
                | Obj    obj -> obj.Item name
                | Fun    (env, arglist, body) -> Null
                | NativeObject obj -> Null
                | NativeFunc obj -> Null
                | Null -> Null
        let set (obj:Value) (name:string) (v:Value):Value =
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
                                | Value.Fun (env, arglist, fnast) -> eval (obj :: selfStack) ((inheritObj env) :: stack) fnast
                                | Value.NativeFunc fn -> (fn (obj :: args))
                                | _ -> raise (invalidArg "" "")
                        | v ->
                            match (eval selfStack stack v) with
                                | Value.Fun (env, arglist, fnast) ->
                                    if (List.length arglist <> List.length args ) then
                                            raise (invalidOp (sprintf "Argument length does not match: %d vs %d" (List.length arglist) (List.length args)))
                                        else ()
                                    let env = (inheritObj env)
                                    for (n,v) in List.zip arglist args do
                                        env.Add(n,v)
                                    eval (Null :: selfStack) (env :: stack) fnast
                                | Value.NativeFunc fn -> (fn (Null :: args))
                                | v -> raise (invalidArg "ValueAST" (sprintf "%A" v))
                | AST.If (condAst, thenAst, elseAst) ->
                    match eval selfStack stack condAst with
                        | (Bool true) -> eval selfStack stack thenAst
                        | (Bool false) -> eval selfStack stack elseAst
                        | v -> raise (invalidOp (sprintf "%A" v))
                | AST.Uni (sym, valueAst) ->
                    let obj = eval selfStack stack valueAst
                    let fn = get obj sym
                    match fn with
                        | Value.Fun (env, arglist, fnast) -> eval (obj :: selfStack) ((inheritObj env) :: stack) fnast
                        | Value.NativeFunc fn -> (fn [obj])
                        | _ -> raise (invalidArg "" "")
                | AST.Binary (val1ast, sym, val2ast) ->
                    let obj1 = eval selfStack stack val1ast
                    let obj2 = eval selfStack stack val2ast
                    let fn = get obj1 sym
                    match fn with
                        | Value.Fun (env, arglist, fnast) -> eval (obj1 :: selfStack) ((inheritObj env) :: stack) fnast
                        | Value.NativeFunc fn -> (fn [obj1; obj2])
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
                | AST.Fun (args, exprs) -> Value.Fun ((inheritObj (List.head stack)), args, exprs)
                | AST.Var (name, expr) ->
                    let obj = eval selfStack stack expr
                    (List.head stack).Remove(name) |> ignore
                    (List.head stack).Add(name, obj)
                    obj
        and evalList (selfStack:Value list) (scopeStack:AkObj list) (src:AST list) : Value =
            match src with
                | it :: [] -> eval selfStack scopeStack it
                | it :: left :: xs -> eval selfStack scopeStack it |> ignore; evalList selfStack scopeStack (left :: xs)
                | [] -> Null
        member self.dance (src:AST) = eval [] [globalObj] (AST.Call (src, []))
        