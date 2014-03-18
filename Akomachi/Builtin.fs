namespace Akomachi

module Builtin=
    type Math()=
        static member pi = System.Math.PI
        static member sin (_:obj) x = System.Math.Sin(x)
        static member cos (_:obj) x = System.Math.Cos(x)
        static member tan (_:obj) x = System.Math.Tan(x)
        static member abs (_:obj) (v:Value) =
                match v with
                         | Int x -> (Int (System.Math.Abs x))
                         | Float x -> (Float (System.Math.Abs x))
                         | _ -> (raise (invalidArg "" ""))
        member self.Save () : string = ""
    type Number() =
        static member op name x y fn1 fn2 =
            match [x;y] with
                | [Int x; Float y] -> Float (fn1 (float x) y)
                | [Float x; Float y] -> Float (fn1 x y)
                | [Float x; Int y] -> Float (fn1 x (float y))
                | [Int x; Int y] -> Int (fn2 x y)
                | _ -> (raise (invalidOp ("You need to pass two numerical arguments for " + name)))
        static member opBool name x y fn1 fn2 =
                      match [x;y] with
                       |  [Int x; Float y] -> Bool (fn1 (float x) y)
                       |  [Float x; Float y] -> Bool (fn1 x y)
                       |  [Float x; Int y] -> Bool (fn1 x (float y))
                       |  [Int x; Int y] -> Bool (fn2 x y)
                       | _ -> (raise (invalidOp ("You need to pass two numerical arguments for " + name)))
        static member opAdd x y = Number.op "+" x y (+) (+)
        static member opSub x y = Number.op "-" x y (-) (-)
        static member opMul x y = Number.op "*" x y (*) (*)
        static member opDiv x y = Number.op "/" x y (/) (/)
        static member opMod x y = Number.op "%" x y (/) (/)
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
             | _ -> (raise (invalidArg ((x.GetType()).ToString()) "Type mismatch"))     
        static  member opPlus x =
          match x with
             | Int x -> (Int x)
             | Float x -> (Float x)
             | _ -> (raise (invalidArg ((x.GetType()).ToString()) "Type mismatch"))     
        static  member opMinus x =
          match x with
             | Int x -> (Int -x)
             | Float x -> (Float -x)
             | _ -> (raise (invalidArg ((x.GetType()).ToString()) "Type mismatch"))     
    type Bool() =
        static  member opNot x =
          match x with
             | Bool x -> (Value.Bool (not x))
             | _ -> (raise (invalidArg ((x.GetType()).ToString()) "Type mismatch"))     
    type String() =
        static  member opNot x =
          match x with
             | Bool x -> (Value.Bool (not x))
             | _ -> (raise (invalidArg ((x.GetType()).ToString()) "Type mismatch"))     
