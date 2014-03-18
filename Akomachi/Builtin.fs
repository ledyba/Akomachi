namespace Akomachi

module Builtin=
    type Math()=
        member self.pi = System.Math.PI
        member self.sin x = System.Math.Sin(x)
        member self.cos x = System.Math.Cos(x)
        member self.tan x = System.Math.Tan(x)
        member self.abs (v:Value) =
                match v with
                         | Int x -> (Int (System.Math.Abs x))
                         | Float x -> (Float (System.Math.Abs x))
                         | _ -> (raise (invalidArg "" ""))