namespace Akomachi

type AST =
      Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Null
    | Object of (string * AST) list
    | Access of AST * string
    | Index of AST * AST
    | Call of AST * AST list
    | Uni of string * AST
    | Binary of AST * string * AST
    | Assign of AST * AST
    | List of AST list
    | Block of AST list
    | Fun of string list * AST
    | Ident of string
    | Var of string * AST

