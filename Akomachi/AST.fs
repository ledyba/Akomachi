namespace Akomachi

type AST =
      Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Null
    | Object of (string * AST) list
    | List of AST list
    | Block of AST list
    | Ident of string
    | Self

    | Access of AST * string
    | Index of AST * AST
    | Call of AST * AST list

    | If of AST * AST * AST
    | Loop of AST * AST * AST * AST

    | Uni of string * AST
    | Binary of AST * string * AST
    | Assign of AST * AST

    | Fun of string list * AST * string
    | Var of string * AST
