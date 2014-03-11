namespace Akomachi

type AST =
      Int of int
    | Float of float
    | String of string
    | Bool of bool
    | Name of string
    | Access of AST * string
    | Index of AST * AST
    | Call of AST * AST
    | UniPlus of AST
    | UniMinus of AST
    | UniComplement of AST
    | UniNot of AST
    | Binary of AST * string * AST
