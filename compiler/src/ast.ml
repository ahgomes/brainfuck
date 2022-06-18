
type id = string

type binop =
| Add
| Sub
| Mul

type exp =
| Num of int
| Str of string
| Id of id
| Bop of binop * exp * exp
| IsZero of exp
| Assn of exp * exp
| Decl of id * exp

type prog = exp list
