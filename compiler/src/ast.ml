
type id = string

type unop =
| Neg

type binop =
| Add
| Sub
| Mul

type exp =
| Num of int
| Str of string
| PStr of string
| Id of id
| Bop of binop * exp * exp
| Uop of unop * exp
| IsZero of exp

type stmt =
| Assn of exp * exp
| Decl of id * exp
| Print of exp

type prog = stmt list
