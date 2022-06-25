
type id = string

type ty =
| TBool
| TInt
| TString
| TPrint

type unop =
| Neg

type binop =
| Add
| Sub
| Mul

type exp =
| Int of int
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
| If of exp * stmt list * stmt list

type prog = stmt list
