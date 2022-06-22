
type cmd =
| Right
| Left
| Inc
| Dec
| Out
| In
| Loop of cmd list

type block = cmd list

type prog = block list

let rec string_of_cmd : cmd -> string = function
  | Right -> ">"
  | Left -> "<"
  | Inc -> "+"
  | Dec -> "-"
  | Out -> "."
  | In -> ","
  | Loop b -> "[" ^ (string_of_block b) ^ "]"
and string_of_block block =
  String.concat "" @@ List.map string_of_cmd block

let string_of_prog prog =
  List.map string_of_block prog
  |> String.concat ""
  |> fun x -> x
