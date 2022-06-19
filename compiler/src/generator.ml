open Ast
open Bf

exception Compile_Error of string

let tape_size = 30000

type layout = (Ast.id * int) list
type ctxt = { ptr : int ref; mutable layout : layout; tape : bool array }

let lookup (l:layout) (id:id) : int =
  try (List.assoc id l)
  with _ -> raise (Compile_Error ("Unbound value " ^ id))

let add (ctxt:ctxt) (id:id) (loc:int) =
  ctxt.layout <- (id, loc)::ctxt.layout

let layout_str layout =
  let f x y = Printf.sprintf "%s(%s, %d); " x (fst y) (snd y) in
  List.fold_left f "" layout

let tape_str tape =
  let f i x = if x then i else -1 in
  let g x y = if y >= 0 then Printf.sprintf "%s%d; " x y else x in
  Array.fold_left g "" @@ Array.mapi f tape

let print_ctxt ctxt =
  let l_str = layout_str ctxt.layout in
  let t_str = tape_str ctxt.tape in
  Printf.printf "ptr: %d; layout: [ %s]; tape: [ taken: %s]\n"
    !(ctxt.ptr) l_str t_str

let rec find_avaiable tape i : int =
  try
    if tape.(i) then find_avaiable tape (i+1)
    else i
  with _ -> raise (Compile_Error ("Memory overflow!"))

let take_loc tape i : int = Array.set tape i true; i
let release_loc tape i : int = Array.set tape i false; i

let ( *@ ) x n = Array.make n x |> Array.to_list

let move_ptr ctxt target = (* QUESTION: disable wrapping or no ? *)
  let dist = target - !(ctxt.ptr) in
  let _ = ctxt.ptr := target in
  if dist > 0 then Right *@ dist
    else Left *@ (abs dist)

let rec gen_num n : block =
  if n < 15 then Inc *@ n
  else []

let rec gen_str s : block = []

let rec gen_block (ctxt:ctxt) (exp:exp) : block =
  let _ = print_ctxt ctxt in
  match exp with
  | Num i -> gen_num i
  | Str s -> gen_str s
  | Id id -> lookup ctxt.layout id |> move_ptr ctxt
  (* | Bop (bop, e1, e2) -> [] *)
(*   | IsZero e -> [] TODO: need add if else statments *)
  (* | Assn (e1, e2) ->  *)
  | Decl (id, e) -> (* TODO: add non-initalized vars *)
    let init = gen_block ctxt e in
    let new_ptr = take_loc ctxt.tape @@ find_avaiable ctxt.tape 0 in
    add ctxt id new_ptr; (move_ptr ctxt new_ptr) @ init
  | _ -> []


let gen_prog (prog:Ast.prog) : string =
  let ctxt = { ptr = ref 2; layout = []; tape = Array.make tape_size false } in
  Bf.string_of_prog @@ List.map (fun x -> gen_block ctxt x) prog

let test_gen () = gen_prog [Num 13; Decl ("x", Num 5); Decl ("y", Num 3);]
  (* Bf.string_of_prog [[Right; Inc; Inc;]; [Left; Dec; Out;]] *)
