open Ast
open Bf

exception Compile_Error of string

let tape_size = 30000

type layout = (Ast.id * (int * int)) list
type ctxt = { ptr : int ref; mutable layout : layout; tape : bool array }

let lookup (l:layout) (id:id) : int * int =
  try (List.assoc id l)
  with _ -> raise (Compile_Error ("Unbound value " ^ id))

let add (ctxt:ctxt) (id:id) (loc:int) (len:int) =
  ctxt.layout <- (id, (loc, len))::ctxt.layout

let layout_str layout =
  let f x y =
    let (id, (s, l)) = y in
    Printf.sprintf "%s{%s: (%d, %d)}; " x id s l
  in List.fold_left f "" layout

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
let release_loc tape i = Array.set tape i false

let ( *@ ) x n = Array.make n x |> Array.to_list

let move_dist ctxt dist : block =
  let _ = ctxt.ptr := !(ctxt.ptr) + dist in
  if dist > 0 then Right *@ dist
    else Left *@ (abs dist)

let move_to ctxt target : block = (* QUESTION: disable wrapping or no ? *)
  let dist = target - !(ctxt.ptr) in move_dist ctxt dist

let div_mod n d : int * int =
  try (n / d, n mod d)
  with _ -> raise (Compile_Error (Printf.sprintf "%d/%d" n d))

let min_tup t1 t2 = if fst t1 < fst t2 then t1 else t2

let rec gen_eq_as n dx =
  if n < 15 then [n]
  else (
    let bases = List.init 11 (fun x -> x + 4) in
    List.map (gen_base_eq dx n) bases
      |> List.map (fun e -> (eq_len e dx, e))
      |> List.fold_left min_tup (Int.max_int, [])
      |> snd
  )
and gen_base_eq dx n b =
  let (q, r) = div_mod n b in
  (gen_eq_as q dx) @ [b; r]
and eq_len e dx =
  let dx_m = 3 + dx * 3 in
  List.mapi (fun i x -> (i mod 2, x)) e
  |> List.fold_left (fun x (a, b) -> if a == 0 then x + b else x + b + dx_m) 0

let rec gen_num ctxt n : block =
  if n < 15 then Inc *@ n
  else (
    let temp = take_loc ctxt.tape @@ find_avaiable ctxt.tape 0 in
    let dist = temp - !(ctxt.ptr) in
    let eq = gen_eq_as n dist in
    let dir = ref 1 in
    let f i x =
      if i mod 2 == 0 then Inc *@ x
      else (
        let loop = Loop (gen_mul_block ctxt dist x dir) in
        let move = dir := Int.neg !dir; move_dist ctxt (dist * !dir) in
        dir := Int.neg !dir; loop :: move
      )
    in
    release_loc ctxt.tape temp; List.mapi f eq |> List.flatten
  )
and gen_mul_block ctxt dx n dir =
  let m1 = move_dist ctxt (dx * !dir) in
  let inc = Inc *@ n in
  let m2 = dir := Int.neg !dir; move_dist ctxt (dx * !dir) in
  m1 @ inc @ m2 @ [Dec]


let rec gen_str s : block = []


let rec gen_block (ctxt:ctxt) (exp:exp) : block =
  let _ = print_ctxt ctxt in
  match exp with
  | Num i -> gen_num ctxt i
  | Str s -> gen_str s
  | Id id -> lookup ctxt.layout id |> fst |> move_to ctxt
  (* | Bop (bop, e1, e2) -> [] *)
(*   | IsZero e -> [] TODO: need add if else statments *)
  (* | Assn (e1, e2) ->  *)
  | Decl (id, e) -> (* TODO: add non-initalized vars *)
    let new_ptr = take_loc ctxt.tape @@ find_avaiable ctxt.tape 0 in
    let move = (move_to ctxt new_ptr) in
    let init = gen_block ctxt e in
    add ctxt id new_ptr (List.length init);  move @ init
  | _ -> []


let gen_prog (prog:Ast.prog) : string =
  let ctxt = { ptr = ref 0; layout = []; tape = Array.make tape_size false } in
  Bf.string_of_prog @@ List.map (fun x -> gen_block ctxt x) prog

let test_gen () = gen_prog [Decl ("x", Num 130); Decl ("y", Num 30);Num 0]
  (* Bf.string_of_prog [[Right; Inc; Inc;]; [Left; Dec; Out;]] *)
