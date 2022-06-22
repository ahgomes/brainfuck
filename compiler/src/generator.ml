open Ast
open Bf

exception Compile_Error of string

(* context -------------------------------------------------- *)

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

(* helper functions -------------------------------------------------- *)

let ( *@ ) x n = Array.make n x |> Array.to_list

let move_dist ctxt dist : block =
  let _ = ctxt.ptr := !(ctxt.ptr) + dist in
  if dist > 0 then Right *@ dist
    else Left *@ (abs dist)

let move_to ctxt target : block = (* QUESTION: disable wrapping or no ? *)
  let dist = target - !(ctxt.ptr) in move_dist ctxt dist

let add_int n : block = if n > 0 then Inc *@ n else Dec *@ (abs n)

let div_mod n d : int * int = (n / d, n mod d)

let bf_print b = b @ [Out]

let min_tup t1 t2 = if fst t1 < fst t2 then t1 else t2

(* generation functions -------------------------------------------------- *)

let rec gen_eq n dx =
  if n < 15 && n > -15 then [n]
  else (
    let bases = List.init 8 (fun x -> x + 4) in
    List.map (gen_base_eq dx n) bases
      |> List.map (fun e -> (eq_len e dx, e))
      |> List.fold_left min_tup (Int.max_int, [])
      |> snd
  )
and gen_base_eq dx n b =
  let (q, r) = div_mod (abs n) b in
  (gen_eq q dx) @ (if n < 0 then [-b; -r] else [b; r])
and eq_len e dx =
  let dx_m = 3 + dx * 3 in
  List.mapi (fun i x -> (i mod 2, x)) e
  |> List.fold_left (fun x (a, b) -> if a == 0 then x + b else x + b + dx_m) 0

let rec gen_num ctxt n : block =
  if n < 15 && n > -15 then add_int n
  else (
    let temp = take_loc ctxt.tape @@ find_avaiable ctxt.tape 0 in
    let dist = temp - !(ctxt.ptr) in
    let eq = gen_eq n dist in
    let dir = ref 1 in
    let init_move = if (List.length eq) mod 4 == 3
      then (dir := Int.neg !dir; move_to ctxt temp)
      else (move_to ctxt !(ctxt.ptr))
    in
    let f i x =
      if i mod 2 == 0 then add_int x
      else (
        let loop = Loop (gen_mul_block ctxt dist x dir) in
        let move = dir := Int.neg !dir; move_dist ctxt (dist * !dir) in
        dir := Int.neg !dir; loop :: move
      )
    in
    let r =  List.mapi f eq |> List.flatten in
    release_loc ctxt.tape temp; init_move @ r
  )
and gen_mul_block ctxt dx n dir =
  let m1 = move_dist ctxt (dx * !dir) in
  let inc = add_int n in
  let m2 = dir := Int.neg !dir; move_dist ctxt (dx * !dir) in
  m1 @ inc @ m2 @ [Dec]

let rec gen_str ctxt s : block =
  let chars = List.init (String.length s) (String.get s) in
  let ords = List.map Char.code chars in
  let f o =
    let new_ptr = take_loc ctxt.tape @@ find_avaiable ctxt.tape 0 in
    let move = (move_to ctxt new_ptr) in
    move @ (gen_num ctxt o)
  in
  List.map f ords |> List.flatten

let rec gen_pstr ctxt s : block =
  let chars = List.init (String.length s) (String.get s) in
  let ords = List.map Char.code chars in
  let prev = bf_print @@ gen_num ctxt (List.hd ords) in
  prev @ gen_pstr_as ctxt (List.hd ords) (List.tl ords)
and gen_pstr_as ctxt prev rest : block =
  match rest with
  | [] -> []
  | hd::tl ->
    let result = gen_num ctxt (hd - prev) in
    (bf_print result) @ (gen_pstr_as ctxt hd tl)

let rec gen_exp (ctxt:ctxt) (exp:exp) : block =
  match exp with
  | Num i -> gen_num ctxt i
  | Str s -> gen_str ctxt s
  | PStr s -> gen_pstr ctxt s
  | Id id -> lookup ctxt.layout id |> fst |> move_to ctxt
  | Bop (bop, e1, e2) -> []
  (*   | IsZero e -> [] TODO: need add if else statments *)
  | Uop (Neg, Num i) -> gen_exp ctxt (Num (-i))
  | Uop (Neg, Id id) -> []
  | _ -> failwith "Invalid expression."

let rec gen_block (ctxt:ctxt) (stmt:stmt) : block =
  let _ = print_ctxt ctxt in
  match stmt with
(*| Assn (e1, e2) ->  *)
  | Decl (id, e) -> (* TODO: add non-initalized vars *)
    let new_ptr = take_loc ctxt.tape @@ find_avaiable ctxt.tape 0 in
    let move = (move_to ctxt new_ptr) in
    let init = gen_exp ctxt e in
    add ctxt id new_ptr (List.length init); move @ init
  | Print (Str s) ->
    let temp = take_loc ctxt.tape @@ find_avaiable ctxt.tape 0 in
    let move = (move_to ctxt temp) in
    let pstr = gen_exp ctxt (PStr s) in
    release_loc ctxt.tape temp; move @ pstr
  | _ -> failwith "Invalid statment."

let gen_prog (prog:Ast.prog) : string =
  let ctxt = { ptr = ref 0; layout = []; tape = Array.make tape_size false } in
  Bf.string_of_prog @@ List.map (fun x -> gen_block ctxt x) prog
