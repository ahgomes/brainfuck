open Ast
open Bf

exception Compile_Error of string

(* context -------------------------------------------------- *)

let tape_size = 30000

type layout = (Ast.id * (int * Ast.ty)) list
type ctxt = { ptr : int ref; mutable layout : layout; tape : bool array }

(* layout *)
let lookup (l:layout) (id:id) : int * Ast.ty =
  try (List.assoc id l)
  with _ -> raise (Compile_Error ("Unbound value " ^ id))

let add (ctxt:ctxt) (id:id) (loc:int) (ty:ty) =
  ctxt.layout <- (id, (loc, ty))::ctxt.layout

(* tape *)
let rec find_avaiable tape i : int =
  try
    if tape.(i) then find_avaiable tape (i+1)
    else i
  with _ -> raise (Compile_Error ("Memory overflow!"))

let take_loc tape i : int = Array.set tape i true; i
let release_loc tape i = Array.set tape i false

let take_available tape : int =
  take_loc tape @@ find_avaiable tape 1

(* printing *)
let layout_str layout =
  let f x y =
    let (id, (l, t)) = y in
    let ty = function
      | TInt -> "int"
      | TString -> "string"
      | TBool -> "bool"
    in
    Printf.sprintf "%s{%s: (%d, <%s>)}; " x id l (ty t)
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

(* helper functions -------------------------------------------------- *)

(* general *)
let ( *@ ) x n = Array.make n x |> Array.to_list

let fstt t = let (f, _, _) = t in f
let sndt t = let (_, s, _) = t in s
let thdt t = let (_, _, th) = t in th

(* type checking *)
let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Sub | Mul -> (TInt, TInt, TInt)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg -> (TInt, TInt)

let typ_of_exp ctxt exp : Ast.ty =
  match exp with
  | Int _ -> TInt
  | Str _ | PStr _ -> TString
  | Id id -> snd (lookup ctxt.layout id)
  | Uop (uop, _) -> snd (typ_of_unop uop)
  | Bop (bop, _, _) -> thdt (typ_of_binop bop)
  | IsZero _ -> TBool

(* binop *)
let get_int_bop = function
  | Add -> Int.add
  | Sub -> Int.sub
  | Mul -> Int.mul

(* move pointer *)
let move_dist ctxt dist : block =
  let _ = ctxt.ptr := !(ctxt.ptr) + dist in
  if dist > 0 then Right *@ dist
    else Left *@ (abs dist)

let move_to ctxt target : block =
  let dist = target - !(ctxt.ptr) in move_dist ctxt dist

(* math *)
let add_int n : block = if n > 0 then Inc *@ n else Dec *@ (abs n)

let div_mod n d : int * int = (n / d, n mod d)

let min_tup t1 t2 = if fst t1 < fst t2 then t1 else t2

let rec get_eq n dx =
  if n < 15 && n > -15 then [n]
  else (
    let bases = List.init 8 (fun x -> x + 4) in
    List.map (get_base_eq dx n) bases
      |> List.map (fun e -> (eq_len e dx, e))
      |> List.fold_left min_tup (Int.max_int, [])
      |> snd
  )
and get_base_eq dx n b =
  let (q, r) = div_mod (abs n) b in
  (get_eq q dx) @ (if n < 0 then [-b; -r] else [b; r])
and eq_len e dx =
  let dx_m = 3 + dx * 3 in
  List.mapi (fun i x -> (i mod 2, x)) e
  |> List.fold_left (fun x (a, b) -> if a == 0 then x + b else x + b + dx_m) 0


(* generation functions -------------------------------------------------- *)

(* numbers *)
let rec gen_num ctxt n : block =
  if n < 15 && n > -15 then add_int n
  else (
    let temp = take_available ctxt.tape in
    let dist = temp - !(ctxt.ptr) in
    let eq = get_eq n dist in
    let dir = ref 1 in
    let init_move = if (List.length eq) mod 4 == 3
      then (dir := Int.neg !dir; move_to ctxt temp)
      else (move_to ctxt !(ctxt.ptr))
    in
    let f i x =
      if i mod 2 == 0 then add_int x
      else (
        let loop = Bf.loop (gen_mul_block ctxt dist x dir) in
        let move = dir := Int.neg !dir; move_dist ctxt (dist * !dir) in
        dir := Int.neg !dir; loop @ move
      )
    in
    let r =  List.mapi f eq |> List.flatten in
    release_loc ctxt.tape temp; init_move @ r
  )
and gen_mul_block ctxt dx n dir =
  let m1 = move_dist ctxt (dx * !dir) in
  let inc = add_int n in
  let m2 = dir := Int.neg !dir; move_dist ctxt (dx * !dir) in
  m1 @ inc @ m2 @ Bf.dec

(* temporaries *)
let gen_temp ctxt i =
  let ptr = take_available ctxt.tape in
  let add = gen_num ctxt i in
  (ptr, Bf.zero @ add)

(* strings *)
let rec gen_str ctxt s : block =
  let chars = List.init (String.length s) (String.get s) in
  let ords = List.map Char.code chars in
  let f i o =
    if i > 0 then (
      let new_ptr = take_available ctxt.tape in
      let move = (move_to ctxt new_ptr) in
      move @ (gen_num ctxt o))
    else gen_num ctxt o
  in
  List.mapi f ords |> List.flatten

(* print strings *)
let rec gen_pstr ctxt s : block =
  let chars = List.init (String.length s) (String.get s) in
  let ords = List.map Char.code chars in
  let prev = bf_print @@ gen_num ctxt (List.hd ords) in
  prev @ gen_pstr_as ctxt (List.hd ords) (List.tl ords) @ bf_zero
and gen_pstr_as ctxt prev rest : block =
  match rest with
  | [] -> []
  | hd::tl ->
    let result = gen_num ctxt (hd - prev) in
    (bf_print result) @ (gen_pstr_as ctxt hd tl)

(* compile expressions *)
let rec gen_exp (ctxt:ctxt) (exp:exp) : Ast.ty * Bf.block =
  let _ = print_ctxt ctxt in
  match exp with
  (* | Str of string
  | PStr of string
  | Uop of unop * exp *)
  | Int i -> (TInt, gen_num ctxt i)
  | Id id ->
    let ptr, ty = lookup ctxt.layout id in
    (ty, move_to ctxt ptr)
  | Bop (bop, e1, e2) ->
    let in1, in2, _ = typ_of_binop bop in
    let ty1 = typ_of_exp ctxt e1 in
    let ty2 = typ_of_exp ctxt e2 in
    if ty1 <> in1 || ty2 <> in2 then raise (Compile_Error "Missmatched types.")
    else (
      begin match (e1, e2) with
      | (Int i1, Int i2) -> Int ((get_int_bop bop) i1 i2) |> gen_exp ctxt
      | _ -> failwith "not implemented."
      end
    )
  | IsZero e ->
    let _, block = gen_exp ctxt e in
    let curr = !(ctxt.ptr) in
    let temp, t_val = gen_temp ctxt 0 in
    let mv_t = move_to ctxt temp in
    let mv_c = move_to ctxt curr in
    let l1 = Bf.loop (mv_t @ Bf.inc @ mv_c @ Bf.zero) in
    let l2 = Bf.loop (mv_c @ Bf.dec @ mv_t @ Bf.dec) in
    let result = block @ mv_t @ t_val @ mv_c @ l1 @ Bf.inc @ mv_t @ l2 @ mv_c in
    release_loc ctxt.tape temp; (TBool, result)
  | _ -> failwith "not implemented."

(* compile statments *)
let rec gen_block (ctxt:ctxt) (stmt:Ast.stmt) : Bf.block =
  let _ = print_ctxt ctxt in
  match stmt with
  (* | Assn (e1, e2) -> *)
  | Decl (id, e) -> (* TODO: add non-initalized vars *)
    let new_ptr = take_available ctxt.tape in
    let move = move_to ctxt new_ptr in
    let ty, block = gen_exp ctxt e in
    add ctxt id new_ptr ty; move @ block
  (* | If of exp * stmt list * stmt list *)
  | Print e -> let ty, block = gen_exp ctxt e in block @ Bf.out
  | _ -> failwith "not implemented."

let gen_prog (prog:Ast.prog) : string =
  let ctxt = { ptr = ref 0; layout = []; tape = Array.make tape_size false } in
  Bf.string_of_prog @@ List.map (fun x -> gen_block ctxt x) prog
