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

let rec find_group tape len start : int list =
  let gp = List.init len (fun i -> find_avaiable tape (start + i)) in
  let f a b = if b-1 = a then b else -1 in
  let con = List.fold_left f (List.hd gp) (List.tl gp) in
  if con = -1 then find_group tape len (start + 1) else gp

let take_loc tape i : int = Array.set tape i true; i
let release_loc tape i = Array.set tape i false

let take_available tape : int =
  take_loc tape @@ find_avaiable tape 0

let take_group tape len : int list =
  List.map (take_loc tape) @@ find_group tape len 0

let release_group tape l = List.iter (release_loc tape) l

(* printing *)
let layout_str layout =
  let f x y =
    let (id, (l, t)) = y in
    let ty = function
      | TInt -> "int"
      | TString -> "string"
      | TBool -> "bool"
      | TPrint -> "print string"
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
  | Str _ -> TString
  | PStr _ -> TPrint
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
    let curr = !(ctxt.ptr) in
    let temp = take_available ctxt.tape in
    let clear =
      (move_to ctxt temp) @ Bf.zero
        @ (move_to ctxt temp; move_to ctxt curr)
    in
    let _ = move_to ctxt curr in
    let dist = temp - curr in
    let eq = get_eq n dist in
    let dir = ref 1 in
    let init_move = if (List.length eq) mod 4 == 3
      then (dir := Int.neg !dir; move_to ctxt temp)
      else (move_to ctxt curr)
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
    release_loc ctxt.tape temp; clear @ init_move @ r
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

let gen_temp_group ctxt len =
  let gp = take_group ctxt.tape len in
  List.map (fun p -> (p, Bf.zero)) gp

(* strings *)
let rec gen_str ctxt s : block =
  if String.length s = 0 then []
  else (
    let chars = List.init (String.length s) (String.get s) in
    let ords = List.map Char.code chars in
    let f i o =
      if i > 0 then (
        let temp, t_val = gen_temp ctxt 0 in
        let move = (move_to ctxt temp) in
        move @ t_val @ (gen_num ctxt o))
      else gen_num ctxt o
    in
    let str = List.mapi f ords |> List.flatten in
    let end_ptr, end_block = gen_temp ctxt 0 in
    str @ (move_to ctxt end_ptr) @ end_block
  )

(* print strings *)
let rec gen_pstr ctxt s : block =
  if String.length s = 0 then [] else (
    let chars = List.init (String.length s) (String.get s) in
    let ords = List.map Char.code chars in
    let prev = gen_num ctxt (List.hd ords) @ Bf.out in
    prev @ gen_pstr_as ctxt (List.hd ords) (List.tl ords) @ Bf.zero
  )
and gen_pstr_as ctxt prev rest : block =
  match rest with
  | [] -> []
  | hd::tl ->
    let result = gen_num ctxt (hd - prev) in
    result @ Bf.out @ (gen_pstr_as ctxt hd tl)

(* print int *)
let gen_print_int ctxt : block =
  let curr = !(ctxt.ptr) in
  let temps = (curr, []) :: (gen_temp_group ctxt 9) in
  let g a b = a @ (move_to ctxt (fst b)) @ snd b in
  let tidx = List.map fst temps in
  let get i = List.nth tidx i in
  let mv f t = move_to ctxt (get f); move_to ctxt (get t) in
  let mvd d = if d >= 0 then (mv 1 (d+1)) else (mv (abs (d-1)) 1) in
  let clear = List.fold_left g [] temps @ (mv 9 0) in
  let ll =
    (mvd 1) @ Bf.dec
    @ Bf.loop ((mvd 1) @ Bf.inc @ (mvd 2)) @ (mvd 1)
    @ Bf.loop (Bf.inc @ Bf.loop (Bf.dec @ (mvd (-1)) @ Bf.inc @ (mvd 1))
      @ (mvd 1) @ Bf.inc @ (mvd 2))
    @ (mvd (-5))
  in
  let l1 = Bf.loop (Bf.dec @ (mv 0 1) @ Bf.inc @ ll @ (mv 1 0)) in
  let l2 = Bf.loop (Bf.dec @ ll) in
  let divmod =
    (mv 0 2) @ (add_int 10) @ (mv 2 0) @ l1 @ (mv 0 2) @ Bf.zero
      @ (mv 2 5) @ (add_int 10) @ (mv 5 4) @ l2 @ (mv 4 5) @ Bf.zero
  in
  let n48 i =
    (add_int 6) @ Bf.loop (Bf.dec @ (mvd (-i)) @ (add_int 8) @ (mvd i))
  in
  let l3 =
    Bf.loop ((mvd 1) @ (n48 1) @ (mvd (-1)) @ Bf.out @ (mvd (-2)) @ Bf.inc
      @ (mvd 1) @ Bf.inc @ (mvd 1) @ Bf.zero )
  in
  let l4 =
    Bf.loop ((mvd (-1)) @ Bf.loop (Bf.dec @ (mvd 1) @ Bf.dec @ (mvd (-1)))
      @ (n48 (-1)) @ (mvd 1) @ Bf.out @ Bf.zero)
  in
  let print =
    (mv 5 7) @ l3 @ (mv 7 6) @ l4 @ (mv 6 4) @ (n48 1) @ (mv 4 3)
      @ Bf.out @ Bf.zero
  in
  let l5 = Bf.loop (Bf.dec @ (mv 1 0) @ Bf.inc @ (mv 0 1)) in
  let return = (mv 3 1) @ l5 @ (mv 1 0) in
  release_group ctxt.tape (List.tl tidx);
  clear @ divmod @ print @ return

(* compile expressions *)
let rec gen_exp (ctxt:ctxt) (exp:exp) : Ast.ty * Bf.block =
  match exp with
  | Int i -> (TInt, gen_num ctxt i)
  | Str s -> (TString, gen_str ctxt s)
  | PStr p -> (TPrint, gen_pstr ctxt p)
  | Id id -> let ptr, ty = lookup ctxt.layout id in (ty, move_to ctxt ptr)
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
  (* | Uop of unop * exp *)
  | IsZero e -> (* FIXME: overwrites original value problem if id *)
    let ty, block = gen_exp ctxt e in
    if ty <> TInt then failwith "zero? only accepts integer values"
    else (
      let curr = !(ctxt.ptr) in
      let temp, t_val = gen_temp ctxt 0 in
      let mv_t = move_to ctxt temp in
      let mv_c = move_to ctxt curr in
      let l1 = Bf.loop (mv_t @ Bf.inc @ mv_c @ Bf.zero) in
      let l2 = Bf.loop (mv_c @ Bf.dec @ mv_t @ Bf.dec) in
      let result =
        block @ mv_t @ t_val @ mv_c @ l1 @ Bf.inc @ mv_t @ l2 @ mv_c
      in
      release_loc ctxt.tape temp; (TBool, result) )
  | _ -> failwith "not implemented."

(* compile statments *)
let rec gen_block (ctxt:ctxt) (stmt:Ast.stmt) : Bf.block =
  match stmt with
  (* | Assn (e1, e2) -> *)
  | Decl (id, e) -> (* TODO: add non-initalized vars *)
    let new_ptr = take_available ctxt.tape in
    let move = move_to ctxt new_ptr in
    let ty, block = gen_exp ctxt e in
    add ctxt id new_ptr ty; move @ block
  | Print e ->
    let temp, t_val = gen_temp ctxt 0 in
    let mv_t = move_to ctxt temp in
    let ty, block = begin match e with
      | Int i -> gen_exp ctxt (PStr (string_of_int i))
      | Str s -> gen_exp ctxt (PStr s)
      | _ -> gen_exp ctxt e
    end in
    let p_block = begin match ty with
      | TBool ->
        let ifs = If (e, [Print(Str "true")], [Print(Str "false")]) in
        move_to ctxt temp; (gen_block ctxt ifs) @ Bf.out
      | TInt -> block @ gen_print_int ctxt
      | TString -> block @ Bf.loop (Bf.out @ (move_dist ctxt 1))
      | _ -> block @ Bf.out
    end in
    release_loc ctxt.tape temp; move_to ctxt temp;
    mv_t @ t_val @ p_block
  | If (e, s1, s2) ->
    let temp1, t_val1 = gen_temp ctxt 0 in
    let mt1 = move_to ctxt temp1 @ t_val1 in
    let temp2, t_val2 = gen_temp ctxt 0 in
    let mt2 = move_to ctxt temp2 @ t_val2 in
    let temp3, t_val3 = gen_temp ctxt 0 in
    let ty, block = move_to ctxt temp3 @ t_val3; gen_exp ctxt e in
    if ty <> TBool then failwith "if requires boolean expression"
    else (
      let curr = !(ctxt.ptr) in
      let mv_t1 = move_to ctxt temp1 in
      let mv_t2 = move_to ctxt temp2 in
      let mv_c = move_to ctxt curr in
      let mv_t2_t1 = move_to ctxt temp2; move_to ctxt temp1 in
      let mv_c_t1 = move_to ctxt curr; move_to ctxt temp1 in
      let mv_t1_c = move_to ctxt temp1; move_to ctxt curr in
      let l1 = Bf.loop (mv_t1 @ Bf.inc @ mv_t2 @ Bf.inc @ mv_c @ Bf.dec) in
      let l2 = Bf.loop (mv_t1_c @ Bf.inc @ mv_t1 @ Bf.dec) in
      let f s = List.map (gen_block ctxt) s |> List.flatten in
      let b1 = f s1 in
      let b2 = f s2 in
      let setup = mt1 @ mt2 @ mv_c @ block in
      let ifcheck = l1 @ mv_t1 @ l2 @ Bf.inc @ mv_t2 in
      let ifblock = Bf.loop (b1 @ mv_t2_t1 @ Bf.dec @ mv_t2 @ Bf.zero) in
      let elseblock = mv_t2_t1 @ Bf.loop (mv_t1_c @ b2 @ mv_c_t1 @ Bf.dec) in
      release_group ctxt.tape [temp1; temp2; temp3];
      setup @ ifcheck @ ifblock @ elseblock
    )
  | _ -> failwith "not implemented."

let gen_prog (prog:Ast.prog) : string =
  let ctxt = { ptr = ref 0; layout = []; tape = Array.make tape_size false } in
  Bf.string_of_prog @@ List.map (gen_block ctxt) prog
