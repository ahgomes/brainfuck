let ic = open_in (Sys.argv.(1))
let code = really_input_string ic (in_channel_length ic)
;; flush stdout; close_in ic

let mem = Array.make 30000 0
let ptr = ref 0
let loop_stack = Stack.create ()

let rec match_right i left =
  let cmd = code.[i] in
  match cmd with
  | '[' -> match_right (i+1) (left+1)
  | ']' -> if left = 0 then i else match_right (i+1) (left-1)
  | _ -> match_right (i+1) left

let rec interp i =
  if i >= String.length code then () else (
  let cmd = code.[i] in
  match cmd with
    | '>' -> ptr := !ptr + 1; interp (i + 1)
    | '<' -> ptr := !ptr - 1; interp (i + 1)
    | '+' -> Array.set mem !ptr (mem.(!ptr) + 1); interp (i + 1)
    | '-' -> Array.set mem !ptr (mem.(!ptr) - 1); interp (i + 1)
    | '.' -> print_char @@ Char.chr mem.(!ptr); interp (i + 1)
    | ',' -> flush stdout; Array.set mem !ptr @@ Char.code (input_char stdin);
             interp (i + 1)
    | '[' -> if mem.(!ptr) = 0 then interp @@ (match_right (i+1) 0) + 1
             else (Stack.push i loop_stack; interp (i + 1))
    | ']' -> interp (Stack.pop loop_stack)
    | '?' -> print_int mem.(!ptr); interp (i + 1) (* for testing *)
    | _ -> if (i+2) < (String.length code) then interp (i + 1) else () )

;; interp 0
