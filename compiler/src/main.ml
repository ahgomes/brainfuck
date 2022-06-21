open Generator

let cmp oc =
  Lexing.from_channel (open_in Sys.argv.(1))
  |> Parser.prog Lexer.token
  |> gen_prog
  |> output_string oc
  ; flush stdout; close_out oc

let main () =
  let out =
    if Array.length Sys.argv > 2
    then Sys.argv.(2)
    else "a.out"
  in
  out |> open_out |> cmp

;; main ()
