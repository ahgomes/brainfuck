open Generator

let format s =
  let add_nl i ch =
    let end_ch = if (i + 1) mod 80 = 0 then "\n" else "" in
    (String.make 1 ch) ^ end_ch
  in
  List.init (String.length s) (String.get s)
  |> List.mapi add_nl
  |> String.concat ""

let cmp oc =
  Lexing.from_channel (open_in Sys.argv.(1))
  |> Parser.prog Lexer.token
  |> gen_prog
  |> format
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
