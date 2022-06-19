open Generator

let ic = open_in Sys.argv.(1)
(* let code = really_input_string ic (in_channel_length ic) *)
;; flush stdout; close_in ic

let code = test_gen ()

let wr oc =
  output_string oc code; flush stdout;
  close_out oc

let main () =
  let out =
    if Array.length Sys.argv > 2
    then Sys.argv.(2)
    else "a.out"
  in
  out |> open_out |> wr

;; main ()
