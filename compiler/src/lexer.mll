{
  open Lexing
  open Parser

  exception Lexer_Error of string

  let unexpected_char (c:char) =
    raise (Lexer_Error (Printf.sprintf "Unexpected character: '%c'" c))

  let illegal_esc (s:string) =
    raise (Lexer_Error (s ^ " is an illegal escaped character"))

  let newline lexbuf =
    lexbuf.lex_curr_p <- { (lexeme_end_p lexbuf) with
      pos_bol = (lexeme_end lexbuf);
      pos_lnum = (lexeme_end_p lexbuf).pos_lnum + 1 }

  let reserved_words =
    [ ("zero?", ISZERO);
      ("let", LET);
      ("print", PRINT);
      ("if", IF);
      ("else", ELSE); ]

  let symbols =
    [ ("+", PLUS);
      ("-", MINUS);
      ("*", MULT);
      ("=", EQ);
      ("(", LPAREN);
      (")", RPAREN);
      ("{", LBRACE);
      ("}", RBRACE); ]

  let res_sym = reserved_words @ symbols
  let sym_table = Hashtbl.create 1024
  ;; List.iter (fun (str, tok) -> Hashtbl.add sym_table str tok) res_sym

  let create_token str =
    try (Hashtbl.find sym_table str)
    with _ -> IDENT str

  let string_buf = Buffer.create 128
  let add_str (c:char) = Buffer.add_char string_buf c
  let get_str () = Buffer.contents string_buf
  let reset_str () = Buffer.reset string_buf

}

let newline = '\n' | '\r' | ('\r' '\n')
let character = [ 'A'-'Z' 'a'-'z' ]
let whitespace = [ ' ' '\t' ]
let digit = [ '0'-'9' ]

rule token = parse
  | eof { EOF }
  | whitespace+ { token lexbuf }
  | newline { newline lexbuf; token lexbuf }
  | (character | '_') (digit | character | '_')* ('?')? as s { create_token s }
  | digit+ as d { INT (int_of_string d) }
  | '"' { reset_str (); string lexbuf }
  | '+' | '-' | '*' | '=' | '(' | ')' | '{' | '}' as c
    { create_token (Char.escaped c) }
  | _ as c { unexpected_char c }
and string = parse
  | '"' { STRING (get_str ()) }
  | '\\' { add_str (escaped lexbuf); string lexbuf }
  | '\n' { add_str '\n'; newline lexbuf; string lexbuf }
  | eof { raise (Lexer_Error "Missing terminating \" character") }
  | _ as c { add_str c; string lexbuf }
and escaped = parse
  | 'n' { '\n' }
  | 't' { '\t' }
  | '\\' | '"' | '\'' as c { c }
  | ['0'-'9']['0'-'9']['0'-'9'] as d
    { let x = (int_of_string d) in
      if x < 256 then Char.chr x
      else raise (illegal_esc d) }
  | _ as c { illegal_esc (Char.escaped c) }
