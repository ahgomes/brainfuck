%{
open Ast
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token LPAREN RPAREN
%token PLUS MINUS MULT
%token EQ
%token LET
%token ISZERO
%token EOF

%left PLUS MINUS
%left MULT
%right EQ
%nonassoc LPAREN

%start prog
%type <Ast.prog> prog
%type <Ast.exp> exp
%%

prog:
  | p = list(exp) EOF { p }

%inline bop:
  | PLUS { Add }
  | MINUS { Sub }
  | MULT { Mul }

exp:
  | i=INT { Num i }
  | s=STRING { Str s }
  | id=IDENT { Id id }
  | e1=exp op=bop e2=exp { Bop (op, e1, e2) }
  | ISZERO e=exp { IsZero e }
  | e1=exp EQ e2=exp { Assn (e1, e2) }
  | LET id=IDENT EQ e=exp { Decl (id, e) }
  | LPAREN e=exp RPAREN { e }
