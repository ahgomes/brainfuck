%{
open Ast
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token LPAREN RPAREN
%token LBRACE RBRACE
%token PLUS MINUS MULT
%token EQ
%token LET
%token ISZERO
%token PRINT
%token IF ELSE
%token EOF

%left PLUS MINUS
%left MULT
%right EQ

%start prog
%type <Ast.prog> prog
%type <Ast.stmt> stmt
%%

prog:
  | p = list(stmt) EOF { p }

%inline uop:
  | MINUS { Neg }

%inline bop:
  | PLUS { Add }
  | MINUS { Sub }
  | MULT { Mul }

exp:
  | i=INT { Int i }
  | s=STRING { Str s }
  | id=IDENT { Id id }
  | e1=exp op=bop e2=exp { Bop (op, e1, e2) }
  | u=uop e=exp { Uop (u, e) }
  | ISZERO e=exp { IsZero e }
  | LPAREN e=exp RPAREN { e }

stmt:
  | e1=exp EQ e2=exp { Assn (e1, e2) }
  | LET id=IDENT EQ e=exp { Decl (id, e) }
  | PRINT e=exp { Print e }
  | ifs=if_stmt { ifs }

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

if_stmt:
  | IF LPAREN e=exp RPAREN b1=block b2=else_stmt { If(e,b1,b2) }

else_stmt:
  | (* empty *)       { [] }
  | ELSE b=block      { b }
  | ELSE ifs=if_stmt  { [ ifs ] }
