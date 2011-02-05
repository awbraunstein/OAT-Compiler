%{
open Ast;;
%}

/* Declare your tokens here. */
%token EOF
%token <Range.t * int32> INT
%token <Range.t> X        /* X */
%token <Range.t> PLUS


/* ---------------------------------------------------------------------- */
%start toplevel
%type <Ast.exp> toplevel
%type <Ast.exp> exp
%%

toplevel:
  | exp EOF { $1 }

/* Declare your productions here, starting with 'exp'. */

exp:
| X   { Arg }

B1:
  |B2 PLUS B2 {Binop (Plus, $1, $3)}
  |B2 { $1 }
B2:
  |INT {Cint (snd $1)}
  |X {Arg}