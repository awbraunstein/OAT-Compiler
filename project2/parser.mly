%{
open Ast;;
%}

/* Declare your tokens here. */
%token EOF
%token <Range.t * int32> INT
%token <Range.t> X        /* X */
%token <Range.t> PLUS     /* + */
%token <Range.t> TIMES    /* '*' */
%token <Range.t> DIGIT


/* ---------------------------------------------------------------------- */
%start toplevel
%type <Ast.exp> toplevel
%type <Ast.exp> exp
%%

toplevel:
  | exp EOF { $1 }

/* Declare your productions here, starting with 'exp'. */

exp:
  |A1 { $1 }

A1:
  |A3 PLUS A3 {Binop (Plus, $1, $3)}
  |A3 { $1 }
A2:
  |A3 TIMES A3 {Binop (Times, $1, $3)}
  |A2 { $1 }
A3:
  |INT {Cint (snd $1) }
  |X   { Arg }