%{
open Ast;;
%}

/* Declare your tokens here. */
%token EOF
%token <Range.t * int32> INT
%token <Range.t> X        /* X */


/* ---------------------------------------------------------------------- */
%start toplevel
%type <Ast.exp> toplevel
%type <Ast.exp> exp
%%

toplevel:
  | exp EOF { $1 }
  | exp Plus {exp + exp}

/* Declare your productions here, starting with 'exp'. */

exp:
| X   { Arg }