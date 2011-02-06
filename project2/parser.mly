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
%token <Range.t> MINUS
%token <Range.t> OR
%token <Range.t> SHL
%token <Range.t> SHR
%token <Range.t> SAR
%token <Range.t> LT
%token <Range.t> LTE
%token <Range.t> GT
%token <Range.t> GTE
%token <Range.t> EQ
%token <Range.t> NEQ
%token <Range.t> AND
%token <Range.t> NEG
%token <Range.t> LOGNOT
%token <Range.t> NOT
%token <Range.t> LPAREN
%token <Range.t> RPAREN


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
  | A2 OR A2 {Binop (Or, $1, $3)}
  | A2 { $1 }
A2:
  | A1 AND A2 {Binop (And, $1, $3)}
  | A3 { $1 }
A3:
  | A4 NEQ A4 {Binop (Neq, $1, $3)}
  | A4 { $1 }
A4:
  | A5 EQ A5 {Binop (Eq, $1, $3) }
  | A5 { $1 }
A5:
  | A6 LT A6 {Binop (Lt, $1, $3) }
  | A6 { $1 }
A6:
  | A7 LTE A7 {Binop (Lte, $1, $3) }
  | A7 { $1 }
A7:
  | A8 GT A8 {Binop (Gt, $1, $3) }
  | A8 { $1 }
A8:
  | A9 GTE A9 {Binop (Gte, $1, $3) }
  | A9 { $1 }
A9:
  | A10 SHL A10 {Binop (Shl, $1, $3) }
  | A10 { $1 }
A10:
  | A11 SHR A11 {Binop (Shr, $1, $3) }
  | A11 { $1 }
A11:
  | A12 SAR A12 {Binop (Sar, $1, $3) }
  | A12 { $1 }
A12:
  | A13 PLUS A13 {Binop (Plus, $1, $3) }
  | A13 { $1 }
A13:
  | A14 MINUS A14 {Binop (Minus, $1, $3) }
  | A14 { $1 }
A14:
  | A15 TIMES A15 {Binop (Times, $1, $3) }
  | A15 { $1 }
A15:
  | NEG A16 {Unop (Neg, $2) }
  | A16 { $1 }
A16:
  | LOGNOT A17 {Unop (Lognot, $2) }
  | A17 { $1 }
A17:
  | NOT A18 {Unop (Not, $2) }
  | A18 { $1 }
A18:
  | INT {Cint (snd $1) }
  | X   { Arg }
  | LPAREN A1 RPAREN {$2}