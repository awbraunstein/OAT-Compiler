(* astlib.mli *)

(* Helper functions of abstract syntax of trees. *)
(******************************************************************************)

open Ast

(* Printer of AST, AST to ML, and AST to C*)
val string_of_unop : Range.t unop -> string
val string_of_binop : Range.t binop -> string

val print_typ : typ -> unit
val string_of_typ : typ -> string
val ml_string_of_typ : typ -> string

val print_exp : Range.t exp -> unit
val string_of_exp : Range.t exp -> string
val ml_string_of_exp : Range.t exp -> string

val print_stmt : Range.t stmt -> unit
val string_of_stmt : Range.t stmt -> string
val ml_string_of_stmt : Range.t stmt -> string

val print_prog : Range.t prog -> unit
val string_of_prog : Range.t prog -> string
val c_print_prog : Range.t prog -> unit
val c_string_of_prog : Range.t prog -> string
val ml_string_of_prog : Range.t prog -> string

val c_run_prog : Range.t prog -> string -> string

(* Checking equivalence of AST *)
val eq_prog : Range.t prog -> Range.t prog -> bool

(* File informatin of AST *)
val mk_parse_range : Range.t -> Range.t -> Range.t
val unop_info : 'a unop -> 'a
val binop_info : 'a binop -> 'a
val const_info : 'a const -> 'a
val exp_info : Range.t exp -> Range.t
val lhs_info : Range.t lhs -> Range.t
val init_info : Range.t init -> Range.t
