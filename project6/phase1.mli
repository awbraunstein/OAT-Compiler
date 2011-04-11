(* phase1.mli *)

(* Parses an expression from the given lexbuf.  

   The first argument is the filename (or "stdin") from
   which the program is read -- it is used to generate
   error messages.
*)
val parse : string -> Lexing.lexbuf -> Range.t Ast.prog

(* Builds a globally-visible X86 instruction block that acts like the C fuction:
   int program() { <prog> }
   Follows cdecl calling conventions and platform-specific name mangling policy. *)
val compile_prog : Range.t Ast.prog -> Il.prog
