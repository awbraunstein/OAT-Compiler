(* compiler.ml *)
(* A compiler for simple arithmetic expressions. *)

(******************************************************************************)

open Printf
open Ast
open X86   (* Note that Ast has similarly named constructors that must be 
              disambiguated.  For example: Ast.Shl vs. X86.Shl *)

(* Parse an AST from a preexisting lexbuf. 
 * The filename is used to generate error messages.
*)
let parse (filename : string) (buf : Lexing.lexbuf) : exp =
  try
    Lexer.reset_lexbuf filename buf;
    Parser.toplevel Lexer.token buf
  with Parsing.Parse_error ->
    failwith (sprintf "Parse error at %s."
        (Range.string_of_range (Lexer.lex_range buf)))



(* Builds a globally-visible X86 instruction block that acts like the C fuction:

   int program(int X) { return <expression>; }

   Follows cdecl calling conventions and platform-specific name mangling policy. *)
   
let compile_aux (e:exp) (stream : insn list) : insn list =
  begin match e with
    | Cint i -> Mov (eax, Imm i) :: stream
    | Arg -> Mov (eax, edx) :: stream
    | Binop (op, e1, e2) -> binop_aux op e1 e2 :: stream
  end
  
and let binop_aux (op:binop) (e1:exp) (e2:exp) (stream : insn list) : insn list =
  let stream' = [] in
  compile_exp e1 stream :: stream';
  Push(eax) :: stream';
  compile_exp e2 stream';
  begin match op with
    | Plus -> Add(Eax, (X86.stack_offset 0l)) :: stream'; List.rev(stream'); Pop();
  end

let compile_exp (ast:exp) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl "program") in
    Mov (Edx, stack_offset 4);
    compile_aux ast;