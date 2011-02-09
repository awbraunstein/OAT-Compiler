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
  


  
let rec emit_exp (exp:exp) (stream : insn list) : insn list =
  begin match exp with
    | Cint i -> Mov (eax, Imm i) :: stream
    | Arg -> Mov (eax, edx) :: stream
    | Binop (a, x, y) -> binop_aux a x y stream
    (*| Unop (a, x) -> unop_aux a x stream*)
  end
(*  
and unop_aux (u:unop) (x:exp) (i:insn list): insn list=
  begin match u with
    | Not -> X86.Not(eax)::emit_exp x []@i
    | Lognot -> X86.Not(eax)::emit_exp x []@i
    | Neg -> X86.Neg(eax)::emit_exp x [] @i
  end
  *)
  
and binop_aux2(x:exp)(y:exp)(i:insn list):insn list=
  emit_exp y []@Mov(ecx, eax)::emit_exp x []@i
  
and binop_aux (b:binop) (x:exp) (y:exp) (i: insn list): insn list=
  begin match b with
    | Plus -> X86.Add(eax, ecx)::binop_aux2 x y i
    | Minus -> X86.Sub(eax, ecx)::binop_aux2 x y i
    | Times -> X86.Imul(Eax, ecx)::binop_aux2 x y i
    | Ast.And -> X86.And(eax, ecx)::binop_aux2 x y i
    | Ast.Or  -> X86.Or(eax,ecx)::binop_aux2 x y i
    | Ast.Shl -> X86.Shl(eax,ecx)::binop_aux2 x y i
    | Ast.Shr -> X86.Shr(eax,ecx)::binop_aux2 x y i
    | Ast.Sar -> X86.Sar(eax,ecx)::binop_aux2 x y i
  end

let compile_exp (ast:exp) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl "program") in
    Mov (edx, stack_offset 4l);
      let stream = [] in
        let c = emit_exp ast stream in
          let block : X86.insn_block = X86.mk_block block_name c in
            let comp : Cunit.component = block in
              let unit : Cunit.cunit = [comp] in 
                unit;
      