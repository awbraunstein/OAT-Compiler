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
let (>::) x y =y::x  

let test1 : insn list = [Mov (edx, stack_offset (4l))] @ [Mov (eax, edx)] @ [Ret]

  
let rec emit_exp (exp:exp) (stream : insn list) : insn list =
  begin match exp with
    | Cint i -> stream >:: Mov (eax, Imm i)
    | Arg -> Mov (eax, edx)::stream
    (*| Binop (a, l, r) -> binop_aux a l r stream*)
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
(*  
and binop_aux2(x:exp)(y:exp)(i:insn list):insn list=
  emit_exp y []@Mov(ecx, eax)::emit_exp x []@i
  *)
(*  
and binop_aux (b:binop) (l:exp) (r:exp) (i: insn list): insn list=
  begin match b with
    | Plus ->
      let str_l = 
        (emit_exp l i) >:: (Push eax)
      in
      (emit_exp r str_l) >::
      (Add(eax, stack_offset (4l))) >::
      (Add(esp, Imm 4l))
  end
*)
let compile_exp (ast:exp) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl "program") in
    let init_str = [Mov (edx, stack_offset (4l))] in
     (* let insns = List.rev(emit_exp ast init_str >:: X86.Ret)  in*)
    let insns = test1 in
        let block : X86.insn_block = {global = true; label = (mk_lbl_named block_name); insns=insns} in
          let comp = Cunit.Code block in
            [comp]
      