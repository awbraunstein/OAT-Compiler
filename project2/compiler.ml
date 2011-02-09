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

  
let rec emit_exp (exp:exp) (stream : insn list) : insn list =
  begin match exp with
    | Cint i -> stream >:: Mov (eax, Imm i)
    | Arg -> stream >:: Mov (eax, edx)
    | Binop (a, l, r) -> binop_aux a l r stream
    | Unop (a, x) -> unop_aux a x stream
  end

and unop_aux (u:unop) (x:exp) (i:insn list): insn list=
  begin match u with
    | Ast.Not -> X86.Not(eax)::emit_exp x []@i
    | Ast.Lognot -> X86.Not(eax)::emit_exp x []@i
    | Ast.Neg -> X86.Neg(eax)::emit_exp x [] @i
  end
  

and binop_aux2 (l:exp)(r:exp) (i:insn list) : insn list = 
  let str_l = 
    (emit_exp r i) >:: (Push eax)
    in
    (emit_exp l str_l);

and binop_aux (b:binop) (l:exp) (r:exp) (i: insn list): insn list=
  begin match b with
    | Plus -> binop_aux2 l r i >::
      (Add(eax, stack_offset (0l))) >:: (Add(esp, Imm 4l))
    | Times ->
      binop_aux2 l r i >::
      (Imul(Eax, stack_offset (0l))) >::
      (Add(esp, Imm 4l))
    | Minus ->
       binop_aux2 l r i >::
      (Sub(eax, stack_offset (0l))) >::
      (Add(esp, Imm 4l))
    | Ast.Eq -> i(* binary equality *)
    | Neq -> i(* binary inequality *)
    | Lt -> i(* binary signed less-than *)
    | Lte -> i(* binary signed less-than or equals *)
    | Gt -> i(* binary signed greater-than *)
    | Gte -> i(* binary signed greater-than or equals *)
    | Ast.And ->
       binop_aux2 l r i >::
      (X86.And(eax, stack_offset (0l))) >::
      (Add(esp, Imm 4l))
    | Ast.Or ->
      binop_aux2 l r i >::
      (X86.Or(eax, stack_offset (0l))) >::
      (Add(esp, Imm 4l))   
    | Ast.Shl ->
      binop_aux2 l r i >::
      (Shl(eax, stack_offset (0l))) >::
      (Add(esp, Imm 4l))
    | Ast.Shr ->
      binop_aux2 l r i >::
      (Shr(eax, stack_offset (0l))) >::
      (Add(esp, Imm 4l))
    | Ast.Sar ->
      binop_aux2 l r i >::
      (Sar(eax, stack_offset (0l))) >::
      (Add(esp, Imm 4l))
  end



let compile_exp (ast:exp) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl "program") in
    let init_str = [Mov (edx, stack_offset 4l)] in
      let insns = List.rev(emit_exp ast init_str >:: X86.Ret)  in
        let block : X86.insn_block = {global = true; label = (mk_lbl_named block_name); insns=insns} in
          let comp = Cunit.Code block in
            [comp]