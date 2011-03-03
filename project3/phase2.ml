open Il
open X86
open Cunit

(*let find_op (op: Il.operand) : X86.opnd =
  begin match op with
    | Imm x -> x
    | Slot x -> X86.stack_offset 4*x
  end*)


let compile_three (bb: Il.insn) =
 begin match bb with
  | _ -> []
  end

let rec compile_two (bb: Il.bb) =  
  let block : X86.insn_block =
  {X86.global = true; X86.label = bb.bb_lbl; X86.insns=[]} in
  begin match bb.bb_body with
    | h::tl -> compile_three h @ compile_one tl
  end

and let rec compile_one (bb: Il.bb list) =
  begin match bb with
    | h::tl -> compile_two h @ compile_one tl @ epilogue
  end

let compile_prog (prog:Il.prog) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in
   compile_one prog.il_cfg