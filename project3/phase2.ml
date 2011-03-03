open Il
open X86
open Cunit

(*let find_op (op: Il.operand) : X86.opnd =
  begin match op with
    | Imm x -> x
    | Slot x -> X86.stack_offset 4*x
  end*)


let compile_three (bb: Il.bb) =
 begin match bb with
  | _ -> []
  end

let rec compile_two (bb: Il.bb) : X86.insn_block =  
  let block : X86.insn_block =
  {X86.global = true; X86.label = bb.bb_lbl; X86.insns=compile_three bb} in
  block

and compile_one (bb_list: Il.bb list) : Cunit.component list =
  let l : Cunit.component list = [] in
  begin match bb_list with
    | h::tl -> compile_two h @ compile_one tl @ l
  end

let compile_prog (prog:Il.prog) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl "program") in
    let return_unit : Cunit.cunit = [] in
    return_unit = compile_one prog.il_cfg