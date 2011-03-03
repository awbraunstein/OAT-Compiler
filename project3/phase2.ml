open Il
open X86
open Cunit

let find_op (op: Il.operand) : X86.opnd =
  begin match op with
    | Imm x -> x
    | Slot x -> X86.stack_offset 4*x
  end

let compile_two (bb: Il.bb) : insns_list =
  let block : X86.insn_block =
  {global = true; label = bb.bb_lbl; insns=[]} in
  begin match bb.bb_body with
    | BinArith (op1, b, op2) ->
      let op_1 = find_op op1 in let op_2 = find_op op2 in
      begin match b with
        | Plus -> X86.Add
      end
    end

let compile_one (bb: Il.bb list) =
  begin match bb with
    | h::tl -> compile_two h @ compile_one tl
  end

let compile_prog (prog:Il.prog) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in
    compile_one prog.il_cfg
    
    (*let op1_ = op1 in let op2_ = op2 in
    begin match binop with
      | Plus -> X86.Add find_op 
      | Times -> X86.Imul 
      | Minus -> X86.Sub 
      | Shl -> X86.Shl
      | Shr -> X86.Shr
  end*)