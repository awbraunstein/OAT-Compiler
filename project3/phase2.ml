open Il
open X86
open Cunit

let get_op (op: Il.operand) : X86.opnd =
  begin match op with
    | Imm x -> Imm x
    | Slot x -> Imm 3
  end

let compile_three (bb: Il.bb) : X86.insn =
  begin match bb.bb_body with
    | h::tl ->
      begin match h with
      | BinArith (op1, b, op2) ->
        let op_1 : X86.opnd = get_op op1 in
        let op_2 : X86.opnd = get_op op2 in
        begin match b with
          | Plus -> X86.Add op_1 op_2
        end
      | UnArith (u, op1) ->
        begin match u with
          | Neg -> X86.Neg
        end
      end
  end

let rec compile_two (bb: Il.bb) : Cunit.component =  
  let block : Cunit.component =
  Code({X86.global = true; X86.label = bb.bb_lbl; X86.insns=compile_three bb}) in
  block

and compile_one (bb_list: Il.bb list) : Cunit.component list =
  let l : Cunit.component list = [] in List.map compile_two bb_list @ l

let compile_prog (prog:Il.prog) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl "program") in
    let return_unit : Cunit.cunit = compile_one prog.il_cfg in
    return_unit