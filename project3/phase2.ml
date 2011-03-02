open Il
open X86
open Cunit

let find_op (op: Il.operand) =
  begin match op with
    | Imm x -> x
    | Slot x -> X86.stack_offset 4*x
  end

let compile_one (bb: Il.bb list) =
  begin match bb with
    | h::tl -> compile_two h
  end

let compile_prog (prog:Il.prog) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in
    compile_one prog.il_cfg