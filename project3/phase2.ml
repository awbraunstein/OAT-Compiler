open Il
open X86
open Cunit

let find_ops (op: Il.operand) =
  begin match op with
    | Imm x -> x
    | Slot x ->

let compile_three

let compile_two

let compile_one (bb: Il.bb list) (uid: Il.uid list)=
  begin match bb with
    | h::tl -> compile_two h


let compile_prog (prog:Il.prog) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in
compile_one prog.il_cfg prof.il_temps