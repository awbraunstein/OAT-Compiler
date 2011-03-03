open Il
open X86
open Cunit



let compile_three (bb: Il.bb) : X86.insn list =
  begin match bb with
    | _ -> [X86.Mov(Imm 4l,eax)]
  end

let rec compile_two (bb: Il.bb) : Cunit.component =  
  let prologue = [Push(ebp)] @ [Mov(ebp,esp)] in
  let epilogue = [Mov(esp,ebp)] @ [Pop(ebp)] @ [Ret] in 
  let block : Cunit.component =
  Code({X86.global = true; X86.label = bb.bb_lbl; X86.insns=prologue @ compile_three bb @ epilogue}) in
  block

and compile_one (bb_list: Il.bb list) : Cunit.component list =
  let l : Cunit.component list = [] in List.map compile_two bb_list @ l

let compile_prog (prog:Il.prog) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl "program") in
    let return_unit : Cunit.cunit =
      compile_one prog.il_cfg in
    return_unit