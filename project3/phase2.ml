open Il
open X86
open Cunit

let get_op (op: Il.operand) : X86.opnd =
  begin match op with
    | Il.Imm x -> X86.Imm x
    | Il.Slot (x,y) -> (X86.stack_offset (Int32.of_int(4*x)))
  end


let compile_four (i: Il.insn) : X86.insn =
  begin match i with
  | BinArith (op1, b, op2) ->
    let o1 = get_op op1 in let o2 = get_op op2 in
      begin match b with
        | Il.Plus -> X86.Add(o1,o2)
        | Il.Times -> X86.Imul(Eax,o2)
        | Il.Minus -> X86.Sub(o1,o2)
        | Il.Shl -> X86.Shl(o1,o2)
        | Il.Shr -> X86.Shr(o1,o2)
        | Il.Sar -> X86.Sar(o1,o2)
        | Il.And -> X86.And(o1,o2)
        | Il.Or -> X86.Or(o1,o2)
        | Il.Xor -> X86.Xor(o1,o2)
        | Il.Compare c ->
          begin match c with
            | Il.Eq -> X86.Cmp(o1,o2)
            | Il.Neq -> X86.Cmp(o1,o2)
            | Il.Lt -> X86.Cmp(o1,o2)
            | Il.Lte -> X86.Cmp(o1,o2)
            | Il.Gt -> X86.Cmp(o1,o2)
            | Il.Gte -> X86.Cmp(o1,o2)
          end
        | Il.Move -> X86.Mov(o1,o2)
      end
    | UnArith (u, op) ->
      let o1 = get_op op in
      begin match u with
        | Il.Neg -> X86.Neg(o1)
        | Il.Not -> X86.Not(o1)
        | Il.Lognot -> X86.Not(o1)
      end
  end
  
  
let compile_three (bb: Il.bb) : X86.insn list =
  let r : X86.insn list = [] in List.map compile_four bb.bb_body @ r

let rec compile_two (bb: Il.bb) : Cunit.component =  
  let prologue = [Push(ebp)] @ [Mov(ebp,esp)] in
    let epilogue = [Mov(esp,ebp)] @ [Pop(ebp)] @ [Ret] in 
      let block : Cunit.component =
        Code({X86.global = true; X86.label = bb.bb_lbl;
          X86.insns=prologue @ compile_three bb  @ epilogue}) in
            block

and compile_one (bb_list: Il.bb list) : Cunit.component list =
  let l : Cunit.component list = [] in List.map compile_two bb_list @ l

let compile_prog (prog:Il.prog) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl "program") in
    let return_unit : Cunit.cunit =
      compile_one prog.il_cfg in return_unit