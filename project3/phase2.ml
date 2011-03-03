open Il
open X86
open Cunit

let get_op (op: Il.operand) : X86.opnd =
  begin match op with
    | Il.Imm (x) -> X86.Imm x
    | Il.Slot (x,y) -> (X86.stack_offset (Int32.of_int(-4-(4*x))))
  end

let compile_four (i: Il.insn) : X86.insn list =
  begin match i with
  | BinArith (op1, b, op2) ->
    let o1 = get_op op1 in let o2 = get_op op2 in
    let temp = [X86.Mov(X86.ecx,o1)]@[X86.Mov(X86.ebx,o2)] in
      begin match b with
        | Il.Plus -> temp@[X86.Add(ecx,ebx)]@[X86.Mov(o1,X86.ecx)]
        | Il.Times -> temp@[X86.Imul(Ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
        | Il.Minus -> temp@[X86.Sub(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
        | Il.Shl -> temp@[X86.Shl(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
        | Il.Shr -> temp@[X86.Shr(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
        | Il.Sar -> temp@[X86.Sar(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
        | Il.And -> temp@[X86.And(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
        | Il.Or -> temp@[X86.Or(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
        | Il.Xor -> temp@[X86.Xor(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
        | Il.Compare c ->
          begin match c with
            | Il.Eq -> temp@[X86.Cmp(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
            | Il.Neq -> temp@[X86.Cmp(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
            | Il.Lt -> temp@[X86.Cmp(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
            | Il.Lte -> temp@[X86.Cmp(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
            | Il.Gt -> temp@[X86.Cmp(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
            | Il.Gte -> temp@[X86.Cmp(ecx,ebx)]@[X86.Mov(o2,X86.ecx)]
          end
        | Il.Move -> [X86.Mov(o1,o2)]
      end
  | UnArith (u, op) ->
    let o1 = get_op op in
    let temp = [X86.Mov(o1, X86.ecx)] in
      begin match u with
        | Il.Neg -> temp@[X86.Neg(o1)]@[X86.Mov(o1,X86.ecx)]
        | Il.Not -> temp@[X86.Not(o1)]@[X86.Mov(o1,X86.ecx)]
        | Il.Lognot -> temp@[X86.Not(o1)]@[X86.Mov(o1,X86.ecx)]
      end
  end
  
  
let compile_three (bb: Il.bb) : X86.insn list =
  let rec compile_three_aux (b_list: Il.insn list) (r: X86.insn list) : X86.insn list = 
    begin match b_list with
      | [] -> r
      | h::tl -> compile_three_aux tl (r@(compile_four h))
    end in compile_three_aux bb.bb_body []
 (* let r : X86.insn list = [] in 
    List.map compile_four bb.bb_body*)

let compile_cfin (bb:Il.bb) =
  begin match bb.bb_link with
    | Il.Ret o -> [X86.Mov(eax, (get_op o))]
  end
    (*| Jump lbl (* jump to a0 *)
    | If of operand * compop * operand * lbl * lbl*)


let rec compile_two (bb: Il.bb) : Cunit.component = 
  let epilogue = compile_cfin bb @ [Sub(esp, Imm 16l)] @ [Mov(esp,ebp)] @ [Pop(ebp)]@ [X86.Ret] in 
    let block : Cunit.component =
     Code({X86.global = true; X86.label = bb.bb_lbl;
       X86.insns= compile_three bb  @ epilogue}) in
         block

let compile_one (bb_list: Il.bb list) (uids: uid list): Cunit.component list =
  let program : Cunit.component =
      let block_name = X86.mk_lbl_named "_program" in 
      Code({X86.global = true; X86.label = block_name;
        X86.insns= [Push(ebp)] @ [Mov(ebp,esp)]@[Add(esp, Imm 16l)]}) in 
  let l : Cunit.component list = [] in program :: List.map compile_two bb_list @ l

let compile_prog (prog:Il.prog) : Cunit.cunit =
  let return_unit : Cunit.cunit =
    compile_one prog.il_cfg prog.il_tmps in return_unit