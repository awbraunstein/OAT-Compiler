open Il
open X86
open Cunit


let stack_offset_ebp (amt:int32) : opnd =
  Ind{i_base = Some Ebp;
      i_iscl = None;
      i_disp = Some (DImm amt)} 
    
let get_op (op: Il.operand) : X86.opnd =
  begin match op with
    | Il.Imm (x) -> X86.Imm x
    | Il.Slot (x,y) -> (stack_offset_ebp (Int32.of_int(-4-(4*(x)))))
  end

let compile_four (i: Il.insn) : X86.insn list =
  begin match i with
  | BinArith (op1, b, op2) ->
    let o1 = get_op op1 in let o2 = get_op op2 in
    let temp = [X86.Mov(X86.ecx,o1)]@[X86.Mov(X86.eax,o2)] in
      begin match b with
        | Il.Plus -> temp@[X86.Add(ecx,eax)]@[X86.Mov(o1,X86.ecx)]
        | Il.Times -> temp@[X86.Imul(Ecx,eax)]@[X86.Mov(o1,X86.ecx)]
        | Il.Minus -> temp@[X86.Sub(ecx,eax)]@[X86.Mov(o1,X86.ecx)]
        | Il.Shl -> temp@[X86.Shl(ecx,eax)]@[X86.Mov(o1,X86.ecx)]
        | Il.Shr -> temp@[X86.Shr(ecx,eax)]@[X86.Mov(o1,X86.ecx)]
        | Il.Sar -> temp@[X86.Sar(ecx,eax)]@[X86.Mov(o1,X86.ecx)]
        | Il.And -> temp@[X86.And(ecx,eax)]@[X86.Mov(o1,X86.ecx)]
        | Il.Or -> temp@[X86.Or(ecx,eax)]@[X86.Mov(o1,X86.ecx)]
        | Il.Xor -> temp@[X86.Xor(ecx,eax)]@[X86.Mov(o1,X86.ecx)]
        | Il.Compare c ->
          begin match c with
            | Il.Eq -> temp@[X86.Cmp(ecx,eax)]@[X86.Setb(ecx,X86.Eq)]@[X86.Mov(o1,X86.ecx)]
            | Il.Neq -> temp@[X86.Cmp(ecx,eax)]@[X86.Setb(ecx,X86.NotEq)]@[X86.Mov(o1,X86.ecx)]
            | Il.Lt -> temp@[X86.Cmp(ecx,eax)]@[X86.Setb(ecx,X86.Slt)]@[X86.Mov(o1,X86.ecx)]
            | Il.Lte -> temp@[X86.Cmp(ecx,eax)]@[X86.Setb(ecx,X86.Sle)]@[X86.Mov(o1,X86.ecx)]
            | Il.Gt -> temp@[X86.Cmp(ecx,eax)]@[X86.Setb(ecx,X86.Sgt)]@[X86.Mov(o1,X86.ecx)]
            | Il.Gte -> temp@[X86.Cmp(ecx,eax)]@[X86.Setb(ecx,X86.Sge)]@[X86.Mov(o1,X86.ecx)]
          end
        | Il.Move -> temp@[X86.Mov(X86.ecx,o2)]@[X86.Mov(o1,X86.ecx)]
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
  let rec compile_three_aux(b_list:Il.insn list)(r:X86.insn list):X86.insn list= 
    begin match b_list with
      | [] -> r
      | h::tl -> compile_three_aux tl (r@(compile_four h))
    end in compile_three_aux bb.bb_body []

let compile_cfin (bb:Il.bb) =
  begin match bb.bb_link with
    | Il.Ret o -> [X86.Mov(eax, (get_op o))]
    | Jump lbl -> [X86.Jmp (Lbl lbl)]
    | If (op1, compop, op2, lbl1, lbl2) ->
      begin match compop with
        | Il.Eq -> (compile_four (BinArith(op1,Compare(compop),op2)))@[X86.J(X86.Eq, lbl2)]
        | Il.Neq ->(compile_four (BinArith(op1,Compare(compop),op2)))@[X86.J(X86.NotEq, lbl2)]
        | Il.Lt -> (compile_four (BinArith(op1,Compare(compop),op2)))@[X86.J(X86.Slt, lbl2)]
        | Il.Lte -> (compile_four (BinArith(op1,Compare(compop),op2)))@[X86.J(X86.Sle, lbl2)]
        | Il.Gt -> (compile_four (BinArith(op1,Compare(compop),op2)))@[X86.J(X86.Sgt, lbl2)]
        | Il.Gte -> (compile_four (BinArith(op1,Compare(compop),op2)))@[X86.J(X86.Sge, lbl2)]
      end
  end


let rec compile_two (bb: Il.bb) : Cunit.component = 
  let epilogue = compile_cfin bb in 
    let block : Cunit.component =
     Code({X86.global = false; X86.label = bb.bb_lbl;
       X86.insns= compile_three bb  @ epilogue}) in
         block

let compile_one (bb_list: Il.bb list): Cunit.component list =
  let epi : Cunit.component =
    let name = X86.mk_lbl_named "_epilogue" in
    Code({X86.global = false; X86.label = name;
       X86.insns= [Pop(ecx)]@[Pop(edx)]@[Add(esp, Imm 100l)]@[Mov(ebp,esp)]
        @ [Pop(ebp)] @ [X86.Ret]}) in
  let program : Cunit.component =
      let block_name = X86.mk_lbl_named "_program" in 
      Code({X86.global = true; X86.label = block_name;
        X86.insns = [Push(ebp)] @ [Mov(ebp,esp)] @ [Sub(esp, Imm 100l)]
          @[Push(edx)]@[Push(ecx)]}) in 
         program :: List.map compile_two bb_list @ [epi]

let compile_prog (prog:Il.prog) : Cunit.cunit =
  print_endline (string_of_prog prog); 
  let return_unit : Cunit.cunit =
    compile_one prog.il_cfg in return_unit