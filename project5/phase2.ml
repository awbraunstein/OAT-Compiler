open Il
open X86
open Cunit

let ccode_for_compare = function
  | Il.Eq  -> X86.Eq
  | Il.Neq -> X86.NotEq
  | Il.Lt  -> X86.Slt
  | Il.Lte -> X86.Sle
  | Il.Gt  -> X86.Sgt
  | Il.Gte -> X86.Sge

let (>::) x y = y::x
let (>@) x y = y @ x

  

let compile_comparison ccode lhs rhs = 
  [Cmp (lhs, rhs); 
   Setb (lhs, ccode);
   And (lhs, Imm 1l);]

let slot_offset (ind:int) : X86.opnd =
  let amt = Int32.of_int (-4 * (ind + 1)) in
  Ind{i_base = Some Ebp;
      i_iscl = None;
      i_disp = Some (DImm amt)} 

let arg_offset (ind:int) : X86.opnd =
  let amt = Int32.of_int (4 * (ind + 2)) in
  Ind{i_base = Some Ebp;
      i_iscl = None;
      i_disp = Some (DImm amt)} 

let compile_op slu o =
  begin match o with
    | Il.Imm i -> X86.Imm i
    | Global {Cunit.label=l} -> X86.deref_lbl l
    | Arg i -> arg_offset i
    | Il.Slot v -> slot_offset (slu v)
  end
let rec push_args slu n str =
  begin match n with
    | h::tl -> [Push (compile_op slu h)]@(push_args slu tl str)
    | [] -> str
  end

let ind_to_ecx o = 
  begin match o with
    | X86.Imm i -> ([], o)
    | Ind _ -> ([Mov(ecx, o)], ecx)
    | _ -> failwith "Internal Error: ind_to_ecx"
  end

let compile_insn slu i : X86.insn list =
  begin match i with
    | Il.BinArith (l,o,r) -> 
	let lo = compile_op slu l in
	let (code, ro) = ind_to_ecx (compile_op slu r) in
	  code @
	  begin match o with
	    | Il.Plus  -> [Add (lo,ro)]
	    | Il.Times -> [Mov (eax,lo); Imul (Eax,ro); Mov (lo,eax)]
	    | Il.Minus -> [Sub (lo,ro)]
	    | Il.Move  -> [Mov (lo,ro)]
	    | Il.And   -> [And (lo,ro)]
	    | Il.Or    -> [Or (lo,ro)]
	    | Il.Xor   -> [Xor (lo,ro)]
	    | Il.Shl -> [Mov (ecx,ro); X86.Shl (lo,ecx)]
	    | Il.Shr -> [Mov (ecx,ro); X86.Shr (lo,ecx)]
	    | Il.Sar -> [Mov (ecx,ro); X86.Sar (lo,ecx)]
	    | Il.Compare f -> compile_comparison (ccode_for_compare f) lo ro 
	  end
    | Il.UnArith (o,l) -> 
	let lo = compile_op slu l in
	  begin match o with
	    | Il.Neg -> [X86.Neg lo]
	    | Il.Not -> [X86.Not lo]
	    | Il.Lognot -> 
		[X86.Cmp (lo, Imm 0l);
         	 X86.Setb (lo, X86.Eq);
		 X86.And  (lo, Imm 1l);]
	  end
    | Il.Alloc (a0, a1) ->
	let ao0 = compile_op slu a0 in
	let ao1 = compile_op slu a1 in
        [Push (ao1); 
         Call (Lbl (X86.mk_lbl_named (Platform.decorate_cdecl "oat_malloc"))); 
         Mov (ao0, eax);
         Add (esp, Imm 4l);]
    | Il.AddrOf (a0, a1, a2) ->
	let ao0 = compile_op slu a0 in
	let ao1 = compile_op slu a1 in
	let (code, ao2) = ind_to_ecx (compile_op slu a2) in
        begin match ao1 with
          | Ind ind -> code @ [Lea (Eax, ind); Add (eax, ao2); Mov (ao0, eax)]
          | _ -> failwith "Internal Error: compile_insn"
        end
    | Il.Load (a0, a1) ->
	let ao0 = compile_op slu a0 in
	let ao1 = compile_op slu a1 in
        let ind = {i_base = Some Edx; i_iscl = None; i_disp = Some (DImm 0l)} in
        [Mov (edx, ao1); Mov (ecx, Ind ind); Mov (ao0, ecx)]
    | Il.Store (a0, a1) ->
	let ao0 = compile_op slu a0 in
	let ao1 = compile_op slu a1 in
        let ind = {i_base = Some Edx; i_iscl = None; i_disp = Some (DImm 0l)} in
        [Mov (edx, ao0); Mov (ecx, ao1); Mov (Ind ind, ecx)]
    | Il.Call (opa0, fid, as1) ->
        let args = List.fold_left 
           (fun code -> fun a -> (Push (compile_op slu a))::code) 
           [] as1 in 
        args @ [Call (Lbl (X86.mk_lbl_named (Platform.decorate_cdecl fid)))]@
        (match opa0 with
          | Some a0 -> let ao0 = compile_op slu a0 in [Mov (ao0, eax)]
          | None -> []) @ [Add (esp, Imm (Int32.of_int (4 * List.length as1)))]
    | Method (opa0, a1, i2, as3) ->
        (* A value of class:
         *         -------+
         *                v 
         * ------------------------------------------
         * | dispatch_lbl |   f0   |   f1   | ...
         * ------------------------------------------
         *                 offset=0 offset=1 ... 
         *
         * This instruction is equivalent to the C-like code:
         *   opa0 = ((a1-4)[i2]) (a1::as3); 
         * 
         * 1) push (a1::as3)
         * 2) let edx = a1-4
         * 3) call i2(edx)
         * 4) store eax to opa0 if needed
         * 5) pop args
        *)
        let l = a1::as3 in
        let ao1 = compile_op slu a1 in
        let l = List.rev l in
        let code = push_args slu l [] in
        let ind2 = {i_base = Some Edx; i_iscl = None; i_disp = Some (DImm 0l)} in
        let code = code@[Mov(edx,ao1)]@[Sub(edx, Imm 4l)]@[Mov (edx,Ind ind2)]  in
        let ind = {i_base = Some Edx; i_iscl = None; i_disp = Some (DImm (Int32.of_int (4*i2)))} in
        let code = code@[Call(Ind ind)] in
        let code = code@(
          begin match opa0 with
            | Some x -> [Mov((compile_op slu x),eax)]
            | None -> []
          end) in
        code@[Add(esp, Imm (Int32.of_int (4*(List.length l))))]
  end

let compile_cfinsn slu epilogue i = 
  begin match i with
    | Il.Ret (Some o) -> (Mov (eax, compile_op slu o)) :: epilogue
    | Il.Ret None -> epilogue
    | Il.Jump l -> [Jmp (Lbl l)]
    | Il.If (l,c,r,tb,fb) -> 
	let ccd = ccode_for_compare c in
	let lo = compile_op slu l in
	let ro = compile_op slu r in
	          [Mov (ecx,lo);
                   Cmp (ecx,ro); 
		   J (ccd, tb);
		   Jmp (Lbl fb);]
  end


let compile_block slu epilogue {bb_lbl=l; bb_body=code; bb_link=i} : Cunit.component =
  let link = compile_cfinsn slu epilogue i in
  let body = (List.fold_right (fun i stream -> (compile_insn slu i)@stream) code link) in
    Code (mk_insn_block l (body))

let compile_fdecl (fdecl:Il.fdecl) : Cunit.cunit =
  let block_name = (Platform.decorate_cdecl (fdecl.fd_name)) in
  let slu x =
    let rec find l i =
      begin match l with
        | [] -> failwith "uid not found in fd_tmps"
        | h::t -> if x = h then i else find t (i+1)
      end
    in find fdecl.fd_tmps 0 in
  let num_slots = List.length fdecl.fd_tmps in
  let alloc_size = Int32.of_int (num_slots * 4) in
  let prologue = [Push ebp; Mov (ebp, esp); Sub (esp, Imm alloc_size); 
                  Push ebx] in
  let epilogue = [Pop ebx; Mov (esp, ebp); Pop ebp; Ret] in
  let blocks = List.map (compile_block slu epilogue) fdecl.fd_cfg in
  let main = Code {X86.global = true;
	           X86.label = mk_lbl_named block_name;
	           X86.insns = prologue @ [Jmp (Lbl fdecl.fd_entry)]} in
    main::blocks

(* Class representation:
 * ---------------------------
 * entry:
 *   dispatch_lbl of super class = super class entry + 4
 * dispatch_lbl:
 *   m1      // offset = 0
 *   m2      // offset = 1
 *   ...     // ...
 * ---------------------------
 *)
let compile_cdecl 
  {ct_entry=this; ct_super=super_opt; 
   ct_dispatch_lbl=tbl_lbl; ct_dispatch_tbl=tbl}
  : Cunit.cunit =
  let decorate_lbl l = X86.mk_lbl_named (Platform.decorate_cdecl 
    (X86.string_of_lbl l)) in
  (begin match super_opt with
    | Some x -> 
      let n = GLabelOffset(x, 4l) in
      [Data({link=false; label=this; value=n;})]
    | None -> [Data({link=false; label=this; value=GInt32(0l);})]
  end
  @[Data({link=false; label=tbl_lbl; value=GLabels(List.map decorate_lbl tbl);})])


let compile_prog (prog:Il.prog) : Cunit.cunit =
  let cu = List.fold_left 
    (fun cu -> fun fdecl -> (compile_fdecl fdecl)@cu) [] prog.il_fdecls in
  let cu = List.fold_left 
    (fun cu -> fun cd -> (compile_cdecl cd)@cu) cu prog.il_cdecls in
  List.fold_left 
    (fun cu -> fun gd -> [Cunit.Data gd]@cu) cu prog.il_globals
