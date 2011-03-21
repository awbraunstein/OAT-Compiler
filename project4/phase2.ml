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
    | Il.AddrOf (a0, a1, a2) -> failwith "Phase2: AddrOf not implemented"
    | Il.Load (a0, a1) -> failwith "Phase2: Load not implemented"
    | Il.Store (a0, a1) -> failwith "Phase2: Store not implemented"
    | Il.Call (opa0, fid, as1) ->
        let code = List.fold_left 
           (fun code -> fun a -> (Push (compile_op slu a))::code) 
           [] as1 in 
        code @ [Call (Lbl (X86.mk_lbl_named (Platform.decorate_cdecl fid)))]@
        (match opa0 with
          | Some a0 -> let ao0 = compile_op slu a0 in [Mov (ao0, eax)]
          | None -> []) @ [Add (esp, Imm (Int32.of_int (4 * List.length as1)))]
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
  let block_name = (Platform.decorate_cdecl (fdecl.il_name)) in
  let slu x =
    let rec find l i =
      begin match l with
        | [] -> failwith "uid not found in il_tmps"
        | h::t -> if x = h then i else find t (i+1)
      end
    in find fdecl.il_tmps 0 in
  let num_slots = List.length fdecl.il_tmps in
  let alloc_size = Int32.of_int (num_slots * 4) in
  let prologue = [Push ebp; Mov (ebp, esp); Sub (esp, Imm alloc_size); 
                  Push ebx] in
  let epilogue = [Pop ebx; Mov (esp, ebp); Pop ebp; Ret] in
  let blocks = List.map (compile_block slu epilogue) fdecl.il_cfg in
  let main = Code {X86.global = true;
	           X86.label = mk_lbl_named block_name;
	           X86.insns = prologue @ [Jmp (Lbl fdecl.il_entry)]} in
    main::blocks

let compile_prog (prog:Il.prog) : Cunit.cunit =
  let cu = List.fold_left 
    (fun cu -> fun fdecl -> (compile_fdecl fdecl)@cu) [] prog.il_fdecls in
  List.fold_left 
    (fun cu -> fun gd -> [Cunit.Data gd]@cu) cu prog.il_globals
