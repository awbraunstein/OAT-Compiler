open Ast
open Il
open Ctxt
open Printf

(* Parse an AST from a preexisting lexbuf. 
 * The filename is used to generate error messages.
*)
let parse (filename : string) (buf : Lexing.lexbuf) : Range.t Ast.prog =
  try
    Lexer.reset_lexbuf filename 1 buf;
    Parser.toplevel Lexer.token buf
  with Parsing.Parse_error ->
    failwith (sprintf "Parse error at %s."
        (Range.string_of_range (Lexer.lex_range buf)))

(* Generate a fresh temporary name *)
let mk_tmp : unit -> string =
  let ctr = ref 0 in
    fun () -> let c = !ctr in ctr := !ctr + 1; "_tmp" ^ (string_of_int c)

(* Some potentially useful helper functions: *)
let compile_binop bop = 
  match bop with
  | Ast.Plus _ -> Il.Plus
  | Ast.Times _ -> Il.Times
  | Ast.Minus _ -> Il.Minus
  | Ast.Eq _ -> Il.Compare Il.Eq
  | Ast.Neq _ -> Il.Compare Il.Neq
  | Ast.Lt _ -> Il.Compare Il.Lt
  | Ast.Lte _ -> Il.Compare Il.Lte
  | Ast.Gt _ -> Il.Compare Il.Gt
  | Ast.Gte _ -> Il.Compare Il.Gte
  | Ast.And _ -> Il.And
  | Ast.Or _ -> Il.Or
  | Ast.IAnd _ -> Il.And
  | Ast.IOr _ -> Il.Or
  | Ast.Shl _ -> Il.Shl
  | Ast.Shr _ -> Il.Shr
  | Ast.Sar _ -> Il.Sar

let compile_unop uop =
  match uop with
  | Ast.Neg    _ -> Il.Neg
  | Ast.Lognot _ -> Il.Lognot
  | Ast.Not    _ -> Il.Not

let (>::) x y = y::x
let (>@) x y = y @ x

(* Stream of instructions, control-flow operations and labels *)
type elt =
  | I of Il.insn
  | J of Il.cfinsn
  | L of Il.lbl

type stream = elt list

let print_instr instr =
  match instr with
    | I i -> printf "I %s\n" (string_of_insn i)
    | J cfi -> printf "J %s\n" (string_of_cfinsn cfi)
    | L l -> printf "L %s\n" (X86.string_of_lbl l)

let print_ilist code =
  List.iter print_instr code

let blocks_of_ilist code = 
  let rec match_start code blks =
    match code with
      | L lbl :: rest -> match_body lbl [] rest blks
      | _ -> failwith "Expected label to start block"
  and match_body lbl body code blks =
    match code with
      | I insn :: rest -> match_body lbl (insn::body) rest blks
      | J link :: [] -> ((mk_bb lbl (List.rev body) link) :: blks)
      | J link :: more -> 
          match_start more ((mk_bb lbl (List.rev body) link) :: blks)
      | L nextlbl :: rest  -> 
          match_start code ((mk_bb lbl (List.rev body) (Jump nextlbl))::blks)
      | _ -> failwith "Found label with no code"
  in
    List.rev(match_start code [])

(* Compile a constant cn in context c 
 * A constant string "abcde" is compiled to be a pointer p that points to the
 * memory address at which "abcde\0" is stored. The length of the string is 
 * stored at p-1. Note that the compiled string has a null terminator.
 *
 *  p -+
 *     v
 *  len|abcd|e\0..|    
 * 
 * The amount of storage space needed in words is:
 *    1 word  + ((len + 1)/4 + (if (len + 1) mod 4 <> 0 then 1 else 0)
*)
let rec compile_const (c:ctxt) (cn:Range.t const) : ctxt * operand * stream = 
  match cn with
    Cnull _ -> (c, Imm 0l, [])
  | Cbool (_, b) -> (c, (if b then Imm 1l else Imm 0l), [])
  | Cint (_, i) -> (c, Imm i, [])
  | Cstring (_, s) -> 
      let (c, ptr) = alloc (mk_tmp ()) None c in
      let (c, t) = alloc (mk_tmp ()) None c in
      let len = String.length s in 
      let word_len = ((len + 1)/4) + (if ((len + 1) mod 4) <> 0 then 1 else 0) in
      let storage_size_in_words = 1 + word_len in
      let get_ith_word_of_string s i =
	let rec build_word w j =
	  let s_index = i*4 + j in
	    if (j = 4) || (s_index >= String.length s) then w
	  else let w' = Int32.logor w
            (Int32.shift_left (Int32.of_int (Char.code (String.get s s_index))) 
              (8 * j))
	  in
	    build_word w' (j+1)
	in
	  build_word 0l 0
      in
      let rec initialize_string accum i : stream =
	if i = word_len then accum else
	  let index = i * 4 in 
	    initialize_string 
	      (accum >::
              I (BinArith (Slot t, Move, Imm (Int32.of_int index))) >::
              I (BinArith (Slot t, Plus, Slot ptr)) >::
              I (Store (Slot t, Imm (get_ith_word_of_string s i))))
	    (i+1)
      in
      (c, Slot ptr,
        [I (Alloc (Slot ptr, Imm (Int32.of_int (4*storage_size_in_words))))] >::
         I (Store (Slot ptr, Imm (Int32.of_int len))) >::
         I (BinArith (Slot ptr, Plus, Imm 4l)) >@ 
         (initialize_string [] 0))

(* Use this when compiling Index to check the array bounds
 * o1 points to the length of the array, o2 is the index
*)
let check_bound (c:ctxt) (o1:operand) (o2:operand) : ctxt * operand * stream =
  let (c, v2) = alloc (mk_tmp ()) None c in
  let (c, len) = alloc (mk_tmp ()) None c in
  let ltrue1 = X86.mk_lbl_hint "then" in
  let lfalse1 = X86.mk_lbl_hint "false" in
  let ltrue2 = X86.mk_lbl_hint "then" in
  let lmerge = X86.mk_lbl_hint "merge" in
  (c,
  o1,
  [I (Load (Slot len, o1))] >::
  J (If (o2, Lt, Imm 0l, ltrue1, lfalse1)) >::

  L (ltrue1) >::
  I (Call (None, "oat_abort", [Imm 0l])) >::
  J (Jump (lmerge)) >::

  L (lfalse1) >::
  J (If (o2, Gte, Slot len, ltrue2, lmerge)) >::

  L (ltrue2) >::
  I (Call (None, "oat_abort", [Imm 0l])) >::
  J (Jump (lmerge)) >::

  L (lmerge) >::
  I (BinArith (Slot v2, Move, o2)) >::
  I (BinArith (Slot v2, Plus, Imm 1l)) >:: 
  I (BinArith (Slot v2, Times, Imm 4l)) >::
  I (BinArith (o1, Plus, Slot v2)))

(* Compiling path and lhs to the address of a value; 
 * compiling call, lhs_or_call and exp to a value. 
 *
 * compile_this_id, compile_path_id, and compile_path's return type is
 *   ctxt * operand * int option * Ast.ptyp * stream
 * where int option is None if the return operand is for non-methods,
 *              and is Some offset if the return operand is for methods;
 *       Ast.ptyp is the type of the return operand.
 *
 * compile_call's return type is
 *   ctxt * operand option * Ast.rtyp * stream
 * where operand option is None if the call has type unit, Some op otherwise.
 *       Ast.rtyp is the type of the return operand.
 * 
 * This pointer is at Arg 0.
*)
let compile_this_id (c:ctxt) (_,id) 
  : ctxt * operand * int option * Ast.ptyp * stream =
  match (lookup_this_cfield id c, lookup_this_cmethod id c) with
    | (Some (n, t), None) -> 
        let (c, v) = alloc (mk_tmp ()) None c in
        (c, Slot v, None, Ptyp t, [I (BinArith (Slot v, Move, Arg 0))] >::
          I (BinArith (Slot v, Plus, Imm (Int32.of_int (4*n)))))
    | (None, Some cm) ->
        (c, Arg 0, Some cm.cm_offset, Pftyp cm.cm_ftyp, [])
    | (None, None) -> failwith ("undeclared class variable: " ^ id)
    | _ -> failwith ("A field and a method has the same name:" ^ id)

let rec compile_path_id (c:ctxt) lc (_,id) 
  : ctxt * operand * int option * Ast.ptyp * stream =
  let (c, ans, t, code) = compile_lhs_or_call c lc in
  match t with
    | TRef (RClass cn) ->
	begin match (lookup_cfield cn id c, 
                     lookup_cmethod cn id c) with
          | (Some (n, t), None) ->
              let (c, v) = alloc (mk_tmp ()) None c in
              (c, Slot v, None, Ptyp t, 
                 code >:: I (BinArith (Slot v, Move, ans)) >::
                   I (BinArith (Slot v, Plus, Imm (Int32.of_int (4*n)))))
	  | (None, Some cm) ->
              let (c, v) = alloc (mk_tmp ()) None c in
              (c, Slot v, Some cm.cm_offset, Pftyp cm.cm_ftyp, 
                 code >:: I (BinArith (Slot v, Move, ans)))
          | (None, None) -> failwith ("undeclared class variable: " ^ id)
          | _ -> failwith ("A field and a method has the same name:" ^ id)
        end
    | _ -> failwith "access non-class type."

and compile_path (c:ctxt) (p:Range.t path) 
  : ctxt * operand * int option * Ast.ptyp * stream =
  match p with
    | ThisId id -> compile_this_id c id
    | PathId (lc, id) -> compile_path_id c lc id

and compile_call (c:ctxt) (cl:Range.t call)
  : ctxt * operand option * Ast.rtyp * stream =
  let compile_args c accum es =
    List.fold_left
      (fun (c, accum, ops) -> fun e ->
         let (c, ans, code) = compile_exp c e in
         (c, accum >@ code, ans::ops)
      ) (c, accum, []) es in
  let gen_code c rt fid accum ops =
    begin match rt with
      | Some _ -> let (c, v) = alloc (mk_tmp ()) None c in
          (c, Some (Slot v), rt, 
             accum >:: I (Call (Some (Slot v), fid, ops)))
      | None -> (c, None, rt, accum >:: I (Call (None, fid, ops)))
    end in
  match cl with
    | Func ((_,fid), es) ->
        let (_, (_,rt)) = lookup_fdecl fid c in
        let (c, accum, ops) = compile_args c [] es in
        gen_code c rt fid accum (List.rev ops)
    | SuperMethod ((_,id), es) ->
        (* 1) find the label of the super method
         * 2) call the label with arguments (Arg 0::es)
         *    since we keep the invariant that Arg 0 contains this.
	*)
	begin match lookup_super_class c with
	  | None -> failwith "no super class"
	  | Some pid ->
              let (c, accum, ops) = compile_args c [] es in
              match lookup_cmethod pid id c with
   	        | Some {cm_entry=fid;cm_ftyp=(_,rt)} -> 
                    gen_code c rt (X86.string_of_lbl fid) accum 
                      (Arg 0::List.rev ops)
	        | None -> failwith (sprintf "Super class %s has no %s" pid id)
        end
    | PathMethod (p, es) ->
        (* 1) find the offset of the virtual invocation
         * 2) compile the call to Il.Method 
	*)
        let (c, ans, iop, pt, code) = compile_path c p in
        let (c, accum, ops) = compile_args c code es in
	match (iop, pt) with
	  | (Some i, Pftyp (_, (Some t))) ->
              let (c, v) = alloc (mk_tmp ()) None c in
              (c, Some (Slot v), (Some t), 
                 accum >:: I (Method (Some (Slot v), ans, i, List.rev ops)))
	  | (Some i, Pftyp (_, None)) ->
              (c, None, None, accum >:: I (Method (None, ans, i, List.rev ops)))
	  | _ -> failwith "Call: path returns wrong results."

and compile_lhs_or_call (c:ctxt) (lc:Range.t lhs_or_call)
  : ctxt * operand * Ast.typ * stream =
  match lc with
    | Lhs l -> let (c, ans, t, code) = compile_lhs c l in 
        (c, ans, t, code >:: I (Load (ans, ans)))
    | Ast.Call cl -> 
        let (c, ans_opt, rt, code) = compile_call c cl in
  	match (ans_opt, rt) with
          | (Some ans, Some t) -> (c, ans, t, code)
	  | _ -> failwith "LhsOrCall: call returns wrong results."

and compile_exp (c:ctxt) (e:Range.t exp) : ctxt * operand * stream = 
  match e with
  | Const cn -> compile_const c cn

  | This _ -> (c, Arg 0, [])

  | New (e1, id, e2) ->
      let (c, ans1, code1) = compile_exp c e1 in
      let c = enter_scope c in 
      let bound = mk_tmp () in
      let (c, ub) = alloc bound (Some TInt) c in
      let ptr = mk_tmp () in
      let (c, up) = alloc ptr (Some (TRef (RArray TInt))) c in
      let (c, size) = alloc (mk_tmp ()) None c in 
      let code2 =
        [] >::
        I (BinArith (Slot ub, Move, ans1)) >::
        I (BinArith (Slot size, Move, ans1)) >::
        I (BinArith (Slot size, Plus, Imm 1l)) >::
        I (BinArith (Slot size, Times, Imm 4l)) >::
        I (Alloc (Slot up, Slot size)) >::
        I (Store (Slot up, ans1)) >::
        I (BinArith (Slot up, Plus, Imm 4l))
      in
      let (c, code_s) = compile_stmt c
         (For([{v_ty=TInt;v_id=id;v_init=Iexp (Const (Cint (Range.norange,0l)))}], 
              Some (Binop (Ast.Lt Range.norange, 
                           LhsOrCall (Lhs (Var id)), 
                           LhsOrCall (Lhs (Var (Range.norange, bound))))), 
              Some (Assign (Var id, 
                           (Binop (Ast.Plus Range.norange, 
                                  LhsOrCall (Lhs (Var id)), 
                                  Const (Cint (Range.norange, 1l)))))), 
              Assign (Index (Lhs (Var (Range.norange, ptr)), 
                             LhsOrCall (Lhs (Var id))), e2)))
      in
        (leave_scope c, Slot up, code1 >@ (code2 >@ code_s))

  | Ctor ((i, cid), es) ->
      (* 1) allocate a this pointer
       * 2) compile Call cid(this::es) 
      *)
      let this = mk_tmp () in
      let (c, u) = alloc this (Some (TRef (RClass cid))) c in
      let (c, ans, code) = 
        compile_exp c (LhsOrCall (Ast.Call (Func ((i, mk_ctor_name cid), 
          (LhsOrCall (Lhs (Var (i, this))))::es)))) in
      let size = 4 * List.length (lookup_cdecl cid c).cd_fields + 4 in
        (c, ans, [I (Alloc (Slot u, Imm (Int32.of_int size))) ] >::
          I (BinArith (Slot u, Plus, Imm 4l)) >@ code)

  | LhsOrCall lc -> 
      let (c, ans, _, code) = compile_lhs_or_call c lc in 
      (c, ans, code)

  | Binop (op, e1, e2) -> 
      let (c, ans1, code1) = compile_exp c e1 in
      let (c, v) = alloc (mk_tmp ()) None c in
      let (c, ans2, code2) = compile_exp c e2 in
        (c, (Slot v), (code1 >:: I (BinArith (Slot v, Move, ans1))) >@
          (code2 >:: I (BinArith (Slot v, compile_binop op, ans2))))

  | Unop (op, e1) ->
      let (c, ans1, code1) = compile_exp c e1 in
      let (c, v) = alloc (mk_tmp ()) None c in
	(c, (Slot v), code1 >:: I (BinArith (Slot v, Move, ans1)) >::
	  I (UnArith (compile_unop op, Slot v)))

and compile_index (c:ctxt) (lc1:Range.t lhs_or_call) (e2:Range.t exp) 
  : ctxt * operand * Ast.typ * stream =
  let (c, ans1, t1, code1) = compile_lhs_or_call c lc1 in
  let t3 = begin match t1 with
    | TRef (RArray t) -> t
    | _ -> failwith "index non-array type."
    end in
  let (c, ans2, code2) = compile_exp c e2 in
  let (c, v) = alloc (mk_tmp ()) None c in
  let (c, ans3, code3) = check_bound c (Slot v) ans2 in
    (c, ans3, t3,
      (code1 >:: I (BinArith (Slot v, Move, ans1)) >@ 
      (code2 >:: I (BinArith (Slot v, Minus, Imm 4l))) >@ code3))

and compile_lhs (c:ctxt) (l:Range.t lhs) : ctxt * operand * Ast.typ * stream = 
  match l with
  | Var (_,x) ->
      begin match lookup x c with
        | None -> failwith ("undeclared variable: " ^ x)
        | Some (op, Some t) -> 
          let (c, v) = alloc (mk_tmp ()) None c in
          (c, Slot v, t, [I (AddrOf (Slot v, op, Imm 0l))])
	| Some _ -> failwith ("access internal temporals: " ^ x)
      end

  | Path p -> 
      let (c, ans, iopt, pt, code) = compile_path c p in
	begin match (iopt, pt) with
          | (None, Ptyp t) -> (c, ans, t, code)
	  | _ -> failwith "Lhs: path returns no results."
	end

  | Index (lc1, e2) -> compile_index c lc1 e2

(* Compile a constant cn in context c 
 * An array is compiled to be a pointer p that points to the
 * memory address at which the data of the array are stored. 
 * The length of the array is stored as p-1.
*)
and compile_init (c:ctxt) (i:Range.t init) : ctxt * operand * stream = 
  match i with
  | Iexp e -> compile_exp c e 
  | Iarray (_, is) ->
      let (c, ptr) = alloc (mk_tmp ()) None c in
      let (c, i) = alloc (mk_tmp ()) None c in
      let size = List.length is in 
      let (c, accum, _) =
         List.fold_left 
          (fun (c, accum, n) -> fun it ->
             let (c, ans, code) = compile_init c it in 
             (c, 
              accum >@ 
              (code >:: 
               I (BinArith (Slot i, Move, Imm (Int32.of_int (4*n)))) >::
               I (BinArith (Slot i, Plus, Slot ptr)) >::
               I (Store (Slot i, ans))),
              n+1)
          ) (c, [], 0) is in       
      (c, Slot ptr,
       [I (Alloc (Slot ptr, Imm (Int32.of_int (4*(1+size)))))] >::
       I (Store (Slot ptr, Imm (Int32.of_int size))) >::
       I (BinArith (Slot ptr, Plus, Imm 4l)) >@ accum)

and compile_vdecls c vdls : ctxt * stream =
  let compile_vdecl (c, str) {v_ty=t; v_id=(_,x); v_init=i} : ctxt * stream =
    let (c, ans, code) = compile_init c i in
    let (c, u) = alloc x (Some t) c in
      (c, str >@ code >:: (I (BinArith (Slot u, Move, ans))))
  in
    List.fold_left compile_vdecl (c, []) vdls

and compile_block c (vdls, stmts) top: ctxt * stream =
  let c = enter_scope c in
  let (c, decl_code) = compile_vdecls c vdls in
  let (c, stmt_code) = compile_stmts c stmts in
  let c = if not top then leave_scope c else c in
    (c, (decl_code >@ stmt_code))

and compile_stmt c stmt : ctxt * stream =
    begin match stmt with
      | Assign (l1, e2) ->
          let (c, ans1, _, code1) = compile_lhs c l1 in
          let (c, ans2, code2) = compile_exp c e2 in
	  (c, (code1 >@ (code2 >:: I (Store (ans1, ans2)))))

      | Scall cl ->
        let (c, ans_opt, rt, code) = compile_call c cl in
  	begin match (ans_opt, rt) with
          | (None, None) -> (c, code)
	  | _ ->
              Astlib.print_call cl; 
              failwith "Stmt: call returns wrong results."
	end

      | Fail (e) -> 
          (* It is equivalent to
           *   print_string (e);
           *   oat_abort (-1); 
	  *)
	  let (c, ans, code_e) = compile_exp c e in
          let (c, code1) = compile_stmt c 
            (Scall (Func ((Astlib.exp_info e, "print_string"), [e]))) in 
          let (c, code2) = compile_stmt c 
            (Scall (Func ((Astlib.exp_info e, "oat_abort"), 
                    [Const (Cint (Astlib.exp_info e, -1l))]))) in 
          (c, code_e >@ (code1 >@ code2))

      | Ast.If(e, stmt1, ostmt2) ->
	  let ltrue = X86.mk_lbl_hint "then" in
	  let lmerge = X86.mk_lbl_hint "merge" in
	  let (c, ans, code_e) = compile_exp c e in
	  let (c, code_true) = 
	    let (c, code) = compile_stmt c stmt1 in 
	      (c, [L ltrue] >@ code >:: (J (Jump lmerge)) ) in
	  let (c, code_false, lfalse) = 
	    begin match ostmt2 with
	      | None -> (c, [], lmerge)
	      | Some stmt2 -> 
		  let lfalse = X86.mk_lbl_hint "false" in
		  let (c, code) = compile_stmt c stmt2 in 
		    (c, [L lfalse] >@ code >:: (J (Jump lmerge)),
		     lfalse)
	    end in
	    (c, (code_e >::
	        (J (If (ans, Neq, Imm 0l, ltrue, lfalse)))) >@
	        code_true >@
		code_false >::
		(L lmerge))

      | IfNull(r, (info, id), e, stmt1, ostmt2) ->
          (* 1) evaluate [e], and store the value to [id]
           * 2) compile If ([id] stmt1 ostmts)
           * 
           * Note it does not matter if we also allocate [id] for [ostmt2],
           * since TC ensures that [ostmts] cannot use it.
          *)
	  let (c, ans, code_e) = compile_exp c e in
          let c = enter_scope c in
          let (c, u) = alloc id (Some (TRef r)) c in
          let (c, code) = 
            compile_stmt c (Ast.If(LhsOrCall (Lhs (Var (info, id))), 
              stmt1, ostmt2)) in
          let c = leave_scope c in
            (c, code_e >:: I (BinArith (Slot u, Move, ans)) >@ code)

      | Cast(cid, (_, id), e, stmt1, ostmt2) ->
          (* 1) evaluate [e], and store the value to [id]
           * 2) Check if the value of [e] is of a subclass of [cid], and
           *    if so, jump to [stmt1] otherwise fall through to [ostmt2]
           *    if there is one.
           * 
           * Note it does not matter if we also allocate [id] for [ostmt2],
           * since type checking ensures that [ostmt2] cannot use it.
          *)
          let c = Ctxt.enter_scope c in
          let (c, op, str) = compile_exp c e in
          let (c,u) =  Ctxt.alloc id (Some(TRef(RClass(cid)))) c in
          let str = str >::I(Il.BinArith ((Slot u), Il.Move,  op)) in
          let (c,temp_2) = alloc (mk_tmp()) None c in
          let (c,temp_1) = alloc (mk_tmp()) None c in
          let (c,an) = alloc (mk_tmp()) None c in
          let (c, cstmt1) = compile_stmt c stmt1 in
          let d = Ctxt.lookup_cdecl cid c in
          let disp = d.cd_dispatch_lbl in
          let (c, cstmt2) = (
            begin match ostmt2 with
              | Some x -> compile_stmt c x 
              | None -> (c,[])
            end) in
           
          let str = str >@
          [I(Il.BinArith((Slot temp_2,Move, Arg 0)))]>@
          [I(Il.BinArith(Slot temp_2,Il.Minus, Imm 4l))]>@
          [I(Il.AddrOf(Slot temp_1, Global {Cunit.link=false;
            Cunit.label=disp; Cunit.value=Cunit.GZero(0);}, Imm 0l))]>@
          [I(Il.Store(Slot temp_2, Slot temp_1))] in
      	  let lpre = X86.mk_lbl_hint "pre" in
	        let lbody = X86.mk_lbl_hint "body" in
	        let lpost = X86.mk_lbl_hint "post" in 
          let lzero = X86.mk_lbl_hint "zero" in 
          let lcont = X86.mk_lbl_hint "cont" in 
          let ldone = X86.mk_lbl_hint "done" in 
	        let n = Ctxt.lookup_cdecl cid c in
          let disp = n.cd_dispatch_lbl in
          (c, str >@ 
            [I(Il.AddrOf(Slot an, Global {Cunit.link=false;
              Cunit.label=disp; Cunit.value=Cunit.GZero(0);}, Imm 0l))]>@
            [L lpre] >@
            [J (If (Slot an, Neq, Slot temp_2, lbody, lpost))] >@
            [L lbody] >@
            [I(Il.BinArith(Slot temp_2,Il.Minus, Imm 4l))]>@
            [I(Il.Load(Slot temp_1, Slot temp_2))] >@
            [J(Il.If(Slot temp_1, Eq, Imm 0l, lzero, lcont))]>@
            [L lcont] 
            >:: (J (Jump lpre)) >::(L lpost) >@ cstmt1 >@[J(Jump ldone)] >::
            (L lzero)>@ cstmt2 >@[L ldone])
          
       
      | While(e, s) ->
	  let lpre = X86.mk_lbl_hint "pre" in
	  let lbody = X86.mk_lbl_hint "body" in
	  let lpost = X86.mk_lbl_hint "post" in
	  let (c, ans, code_e) = compile_exp c e in
	  let (c, code_s) = compile_stmt c s in
	    (c, 
	     [L lpre] >@ code_e >@
	     [J (If (ans, Neq, Imm 0l, lbody, lpost))] >@
	     [L lbody] >@ code_s >:: (J (Jump lpre)) >:: 
	     (L lpost))

      | For(vdls, eopt, postopt, body) ->
	  let e = 
            match eopt with 
                None -> Const (Cint (Range.norange, 1l)) 
              | Some e -> e 
          in
	  let s = body :: (match postopt with None -> [] | Some s -> [s]) in
	    compile_block c (vdls, [While(e, Block ([],s))]) false

      | Block b -> compile_block c b false
    end

and compile_stmts c (stmts:(Range.t stmt) list) : ctxt * stream =
    List.fold_right (fun s (c, code) -> let (c', code') = compile_stmt c s in
		      (c', code @ code')) stmts (c,[]) 

(* Compile a function declaration in context c 
 * The function "program" initializes global variables by compile_gvdecls. 
*)
let compile_gvdecls c p : ctxt * stream =
  let compile_gvdecl (c, str) g : ctxt * stream =
    match g with
      | Gvdecl {v_ty=t; v_id=(_,x); v_init=ci} ->
          let (c, ans, code) = compile_init c ci in
          begin match (lookup_global x c) with
            | None -> failwith "Internal error: cannot find gvar."
            | Some (op, _) -> 
                let (c, u) = alloc (mk_tmp ()) (Some t) c in
                (c, 
                 str >@ (code >::
                 I (AddrOf (Slot u, op, Imm 0l)) >::
                 I (Store (Slot u, ans))))
          end
      | _ -> (c, str)
  in
    List.fold_left compile_gvdecl (c, []) p

(* Generating code for global functions if [cidopt] is None, 
 * ............... for methods of class [cid] if [cidopt] is Some [cid].
*)
let fdecl_of_code c cidopt fid n l code =
  let blocks = blocks_of_ilist (List.rev code)
  in {fd_name = 
        (match cidopt with 
          | Some _ ->
              (match lookup_this_cmethod fid c with
		            | Some {cm_entry=n} -> X86.string_of_lbl n
	              | None -> failwith (sprintf "Cmethod %s is not defined." fid))
          | _ -> fid);
      fd_entry = l;
      fd_num_of_args = n;
      fd_tmps = c.uids;
      fd_cfg = blocks;}

(* The Oat calling convention for methods requires that the this pointer be 
 * passed on the stack as though it were the first parameter to a cdecl 
 * function. 
 *
 * If this is a method---[cidopt] is not None, add an additional arg "_this"
 * as its first arg, which is the this pointer.  
*)
let compile_fdecl c p cidopt ((_, (_,fid), args, block, reto):Range.t Ast.fdecl)  
  : Il.fdecl =
  let l = (match cidopt with
      | None -> let (l,_) = lookup_fdecl fid c in l 
      | Some _ -> (match lookup_this_cmethod fid c with
          | Some {cm_first_lbl=l} -> l
          | None -> failwith (sprintf "Cmethod %s is not defined." fid)))
  in 
  let c = enter_scope c in
  let c = 
    List.fold_left 
      (fun c -> fun (t, (_,id)) -> add_args id t c)
      (match cidopt with
         | None -> c
         | Some cid -> add_args "_this" (TRef (RClass cid)) c)
      args 
  in
  let (c, gcode) = 
    if (fid = "program") then compile_gvdecls c p else (c, []) 
  in
  let (c, block_stream) = compile_block c block true in
  let prefix = [L (l)] >@ (gcode >@ block_stream) in
  let (c, code) =
    match reto with
      | Some ret -> 
        let (c, ans, exp_stream) = compile_exp c ret in
        (c, prefix >@ (exp_stream >:: J (Ret (Some ans)))) 
      | None -> (c, prefix >:: J (Ret None))  
  in
  let c = leave_scope c in
  let c = leave_scope c in
  let (n, c) = clear_args c in
  fdecl_of_code c cidopt fid n l code

(* create Il.cdecl for the class [cid]. *) 
let gen_cdecl c cid =
  try 
    let cd = List.assoc cid c.cdecls in
    let lsuper_opt =
      begin match cd.cd_super with
        | None -> None
        | Some super -> 
          try 
            Some (List.assoc super c.cdecls).cd_entry
          with
	    | Not_found ->
              failwith (Printf.sprintf "gen_cdecl: Super %s is not found" super)
      end
    in
    let ls = List.map (fun (_,{cm_entry=n}) -> n) cd.cd_methods in 
       {ct_entry=cd.cd_entry;
        ct_super=lsuper_opt;
        ct_dispatch_lbl=cd.cd_dispatch_lbl;
        ct_dispatch_tbl=List.rev ls}    
  with
    | Not_found -> 
      failwith (Printf.sprintf "gen_cdecl: Class %s is not found" cid)

let compile_cinits c cis : ctxt * stream =
  let compile_cinit (c, str) (id, i) : ctxt * stream =
    let (c, ans1, _, code1) = compile_lhs c (Path (ThisId id)) in
    let (c, ans2, code2) = compile_init c i in
      (c, (str >@ code1 >@ (code2 >:: I (Store (ans1, ans2)))))
  in
    List.fold_left compile_cinit (c, []) cis


let rec check_fields f =
    begin match f with
      | (t,_)::tl ->
        begin match t with
          | TNullable r ->
            begin match r with
              | RArray t ->
                begin match t with
                  | TBot -> I(Call(None, "oat_abort", []))
                  | _ -> I(Call(None, "oat_abort", []))
                end
              | RClass c -> if (c = "") then I(Call(None, "oat_abort", []))
                else I(Call(None, "oat_abort", []))
            end
        end
    end
    
    
(* 1) add an additional arg named "_this" as the first args, which is the
 *    this pointer, and with type class [cid].
 * 2) add [args] into context
 * 3) evaluate [es]
 * 4) call super class's constructor with ("_this"::es)
 * 5) add "this._name = [cid];" in [cis] to update class name
 * 6) run [cis] and initialize fields
 * 7) check if all non-null fields are not null, raise a runtime exception if 
 *    null by calling "oat_abort(1)"
 * 8) update vtble to be this class's vtble
 * 9) evalute the block [b]
 * 10) return "_this" 
*)

  
let rec ctor_exp c exps ops str : ctxt * operand list * stream = 
  begin match exps with
    | h::tl -> 
      let (c, op, st) = compile_exp c h in
      ctor_exp c tl (ops@[op]) (str>@st)
    | [] -> (c, ops,str)
  end


let compile_ctor c cid cidopt ((args, es, cis, b):Range.t Ast.ctor) 
  : Il.fdecl =
  let (l, _) = lookup_fdecl (mk_ctor_name cid) c in 
  let c = enter_scope c in
  let c = 
    List.fold_left 
      (fun c -> fun (t, (_,id)) -> add_args id t c)
        (add_args "_this" (TRef (RClass cid)) c)
      args 
  in let (c,call) =
  match cidopt with
    | None -> (c,[])
    | Some cid_o -> compile_stmt c (Assign(Var(Range.norange, "_this"),
      LhsOrCall(Ast.Call(Func((Range.norange, mk_ctor_name cid_o),
        ([(LhsOrCall (Lhs (Var (Range.norange, "_this"))))]@es)))))) in
   let cis = [((Range.norange, "_name"),
      Iexp (Const (Cstring(Range.norange, cid))))]@cis in
  let (c, cis_stream) =
    compile_cinits c cis in
  let (c, block_stream) = compile_block c b true in
  let (c,temp_1) = alloc (mk_tmp()) None c in
  let (c,temp_2) = alloc (mk_tmp()) None c in
  let d = Ctxt.lookup_cdecl cid c in
  let fieid_stream = [] in
  let disp = d.cd_dispatch_lbl in
  let disp_stream =
        [I(Il.BinArith((Slot temp_2,Move, Arg 0)))]>@
        [I(Il.BinArith(Slot temp_2,Il.Minus, Imm 4l))]>@
        [I(Il.AddrOf(Slot temp_1, Global {Cunit.link=false;
        Cunit.label=disp; Cunit.value=Cunit.GZero(0);}, Imm 0l))]>@
        [I(Il.Store(Slot temp_2, Slot temp_1))] in
  let code = [L (l)] >@ (call >@ cis_stream >@
    fieid_stream >@disp_stream >@ block_stream) >:: J (Ret (Some (Arg 0))) in
  let c = leave_scope c in
  let (n, c) = clear_args c in
  fdecl_of_code c None (mk_ctor_name cid) n l code



let compile_cdecl c ((cid, cidopt, fds, ctor, fdls):Range.t Ast.cdecl)
  : ctxt * Il.cdecl * Il.fdecl list =
  let c = set_this c cid in
  let fd = compile_ctor c cid cidopt ctor in  
  let fdecls = 
    List.fold_left 
      (fun accum -> fun fdl -> (compile_fdecl c [] (Some cid) fdl)::accum
      ) [fd] fdls in
  let c = unset_this c in
  (c, gen_cdecl c cid, fdecls)

(* When adding global variables into the context,
 * the function does not compile initalizers of these global variables,
 * but only assigns globals to be 0. The compilation of the fdecl "program"
 * initializes globals (See compile_fdecl).
 *
 * Adding constructors into context like a global function, using
 * mk_ctor_name to generate its name.
*)
let add_gdecls c p : ctxt =
  let add_gdecl c g : ctxt =
    match g with
      | Gvdecl {v_ty=t;v_id=(_,s)} -> add_global_int32 (Some s) 0l t c
      | Gefdecl (rt,(_,fid),args) -> 
          let (ts, _) = List.split args in 
          add_fdecl fid (ts, rt) c
      | Gfdecl (rt,(_,fid),args,_,_) -> 
          let (ts, _) = List.split args in 
          add_fdecl fid (ts, rt) c
      | Gcdecl (cid, cidopt, fds, (args, _, _, _), fdls) ->
          let c = add_cdecl c cid cidopt in
          let c = List.fold_left 
            (fun c -> fun (t, (_,id)) -> add_cfield cid id t c) c fds in
          let c = List.fold_left 
            (fun c -> fun (rt, (_, id), args, _, _) ->
              let (ts, _) = List.split args in 
              add_cmethod cid id (ts, rt) c) c fdls in
          let c = 
            let (ts, _) = List.split args in 
            add_fdecl (mk_ctor_name cid) (ts, Some (TRef (RClass cid))) c 
          in unset_this c
  in
    List.fold_left add_gdecl c p

(* 1) add Object to be the first element in [p].
 *    class Object { 
 *      string _name;
 *      new()() 
 *        this._name = "Object"; //compile_cdecl adds this cinit for all classes
 *      {}
 *      string get_name () {
 *        return this._name;
 *      }
 *    };   
 * 2) add global variables, function and class declarations into 
 *    the context by [add_gdecls].
 * 3) compile [p] 
*)
let compile_prog (p:Range.t Ast.prog) : Il.prog =
  let p = 
    (Gcdecl 
      ("Object", None, 
        [(TRef RString, (Range.norange, "_name"))], 
        ([], [], [], ([], [])), 
        [(Some (TRef RString), 
          (Range.norange, "get_name"),[],([], []), 
          Some (LhsOrCall (Lhs (Path (ThisId (Range.norange, "_name"))))))])
    )::p in
  let c = add_gdecls empty_ctxt p in
  let (c, cds, efds, fds) = 
    List.fold_left 
      (fun (c, cds, efds, fds) -> fun g ->
         match g with
           | Gefdecl (rt,(_,fid),args) -> 
               (c, cds, 
                {efd_name=fid;efd_num_of_args=List.length args}::efds, fds)
	   | Gfdecl fdecl -> 
               (c, cds, efds, (compile_fdecl c p None fdecl)::fds)
	   | Gcdecl cdecl ->
               let (c, cd, fds') = compile_cdecl c cdecl in 
               (c, cd::cds, efds, fds@fds')
 	   | _ -> (c, cds, efds, fds)
      ) (c, [], [], []) p 
  in
    {il_globals = List.map (fun x -> fst (snd x)) c.globals;
     il_efdecls = efds;
     il_fdecls = fds;
     il_cdecls = cds;
     il_start = "program";}

