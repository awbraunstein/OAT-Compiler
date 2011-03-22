open Ast
open Il
open Ctxt
open Printf

(* Parse an AST from a preexisting lexbuf. 
 * The filename is used to generate error messages.
*)
let parse (filename : string) (buf : Lexing.lexbuf) : Range.t Ast.prog =
  try
    Lexer.reset_lexbuf filename buf;
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
      | J link :: more -> match_start more
        ((mk_bb lbl (List.rev body) link) :: blks)
      | L nextlbl :: rest  -> match_start code
        ((mk_bb lbl (List.rev body) (Jump nextlbl))::blks)
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
  | Cbool (_, b) -> (c, (if b then Imm 1l else Imm 0l), [])
  | Cint (_, i) -> (c, Imm i, [])
  | Cstring (_, s) -> 
      let (c, ptr) = alloc (mk_tmp ()) c in
      let (c, t) = alloc (mk_tmp ()) c in
      let len = String.length s in 
      let word_len = ((len + 1)/4) + (if ((len + 1) mod 4) <> 0 then 1
        else 0) in
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

let rec call_helper (c:ctxt)(str:stream)(ops:operand list)
  (exps:Range.t exp list) : ctxt*operand list*stream = 
    begin match exps with
      | h::tl -> 
        begin match compile_exp c h with
          | (c1,o1,s1) ->  call_helper c1  (str>@s1) (ops>@[o1]) tl
        end
      | [] -> (c, ops, str)
    end

(* Compile an expression e in context c 
 * The result is list of labeled instructions in reverse execution order. 
*)
and compile_exp (c:ctxt) (e:Range.t exp) : ctxt * operand * stream = 
  match e with
  | Const cn -> compile_const c cn

  | Lhs l -> 
      compile_lhs_exp c l

  | New (e1, (_,s), e2) ->
      let lpre = X86.mk_lbl_hint "pre" in
      let lbody = X86.mk_lbl_hint "body" in
      let lpost = X86.mk_lbl_hint "post" in
      let c = Ctxt.enter_scope c in
      let (c, id) = alloc s c in
      let (c, ptr) = alloc (mk_tmp ()) c in
      let (c, v) = alloc (mk_tmp ()) c in
      let (c, ai) = alloc (mk_tmp()) c in
      let (c, i2) = alloc (mk_tmp()) c in
      let (c, i) = alloc (mk_tmp ()) c in
      let (c, op1, str) = compile_exp c e1 in
      let code = [I(BinArith(Slot v,Move,op1))]>@
      [I(BinArith(Slot v, Plus, Imm 1l))]>@
      [I(BinArith(Slot v, Times, Imm 4l))]>@
      [I(Alloc (Slot ptr, Slot v))]>@
      [I(Store(Slot ptr, op1))]>@
      [I(BinArith(Slot ptr, Plus, Imm 4l))] in
      let (c, e, code2) = compile_exp c e2 in
      let code3 = str >@ code >@
      [I(BinArith(Slot i, Move, Imm 0l))]>@
      [L lpre]>@
      [J(If(Slot i, Lt, op1, lbody, lpost))]>@
      [L lbody] >@code2 >@
      [I(BinArith(Slot ai, Move, Slot ptr))]>@
      [I(BinArith(Slot i2, Move, Slot i))]>@
      [I(BinArith(Slot i2, Times, Imm 4l))]>@
      [I(BinArith(Slot ai, Plus, Slot i2))]>@
      [I(BinArith(Slot i, Plus, Imm 1l))]>@
      [I(Store(Slot ai, e))]>@
      [J(Jump(lpre))]>@
      [L lpost] in (leave_scope c, Slot ptr, code3)

  | Binop (op, e1, e2) -> 
      let (c, ans1, code1) = compile_exp c e1 in
      let tmp = mk_tmp () in
      let (c, v) = alloc tmp c in
      let (c, ans2, code2) = compile_exp c e2 in
      (c, (Slot v), 
       (code1 >:: I (BinArith (Slot v, Move, ans1))) >@
	 (code2 >:: I (BinArith (Slot v, compile_binop op, ans2))))

  | Unop (op, e1) ->
      let (c, ans1, code1) = compile_exp c e1 in
      let tmp = mk_tmp () in
      let (c, v) = alloc tmp c in
	(c, (Slot v), code1 >::
                   I (BinArith (Slot v, Move, ans1)) >::
		   I (UnArith (compile_unop op, Slot v)))

  | Ecall ((_,fid), es) ->
    let er = List.rev es in
    let tmp = mk_tmp () in
    let (c, v) = alloc tmp c in
    let (c2, ops, str1) = call_helper c [] [] er in
    (c2, Slot v, str1 >:: I(Call(Some (Slot v), fid, ops)))

and compile_index (c:ctxt) (l:Range.t lhs)
  (e: Range.t exp) : ctxt * operand * stream =
  let lc = X86.mk_lbl_hint "continue" in
  let lc2 = X86.mk_lbl_hint "continue2" in
  let lb = X86.mk_lbl_hint "break" in
  let (c, ans1, code1) = compile_lhs c l in
  let (c, ans2, code2) = compile_exp c e in
  let (c,v) = alloc (mk_tmp ()) c in
  let (c,v2) = alloc (mk_tmp ()) c in
  (c, Slot v, code1 >@ code2 >::
        I(Load(Slot v2, ans1))
    >:: I(BinArith(Slot v2, Minus, Imm 4l))
    >:: I(Load(Slot v2, Slot v2))
    >:: J(If(ans2, Lt, Slot v2, lc, lb))
    >:: L lc
    >:: J(If(ans2, Lt, Imm 0l, lb, lc2))
    >:: L lb
    >:: I(Call(None, "oat_abort", []))
    >:: L lc2
    >:: I(Load(Slot v, ans1))
    >:: I(BinArith(Slot v2, Move, ans2))
    >:: I(BinArith(Slot v2, Times, Imm 4l))
    >:: I(BinArith(Slot v, Plus, Slot v2)))
  

and compile_lhs (c:ctxt) (l:Range.t lhs) : ctxt * operand * stream = 
  begin match l with
  | Var (_,x) -> 
    begin match lookup x c with
      | Some n -> let tmp = mk_tmp () in
        let (c,v) = alloc tmp c in
        (c,Slot v,[I(AddrOf (Slot v, n, Imm 0l))])
      | None -> failwith "Var not found"
    end

  | Index (l1, e2) -> 
    compile_index c l1 e2
  end

and compile_lhs_exp (c:ctxt) (l:Range.t lhs) : ctxt * operand * stream = 
  begin match l with
  | Var (_,x) -> 
    begin match lookup x c with
      | Some n -> (c,n,[])
      | None -> failwith "value not found"
    end
  | Index (l1, e2) ->
    let (c,ans,code) = compile_index c l1 e2 in
    (c,ans,code >:: I(Load(ans,ans)))
  end

(* Compile a constant cn in context c 
 * An array is compiled to be a pointer p that points to the
 * memory address at which the data of the array are stored. 
 * The length of the array is stored as p-1.
 *)
and compile_init (c:ctxt) (i:Range.t init) : ctxt * operand * stream = 
  match i with
  | Iexp e -> compile_exp c e 
  | Iarray (_, is) ->
      let (c, ptr) = alloc (mk_tmp ()) c in
      let (c, i) = alloc (mk_tmp ()) c in
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
  let compile_vdecl (c, str) {v_id=(_,x); v_init=i} : ctxt * stream =
    let (c, ans, code) = compile_init c i in
    let (c, u) = alloc x c in
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
       begin match compile_lhs c l1 with
          | (c2, op2, str2) -> 
            begin match compile_exp c2 e2 with
              | (c3, op3, str3) -> 
                begin match op2 with
                  | Slot u1 -> 
                    (c3, str2 >@ str3 >:: 
                    I(Il.Store(Slot u1, op3)))
                  | _ -> failwith "lhs is fucked"
                end  
            end
        end
      | Scall ((_,fid), es) -> 
        let er = List.rev es in
        let tmp = mk_tmp () in
        let (c, v) = alloc tmp c in
        let (c2, ops, str1) = call_helper c [] [] er in
        (c2, str1 >:: I(Call(Some (Slot v), fid, ops)))

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
      | Gvdecl {v_id=(_,x); v_init=ci} ->
          let (c, ans, code) = compile_init c ci in
          begin match (lookup_globals x c) with
            | None -> failwith "Internal error: cannot find gvar."
            | Some op -> 
                let (c, u) = alloc (mk_tmp ()) c in
                (c, 
                 str >@ (code >::
                 I (AddrOf (Slot u, op, Imm 0l)) >::
                 I (Store (Slot u, ans))))
          end
      | _ -> (c, str)
  in
    List.fold_left compile_gvdecl (c, []) p

let fdecl_of_code c fid n l code =
  let blocks = blocks_of_ilist (List.rev code)
  in {il_name = fid;
      il_entry = l;
      il_num_of_args = n;
      il_tmps = c.uids;
      il_cfg = blocks;}

let compile_fdecl c p ((_, (_,fid), args, block, reto):Range.t Ast.fdecl) 
  : ctxt * Il.fdecl =
  let l = lookup_fdecl fid c in 
  let c = enter_scope c in
  let c = 
    List.fold_left 
      (fun c -> fun (_, (_,id)) -> let (c, _) = add_args id c in c) c args 
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
  let c = clear_args c in
  (c, fdecl_of_code c fid (List.length args) l code)
 
(* Adding global variables into the context
 * The function does not compile initalizers of these global variables,
 * but only assigns globals to be 0. The compilation of the fdecl "program"
 * initializes globals (See compile_fdecl).
*)
let add_gvdecls c p : ctxt =
  let add_gvdecl c g : ctxt =
    match g with
      | Gvdecl {v_id=(_,s)} ->
          let (c, _) = add_global_int32 (Some s) 0l c in c
      | Gfdecl (_,(_,fid),_,_,_) -> 
          let (c, _) = add_fdecl fid c in c
  in
    List.fold_left add_gvdecl c p

let compile_prog (p:Range.t Ast.prog) : Il.prog =
  let c = add_gvdecls empty_ctxt p in
  let (c, fdecls) = 
    List.fold_left 
      (fun (c, accum) -> fun g ->
         match g with
	   | Gfdecl fdecl ->
               let (c, code) = compile_fdecl c p fdecl in
               (c, code::accum)
	   | _ -> (c, accum)
      ) (c, []) p 
  in
    {il_globals = List.map snd c.globals;
     il_fdecls = fdecls;
     il_start = "program";}

