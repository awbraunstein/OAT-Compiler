open Ast
open Astlib
open Printf
open Tctxt

let report_typ_error (info:Range.t) (expected:typ) (t:typ) : string =
  sprintf 
    "%s: This expression has type %s but an expression was expected of type %s." 
    (Range.string_of_range info) (string_of_typ t) (string_of_typ expected) 

let report_ftyp_error (info:Range.t) (expected:ftyp) (t:ftyp) : string =
  sprintf 
    "%s: This expression has type %s but an expression was expected of type %s." 
    (Range.string_of_range info) (string_of_ftyp t) (string_of_ftyp expected)

let report_wftyp_error (info:Range.t) (t:typ) : string =
  sprintf "%s: This expression has ill-formed type %s." 
    (Range.string_of_range info) (string_of_typ t)

let rec wellformed_typ (sigs:signature) (t:typ) : bool =
  match t with
    | TRef r -> wellformed_ref sigs r
    | TNullable r -> wellformed_ref sigs r 
    | _ -> true

and wellformed_ref (sigs:signature) (r:ref) : bool =
  match r with
    | RClass cid -> 
        (try (ignore (List.assoc cid sigs); true) with | Not_found -> false)
    | RArray t -> wellformed_typ sigs t
    | _ -> true

(* 
 * A signature is a list of class 'interfaces' (see the ast.mli).
 * 
 * Given such a signature and a class id cid, find_parent returns
 * the Some of parent class or None if there isn't one.  It also returns 
 * remainder of the signature following the parent class (which contains
 * the classes that the parent might itself inherit from).
 *
 * Assumption: if sigs = sigs1 @ (sig1::sigs2) @ (sig2 :: sigs3)
 * then sig2 is defined before sig1 in the source file and hence sig1 may
 * be a child class of sig2.
 *)
let rec find_parent (sigs:signature) (cid:Ast.cid) : cid option * signature = 
  begin match sigs with
    | (s,(Some st, fc, tl, mc))::tail ->
      if cid = s then (Some st, tail) else
        find_parent tail cid
    | (s,(None, fc, tl, mc))::tail ->
       if cid = s then (None, tail) else find_parent tail cid
    | [] -> (None, [])
  end

(*
 * Returns whether cid1 is a subclass of cid2 (transitively) according to
 * the given signatures.
 *)
let rec subclass (sigs:signature) (cid1:Ast.cid) (cid2:Ast.cid) : bool =
  if (cid1 = cid2) then true else
  let p = find_parent sigs cid1 in
  begin match p with
    | (Some a,_) -> if (a = cid2) then true else subclass sigs a cid2
    | (None, _ ) -> false
  end

(*
 * Determines whether r1 is a subtype (as a reference) of r2.
 *)
let subref (sigs:signature) (r1:Ast.ref) (r2:Ast.ref) : bool =
  if r1 = r2 then true else 
    begin match r1 with
      | RClass c ->
        begin match r2 with 
          | RClass c2 -> subclass sigs c c2
          | _ -> false
        end
      | RArray t ->
        begin match r2 with
          | RArray t2 -> (t = t2)
          | _ -> false
        end
      | _ -> false
    end

(*
 * Determines whether type t1 is a subtype of type t2.
 *)
let subtyping (sigs:signature) (t1:Ast.typ) (t2:Ast.typ) : bool =
  if (t1 = t2) then true else
  begin match t1 with
    | TRef r ->
      begin match t2 with
        | TRef r2 -> subref sigs r r2
        | TNullable r2 -> subref sigs r r2
        | _ -> false
      end
    | TNullable r ->
      begin match t2 with
        | TNullable r2 -> subref sigs r r2
        | _ -> false
      end
    | TBot ->
      begin match t2 with
        | TNullable _ -> true
        | _ -> false
      end
    | _ -> false
  end

let rec cid_join sigs cid1 cid2 : cid option =
  match find_parent sigs cid1 with
    | (None, _) -> None
    | (Some pid, sigs2) ->
        if subclass sigs cid2 pid then Some pid
        else if subclass sigs pid cid2 then Some cid2
        else cid_join sigs pid cid2

let join sigs (t1:Ast.typ) (t2:Ast.typ) : typ option = 
  if subtyping sigs t1 t2 then Some t2
  else if subtyping sigs t2 t1 then Some t1
  else match (t1, t2) with
    | (TRef (RClass cid1), TRef (RClass cid2)) ->
        begin match cid_join sigs cid1 cid2 with
          | None -> None
	  | Some cid -> Some (TRef (RClass cid))
        end
    | (TNullable (RClass cid1), TNullable (RClass cid2))
    | (TNullable (RClass cid1), TRef (RClass cid2))
    | (TRef (RClass cid1), TNullable (RClass cid2)) ->
        begin match cid_join sigs cid1 cid2 with
          | None -> None
	  | Some cid -> Some (TNullable (RClass cid))
        end
    | (TBot, TRef r) -> Some (TNullable r) 
    | (TRef r, TBot) -> Some (TNullable r) 
    | _ -> None

let typecheck_unop (uop:Range.t unop) (t:typ) : typ =
  let error_msg = sprintf "%s: %s cannot take input type: %s." 
    (Range.string_of_range (unop_info uop)) (string_of_unop uop)
    (string_of_typ t) in
  match uop with
      Neg _ | Not _ -> 
        if (t = TInt || t = TBool) then TInt else failwith error_msg
    | Lognot _ -> 
        if (t = TInt || t = TBool) then TBool else failwith error_msg

let typecheck_bop c (bop:Range.t binop) (t1:typ) (t2:typ) : typ =
  let error_msg = sprintf "%s: %s cannot take input type: (%s, %s)."
    (Range.string_of_range (binop_info bop)) (string_of_binop bop) 
    (string_of_typ t1) (string_of_typ t2) in
  match bop with
      Plus _ | Times _ | Minus _ | Shl _ | Shr _ | Sar _ -> 
        if (t1 = TInt & t2 = TInt) then TInt else failwith error_msg
    | Eq _ | Neq _ ->
        begin match join c.sigs t1 t2 with
	  | Some _ -> TBool 
	  | None -> failwith error_msg
        end
    | Lt _ | Lte _ | Gt _ | Gte _ -> 
        if (t1 = TInt & t2 = TInt) then TBool else failwith error_msg
    | IAnd _ | IOr _ -> 
        if (t1 = TInt & t2 = TInt) then TInt else failwith error_msg
    | And _ | Or _ -> 
        if (t1 = TBool & t2 = TBool) then TBool else failwith error_msg

let rec typecheck_const (cn:Range.t const) : typ=
  match cn with
    | Cnull _ -> TBot
    | Cbool _ -> TBool
    | Cint _ -> TInt
    | Cstring _ -> TRef RString

let typecheck c e (expected:typ) (t:typ) : unit =
  if (not (subtyping c.sigs t expected)) then 
    failwith (report_typ_error (exp_info e) expected t)

let rec typecheck_path (c:ctxt) (p:Range.t path) : ptyp =
  match p with
    | ThisId (info,id) ->
        begin match c.this with
	  | None -> failwith (sprintf "%s: %s is not in class."
                      (Range.string_of_range info) id)
	  | Some cid ->
              match (hasField c.sigs cid id, hasMethod c.sigs cid id) with
		| (Some t, None) -> Ptyp t
		| (None, Some t) -> Pftyp t
	        | (None, None) -> 
                    failwith (sprintf "%s: %s is not a field or method of this."
                      (Range.string_of_range info) id)
		| _ -> failwith 
                    (sprintf "%s: %s is used as both field and method of this."
                      (Range.string_of_range info) id)
        end
    | PathId (lc,(info,id)) ->
	let t = typecheck_lhs_or_call c lc in
        match t with
	  | TRef (RClass cid) ->
              begin match (hasField c.sigs cid id, hasMethod c.sigs cid id) with
		| (Some t, None) -> Ptyp t
		| (None, Some t) -> Pftyp t
	        | (None, None) -> 
                    failwith (sprintf "%s: %s is not a field or method of %s."
                      (Range.string_of_range info) id cid)
		| _ -> failwith 
                    (sprintf "%s: %s is used as both field and method of this."
                      (Range.string_of_range info) id)
              end
	  | _ -> failwith (sprintf "%s: This exp is not a class."
                      (Range.string_of_range (lhs_or_call_info lc)))

and typecheck_call (c:ctxt) (cl:Range.t call) : rtyp =
  match cl with
    | Func ((info,fid), es) ->
        begin match (lookup_fdecl fid c) with
	  | None -> 
             if (fid = "length_of_array") then 
               Some (check_length_of_array c info es)
             else failwith (sprintf "%s: %s is not declared." 
               (Range.string_of_range info) fid)
          | Some (ts, rt) -> typecheck_exps c info es ts; rt
	end
    | SuperMethod ((info,id), es) ->
        begin match c.this with
	  | None -> failwith (sprintf "%s: %s is not in class."
                      (Range.string_of_range info) id)
	  | Some cid ->
              match lookup_sig cid c with
		| None -> failwith (sprintf "%s: %s is not defined."
                      (Range.string_of_range info) cid)
		| Some (None,_,_,_) ->
                    failwith (sprintf "%s: %s has no parent class."
                      (Range.string_of_range info) id)
		| Some (Some pid,_,_,_) ->
                    match hasMethod c.sigs pid id with
   	              | None -> failwith
                          (sprintf "%s: %s is not a method of super."
                            (Range.string_of_range info) id)
		      | Some (ts, rt) -> typecheck_exps c info es ts; rt
        end
    | PathMethod (p, es) ->
        let info = path_info p in
        match typecheck_path c p with
	  | Pftyp (ts, rt) -> typecheck_exps c info es ts; rt
	  | Ptyp t -> failwith 
              (sprintf "%s: This expression was expected of fucntion type." 
                (Range.string_of_range info))

and typecheck_lhs_or_call (c:ctxt) (lc: Range.t lhs_or_call) : typ =
  match lc with
    | Lhs l -> typecheck_lhs c l
    | Call cl -> 
        begin match (typecheck_call c cl) with
          | Some t -> t
          | _ -> failwith (sprintf "%s: This exp cannot be of type unit." 
             (Range.string_of_range (lhs_or_call_info lc)))
        end

and typecheck_lhs (c:ctxt) (l:Range.t lhs) : typ =
  match l with
    | Var (info,id) ->
        begin match (lookup_vdecl id c) with
          | None -> failwith (sprintf "%s: %s is not declared." 
              (Range.string_of_range info) id)
          | Some t -> t
        end
    | Path p ->
        begin match (typecheck_path c p) with
          | Ptyp t -> t
          | _ -> failwith (sprintf "%s: Lhs cannot be of function types." 
              (Range.string_of_range (path_info p)))
        end
    | Index (lc, e) ->
        begin match (typecheck_exp c e) with
	  | TInt ->
              begin match (typecheck_lhs_or_call c lc) with
                | TRef (RArray t) -> t
                | _ -> failwith ((Range.string_of_range (exp_info e))^
                    ": This expression is not of array type.")
              end
	  | _ as t -> failwith (report_typ_error (exp_info e) TInt t)
        end

and typecheck_exp (c:ctxt) (e:Range.t exp) : typ =
  match e with
    | Const cn -> typecheck_const cn
    | This i ->
        begin match c.this with
	  | None -> failwith (sprintf "%s: This is not in class." 
              (Range.string_of_range i))
	  | Some cid -> 
              match (lookup_sig cid c) with
                | None -> failwith (sprintf 
                   "%s: Internal error-This class %s is not declared." 
                   (Range.string_of_range i) cid)
                 | Some _ -> TRef (RClass cid) 
        end
    | New (e1, (_,id), e2) ->
        let t1 = typecheck_exp c e1 in
        let c = enter_scope c in
        let c = add_vdecl id t1 c in
        TRef (RArray (typecheck_exp c e2)) 
    | Ctor ((info, cid), es) ->
        begin match (lookup_sig cid c) with
          | None -> failwith 
              (sprintf "%s: Class %s is not declared." 
                (Range.string_of_range info) cid)
          | Some (_,_,ts,_) -> typecheck_exps c info es ts; 
              TRef (RClass cid) 
        end
    | LhsOrCall lc -> typecheck_lhs_or_call c lc

    | Binop (bop, e1, e2) -> 
        let t1 = typecheck_exp c e1 in
        let t2 = typecheck_exp c e2 in
        typecheck_bop c bop t1 t2
    | Unop (uop, e) ->
        let t = typecheck_exp c e in
        typecheck_unop uop t

and check_length_of_array c info es : typ =
  match es with
    | [] -> failwith (sprintf "%s: Call has wrong number of args."      
        (Range.string_of_range info))
    | e::[] ->
        begin match typecheck_exp c e with
	  | TRef (RArray _) -> TInt
	  | _ -> failwith (sprintf "%s: This exp should be of array type." 
            (Range.string_of_range (exp_info e)))
        end
    | _ -> failwith (sprintf "%s: Call has wrong number of args." 
      (Range.string_of_range info))

and typecheck_exps c info es ts : unit =
  try
    List.iter2 (fun t -> fun e -> 
      let t' = typecheck_exp c e in 
      typecheck c e t t') ts es
  with 
    | Invalid_argument _ -> 
        failwith (sprintf "%s: Call has wrong number of args." 
          (Range.string_of_range info))

let rec typecheck_init (c:ctxt) (i:Range.t init) : typ =
  match i with
    | Iexp e -> typecheck_exp c e
    | Iarray (info, i::is') ->
	let t = typecheck_init c i in
        let t = List.fold_left
          (fun t -> fun i ->
             let ti = typecheck_init c i in
             match join c.sigs t ti with
	       | None -> failwith (sprintf "%s: Types %s and %s cannot join."
                   (Range.string_of_range info) (string_of_typ t) 
                   (string_of_typ ti))
	       | Some t' -> t'
          ) t is' in
        TRef (RArray t)
    | Iarray (r, _) -> failwith (sprintf "%s: Initializer cannot be of length 0."
       (Range.string_of_range r))

let typecheck_var_decls (c:ctxt) (vdecls:(Range.t vdecl) list) : ctxt =
  let c = enter_scope c in
  List.fold_left 
   (fun c -> fun ({v_ty=t; v_id=(info,id); v_init=i;}) ->
     let ti = typecheck_init c i in
     (if (not (wellformed_typ c.sigs t)) then
       failwith (report_wftyp_error info t));
     if (not (subtyping c.sigs ti t)) then 
       failwith (report_typ_error (init_info i) t ti);
     add_vdecl id t c
   ) c vdecls

let rec typecheck_stmt (c:ctxt) (s:Range.t stmt) : unit =
  match s with
    Assign (l, e) -> 
      let tl = typecheck_lhs c l in
      let te = typecheck_exp c e in
      typecheck c e tl te
  | Scall cl ->
      begin match (typecheck_call c cl) with
        | None -> () 
        | _ -> failwith (sprintf "%s: Stmt must be of type unit." 
           (Range.string_of_range (call_info cl)))
      end
  | Fail e ->
      begin match typecheck_exp c e with
	| TRef RString -> ()
	| _ ->  failwith (sprintf "%s: Fail must be of type string." 
              (Range.string_of_range (exp_info e)))
      end
  | If (e, st1, sto2) ->
      typecheck c e TBool (typecheck_exp c e);
      typecheck_stmt c st1;
      typecheck_option_stmt c sto2
  | IfNull (r, (info,id), e, st1, sto2) ->
      let t = typecheck_exp c e in
      if not (wellformed_ref c.sigs r) then
        failwith (report_wftyp_error info (TRef r));
      begin match t with
	| TNullable r ->
            typecheck_option_stmt c sto2;
            let c = enter_scope c in
            let c = add_vdecl id (TRef r) c in
            typecheck_stmt c st1           
	| _ -> failwith (sprintf "%s: Ifnull must check nullable type." 
              (Range.string_of_range (exp_info e)))
      end
  | Cast (cid0, (info, id), e, st1, sto2) ->
    let t = typecheck_exp c e in
    begin match t with
      | TRef (RClass cid) ->
        if subclass c.sigs cid0 cid then
          let c = enter_scope c in
          let c = add_vdecl id (TRef (RClass cid)) c in
          typecheck_stmt c st1 else failwith "not a subclass TR"
      | TNullable (RClass cid) -> 
        if subclass c.sigs cid0 cid then
          let c = enter_scope c in
          let c = add_vdecl id (TRef (RClass cid)) c in
          typecheck_stmt c st1 else failwith "not a subclass TN"
      | _ -> failwith "Something is wrong..."
    end
  | While (e, st) ->
      typecheck c e TBool (typecheck_exp c e);
      typecheck_stmt c st;
  | For (vdecls, eo, sto1, st2) ->
      let c = typecheck_var_decls c vdecls in
      (match eo with
	 | Some e -> typecheck c e TBool (typecheck_exp c e)
	 | None -> ());
      typecheck_option_stmt c sto1;
      typecheck_stmt c st2
  | Block (vdecls, sts) ->
      let c = typecheck_var_decls c vdecls in 
      List.iter (typecheck_stmt c) sts

and typecheck_option_stmt (c:ctxt) (so:(Range.t stmt) option) : unit =
  match so with
    | Some s -> typecheck_stmt c s
    | _ -> ()

let typecheck_args (c:ctxt) args : ctxt =
  let c = enter_scope c in
  List.fold_left (fun c -> fun (t, (i,id)) -> 
    if wellformed_typ c.sigs t then add_vdecl id t c
    else failwith (report_wftyp_error i t)) c args

let typecheck_fdecl (c:ctxt) 
  ((rt, (info,fid), args, (vdecls, sts), eo): Range.t fdecl) : unit =
  let c = typecheck_args c args in
  let c = typecheck_var_decls c vdecls in 
  List.iter (typecheck_stmt c) sts;
  match (eo, rt) with
    | (Some e, Some t) -> 
        if wellformed_typ c.sigs t then
          typecheck c e t (typecheck_exp c e)
        else failwith (report_wftyp_error info t)
    | (None, None) -> ()
    | (Some _, None) -> 
        failwith ((Range.string_of_range info)^": Expected to return unit.")
    | (None, Some _) -> 
        failwith ((Range.string_of_range info)^": Expected to return non-unit.")

let subftyping (sigs:signature) (ts1, topt1) (ts2, topt2) : bool =
  try 
    List.iter2 (fun t1 -> fun t2 -> 
      if not (subtyping sigs t1 t2) 
      then failwith "subftyping: unmatched") ts2 ts1;
    match (topt1, topt2) with
      | (None, None) -> true
      | (Some t1, Some t2) -> subtyping sigs t1 t2
      | _ -> false
  with
    | Failure _ -> false 
    | Invalid_argument _ -> false

let check_override (sigs:signature) (extopt:cid option) (i,id) (f:ftyp) : unit =
  match extopt with
    | None -> ()
    | Some cid -> 
        match hasMethod sigs cid id with
	  | None -> ()
	  | Some f' -> if not (subftyping sigs f f') then
              failwith (report_ftyp_error i f' f)

let add_method (i,id) ft cid c =
  match c.sigs with
    | [] -> failwith "Internal error: add_method in no class scope."
    | (cid', (extopt, fs, ts, ms))::tl ->
        if cid <> cid' then
          failwith "Internal error: add_method in a different class scope."
        else
          try 
            ignore (List.assoc id ms);
            failwith ("redeclaration of a method: " ^ id)
          with
	    | Not_found ->
        	{c with sigs = (cid, (extopt, fs, ts, (id,ft)::ms))::tl}

let add_methods fdls cid c : ctxt =
  List.fold_left (fun c -> fun (rt, fid, args, _, _) ->
    let (ts, _) = List.split args in add_method fid (ts, rt) cid c
    ) c fdls

let check_shadow (sigs:signature) (extopt:cid option) (i,id) : unit =
  match extopt with
    | None -> ()
    | Some cid -> 
        match hasField sigs cid id with
	  | None -> ()
	  | Some f' -> failwith (sprintf "%s: Shadow parent class's variables."
              (Range.string_of_range i))

let add_field (i,id) t cid c =
  match c.sigs with
    | [] -> failwith "Internal error: add_field in no class scope."
    | (cid', (extopt, fs, ts, ms))::tl ->
        if cid <> cid' then
          failwith "Internal error: add_field in a different class scope."
        else
          try 
            ignore (List.assoc id fs);
            failwith ("redeclaration of a field: " ^ id)
          with
	    | Not_found ->
          try 
            ignore (List.assoc id ms);
            failwith ("a method and a field has the same name: " ^ id)
          with
	    | Not_found ->
                check_shadow c.sigs extopt (i,id);
        	{c with sigs = (cid, (extopt, (id,t)::fs, ts, ms))::tl}

let add_fdecls_and_cdecls (c:ctxt) (p:Range.t prog) : ctxt =
  List.fold_left
   (fun c -> fun g ->
      match g with
  	| Gfdecl (rt, (_,fid), args, _, _) ->
            let (ts, _) = List.split args in
            add_fdecl fid (ts, rt) c
  	| Gefdecl (rt, (_,fid), args) ->
            let (ts, _) = List.split args in
            add_fdecl fid (ts, rt) c
	| Gcdecl (cid, extopt, fds, (args, _, _, _), fdls) ->
            let (ts, _) = List.split args in 
            let c = add_sig cid extopt ts c in
            let c = add_methods fdls cid c in
            List.fold_left (fun c -> fun (t, (info,id)) ->
              add_field (info,id) t cid c) c fds            
  	| _ -> c
   ) c p

let typecheck_fields c (fds: Range.t fields) cid : unit =
  List.iter (fun (t, (info,id)) ->
    (if (not (wellformed_typ c.sigs t)) then 
      failwith (report_wftyp_error info t))) fds

let typecheck_methods c (fdls: Range.t fdecls) cid : unit =
  match lookup_sig cid c with
    | Some (extopt, _, _, _) ->
      List.iter (fun ((rt, fid, args, _, _) as fdl) ->
        let (ts, _) = List.split args in
        check_override c.sigs extopt fid (ts,rt);
        typecheck_fdecl {c with this=Some cid} fdl) fdls
    | None -> failwith ("class is not defined: " ^ cid)

let typecheck_ctor c (args, es, is, (vdecls, sts)) cid extopt : unit =
  let c = typecheck_args c args in
  (match extopt with
    | None -> if List.length es <> 0 then
        failwith "basic class should not call super ctor."
    | Some pid ->
        try 
          let (_, _, ts, _) = List.assoc pid c.sigs in 
          typecheck_exps c Range.norange es ts
        with
          | Not_found -> failwith ("parent's class is not defined: " ^ pid)
  );
  List.iter (fun ((_,id), i) ->  
    match (hasField c.sigs cid id) with
      | None -> failwith ("Undefined field: " ^ id)
      | Some t ->
        let ti = typecheck_init c i in
        if (not (subtyping c.sigs ti t)) then 
          failwith (report_typ_error (init_info i) t ti)
    ) is;
  let c = typecheck_var_decls {c with this=Some cid} vdecls in 
  List.iter (typecheck_stmt {c with this=Some cid}) sts

let typecheck_cdecl c (cid, extopt, fds, (args, es, cis, b), fdls) : unit =
  typecheck_fields c fds cid;
  typecheck_methods c fdls cid;
  typecheck_ctor c (args, es, cis, b) cid extopt

let typecheck_prog (p:Range.t prog) : unit =
  let c = enter_scope empty_ctxt in
  let c = add_fdecls_and_cdecls c p in
  let _ = 
    List.fold_left 
      (fun c -> fun g ->
         match g with
	   | Gvdecl {v_ty=t; v_id=(info,id); v_init=i;} -> 
               let ti = typecheck_init {empty_ctxt with sigs=c.sigs} i in
               (if (not (wellformed_typ c.sigs t)) then
                 failwith (report_wftyp_error info t));
               (if (not (subtyping c.sigs ti t)) then 
                 failwith (report_typ_error (init_info i) t ti));
               add_vdecl id t c
           | Gefdecl (rt, (info,_), args) ->
               let _ = typecheck_args c args in
               (match rt with
                 | Some t -> 
                     if wellformed_typ c.sigs t then c
                     else failwith (report_wftyp_error info t)
                 | None -> c)
	   | Gfdecl f -> typecheck_fdecl c f; c
           | Gcdecl cd -> typecheck_cdecl c cd; c
      ) c p
  in ()
