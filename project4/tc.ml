open Ast
open Astlib
open Printf
open Tctxt

let report_error (info:Range.t) (expected:typ) (t:typ) : string =
  sprintf 
    "%s: This expression has type %s but an expression was expected of type %s." 
    (Range.string_of_range info) (string_of_typ expected) (string_of_typ t)

let rec tc_typ (e1:Range.t exp) (e2:Range.t exp) (c:ctxt) : typ =
  if (tc_exp e1 c <> tc_exp e2 c) then
    failwith (report_error (exp_info e2) (tc_exp e1 c) (tc_exp e2 c))
      else TBool

and tc_bool (e1:Range.t exp) (e2:Range.t exp) (c:ctxt) : typ = 
  if (tc_exp e1 c <> TBool) then
    failwith (report_error (exp_info e1) TBool (tc_exp e1 c))
      else if (tc_exp e2 c <> TBool) then
        failwith (report_error (exp_info e2) TBool (tc_exp e2 c))
          else TBool

and tc_int (e1:Range.t exp) (e2:Range.t exp) (c:ctxt) : typ = 
  if (tc_exp e1 c <> TInt) then
    failwith (report_error (exp_info e1) TInt (tc_exp e1 c))
      else if (tc_exp e2 c <> TInt) then
        failwith (report_error (exp_info e2) TInt (tc_exp e2 c))
          else TInt
          
and tc_binop (b:Range.t binop) (e1:Range.t exp) (e2:Range.t exp) (c:ctxt) : typ =
  begin match b with
    | Eq (_) -> tc_typ e1 e2 c
    | Neq (_) -> tc_typ e1 e2 c
    | And (_) -> tc_bool e1 e2 c
    | Or (_) -> tc_bool e1 e2 c
    | Plus (_) -> tc_int e1 e2 c
    | Times (_) -> tc_int e1 e2 c
    | Minus (_) -> tc_int e1 e2 c
    | Lt (_) -> ignore(tc_int e1 e2 c); TBool
    | Lte (_) -> ignore(tc_int e1 e2 c); TBool
    | Gt (_) -> ignore(tc_int e1 e2 c); TBool
    | Gte (_) -> ignore(tc_int e1 e2 c); TBool
    | IAnd (_) -> tc_int e1 e2 c
    | IOr (_) -> tc_int e1 e2 c
    | Shl (_) -> tc_int e1 e2 c
    | Shr (_) -> tc_int e1 e2 c
    | Sar (_) -> tc_int e1 e2 c
  end

and tc_const (con: Range.t const) (c:ctxt) : typ =
  begin match con with
    | Cbool _ -> TBool
    | Cint _ -> TInt
    | Cstring _ -> TString
  end

and tc_unop (u: Range.t unop) (e1:Range.t exp) (c:ctxt) : typ =
  begin match u with
    | Neg (_) -> if (tc_exp e1 c <> TInt) then
      failwith (report_error (exp_info e1) TInt (tc_exp e1 c)) else TInt
    | Not (_) -> if (tc_exp e1 c <> TInt) then
      failwith (report_error (exp_info e1) TInt (tc_exp e1 c)) else TInt
    | Lognot (_) -> if (tc_exp e1 c <> TBool) then
      failwith (report_error (exp_info e1) TBool (tc_exp e1 c)) else TBool
  end

and tc_lhs (l:Range.t lhs) (c:ctxt) : typ =
  begin match l with
    | Var (_,s) -> let typo = lookup_vdecl s c in
      begin match typo with
        | Some t -> t
        | None -> failwith "LHS: Not matched"
      end
    | Index (lh,e) -> if (tc_exp e c <> TInt) then
      failwith (report_error (exp_info e) TInt (tc_exp e c)) else
        let l = tc_lhs lh c in
        begin match l with
          | TArray(a) -> a
          | _ -> failwith "wrong. no array"
        end
  end
  
and tc_new (e1:Range.t exp) (id: Range.t id) (e2:Range.t exp) (c:ctxt) : typ =
  if (tc_exp e1 c <> TInt) then
    failwith (report_error (exp_info e1) (TInt) (tc_exp e1 c)) else
      begin match id with
        | (_,s) -> let c = add_vdecl s (TInt) (enter_scope c)
          in TArray (tc_exp e2 c)
      end

and tc_length (e:Range.t exp list) (c:ctxt) : typ =
  begin match e with
    | h::[] ->
      begin match tc_exp h c with
        | TArray t -> TInt
        | _ -> failwith "not a TArray"
      end
    | _ -> failwith "shouldnt be longer than 1"
   end

and tc_ecall (s:string) (el:Range.t Ast.exp list) (c:ctxt) : typ = 
  if (s = "length_of_array") then tc_length el c else 
  let f = lookup_fdecl s c in
  begin match f with
    | Some (l,r) ->
      if r = None then failwith "can't call this as expression" else
        List.iter2 (fun a -> fun b -> if a = tc_exp b c then ()
        else failwith "exception : ECall") l el;
        begin match r with
          | Some t -> t
          | None -> failwith "no return type"
        end
    | None -> failwith "not declared yet : ecall"
  end
  
and tc_exp (e:Range.t exp) (c:ctxt) : typ =
 begin match e with
    | Binop (x,y,z) -> tc_binop x y z c
    | Const (x) -> tc_const x c
    | Lhs (x) -> tc_lhs x c
    | New (x,y,z) -> tc_new x y z c
    | Unop (x,y) -> tc_unop x y c
    | Ecall ((_,id),y) -> tc_ecall id y c
  end

and make_type_list (e:Range.t exp list) (c:ctxt) : typ list =
  let r = [] in
  begin match e with
    | h::tl -> r@[tc_exp h c]@(make_type_list tl c)
    | [] -> r
  end

and cmp_typs (e:Range.t exp list) (t: typ list) (c:ctxt) : unit =
  if List.length e <> List.length t then failwith "args dont match up" else
  begin match t with
    | h::tl ->
    begin match e with
      | h2::tl2 -> if tc_exp h2 c <> h then failwith "types dont match"
        else cmp_typs tl2 tl c
      | [] -> ()
    end
    | [] -> ()
  end

and tc_stmt (s: Range.t stmt) (c:ctxt) : unit  =
  begin match s with
    | Assign (l,e) -> if (tc_lhs l c) <> (tc_exp e c) then
       failwith "types dont match" else ()
    | Scall ((_,s),e) -> let f = lookup_fdecl s c in
      begin match f with
        | Some (tl,rt) -> cmp_typs e tl c; if rt <> None then
          failwith "can't call as stmt"
        | None -> failwith "couldn't find function"; ()
      end
    | If (e,st,Some o) -> if tc_exp e c <> TBool then
      failwith (report_error (exp_info e) TBool (tc_exp e c)) else
      tc_stmt st c; tc_stmt o c
    | If (e,st, None) -> if tc_exp e c <> TBool then
      failwith (report_error (exp_info e) TBool (tc_exp e c)) else
        tc_stmt st c
    | While (e,s) -> if (tc_exp e c <> TBool) then failwith "not bool" else
      tc_stmt s c;
    | For (v,Some oe,Some os,st) -> let c = enter_scope c in
      let c = vdecl_h v c in tc_stmt st c; if tc_exp oe c <> TBool
        then failwith "not a bool" else tc_stmt os c; ()
    | For (v,None,Some os,st) -> let c = enter_scope c in let c = vdecl_h v c in
      tc_stmt st c; tc_stmt os c
    | For (v,None,None,st) -> let c = enter_scope c in let c = vdecl_h v c in
      tc_stmt st c
    | For (v, Some oe, None, st) ->let c = enter_scope c in
      let c = vdecl_h v c in tc_stmt st c; if tc_exp oe c <> TBool
        then failwith "not a bool" else ()
    | Block b -> let c = enter_scope c in tc_block b c true; ()
  end
  
and vdecl_h (v:Range.t Ast.vdecls) (c:ctxt) : ctxt =
  begin match v with
    | h::tl -> tc_vdecl h c;
      begin match h with
        | { v_ty = x; v_id = (_, s); v_init = z;} ->
           let c = add_vdecl s x c in vdecl_h tl c
      end
    | [] -> c
  end

and stmt_h (s:Range.t stmt list) (c:ctxt) : unit =
  begin match s with
    | h::tl -> tc_stmt h c; stmt_h tl c
    | [] -> ()
  end

and tc_block (b:Range.t block) (c:ctxt) (flag: bool) : ctxt =
  begin match b with
    | (x,y) -> let c = vdecl_h x c in stmt_h y c; c
  end
  
and tc_fdecl (f:Range.t fdecl) (c:ctxt) : unit  =
  begin match f with
    | (rtyp, (_,s), args, block, exp) ->
      let c = enter_scope c in let c =
        List.fold_left (fun c -> (fun (t,(_,s)) -> add_vdecl s t c)) c args in
      let c = tc_block block c true in
      begin match exp with
        | Some e ->
          begin match rtyp with 
            | Some t -> if ((tc_exp e c) <> t) then
              failwith (report_error (exp_info e) (t) (tc_exp e c)) else ()
            | None -> failwith "no return type"
            end
        | None -> if (rtyp = None) then ()
          else failwith "return type is not None"
      end
  end

and tc_init (i:Range.t init) (c:ctxt) : typ =
   begin match i with
        | Iexp e -> tc_exp e c
        | Iarray (_,l) ->
          begin match l with
            | h::tl -> let typ1 = tc_init h c in
              List.iter (fun init -> if (tc_init init c = typ1) then ()
                else failwith "wrong types") tl; TArray typ1
            | [] -> failwith "must have length greater than 1"
          end
      end

and tc_vdecl (v:Range.t vdecl) (c:ctxt) : unit =
  begin match v with
    | {v_ty = ty; v_id = (_,s); v_init = i;} ->
       if (tc_init i c <> ty) then
          failwith (report_error (init_info i) (ty) (tc_init i c)) else ()
  end

let get_decls (p: Range.t prog) (c:ctxt) : ctxt =
  let rec get_decl_h (c: ctxt) (h: Range.t Ast.gdecl) : ctxt =
    begin match h with
      | Gvdecl {v_ty = x; v_id = (_, s); v_init = z;} ->
         if tc_init z (enter_scope empty_ctxt) <> x then failwith ""
          else add_vdecl s x c
      | Gfdecl _ -> c
    end in List.fold_left get_decl_h c p

let typecheck_prog (p:Range.t prog) : unit =
  let c = enter_scope empty_ctxt in let c = add_fdecls c p in
    let c = get_decls p c in
    let rec typecheck_h (h:Range.t Ast.gdecl) : unit =
      begin match h with
		     | Gvdecl v  -> ()
	       | Gfdecl f -> tc_fdecl f c
		   end in
      List.iter typecheck_h p